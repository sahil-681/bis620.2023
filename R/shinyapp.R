#' Launch a Shiny Application for Clinical Trials Query
#'
#' This function creates and launches a Shiny application that allows users
#' to interactively query and visualize clinical trials data. It establishes
#' a connection to a DuckDB database, reads the data, and sets up a user
#' interface for data exploration.
#'
#' @param con A connection object to the DuckDB database.
#'
#' @importFrom shiny shinyApp fluidPage titlePanel sidebarLayout sidebarPanel
#'   mainPanel tabsetPanel tabPanel textInput selectInput plotOutput sliderInput
#'   dataTableOutput renderPlot renderDataTable reactive
#' @importFrom DBI dbConnect dbListTables dbGetQuery
#' @importFrom dplyr tbl full_join left_join filter mutate select rename collect %>%
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map_dbl
#' @importFrom duckdb duckdb
#' @export
#'
#' @examples
#' con <- dbConnect(duckdb(file.path("ctgov.duckdb"), read_only = TRUE))
#' launchApp(con)

launchApp <- function(con){

  if (length(dbListTables(con)) == 0) {
    stop("Problem reading from connection.")
  }

  # Get Min/Max date
  min_date <- dbGetQuery(con, "SELECT MIN(study_first_submitted_date) FROM studies")[[1]]
  max_date <- dbGetQuery(con, "SELECT MAX(study_first_submitted_date) FROM studies")[[1]]

  # Extract tables
  studies <- tbl(con, "studies")
  sponsors <- tbl(con, "sponsors")
  conditions <- tbl(con, "conditions")
  outcomes <- tbl(con, "outcomes")
  countries <- tbl(con, "countries") %>%
    rename(name.country = name)

  # Join fully by `nct_id` for condition and sponsor data base
  d <- full_join(conditions, sponsors, by = c('nct_id'),
                 suffix = c('.cond', '.spons'))
  # Merge all tables by studies
  d <- left_join(studies, d, by = 'nct_id')

  # Merge all tables by outcome types
  d <- left_join(outcomes, d, by = 'nct_id')

  # Merge all tables by countries
  d <- left_join(countries, d, by = 'nct_id')

  # All phases as a dataframe
  all_phases <- d |>
    select(phase) |>
    distinct() |>
    collect() |>
    mutate(phase = ifelse(is.na(phase), "NA", phase))

  # All conditions as a dataframe
  all_cond <- d |>
    select(study_type) |>
    distinct() |>
    collect() |>
    mutate(study_type = ifelse(is.na(study_type), "NA", study_type))

  # All sponsors as a dataframe
  all_sponsors <- d |>
    select(agency_class) |>
    distinct() |>
    collect() |>
    mutate(agency_class = replace(agency_class, is.na(agency_class), 'NA')) |>
    arrange(agency_class)

  # All outcomes type as a dataframe
  all_outcomes <- d |>
    select(outcome_type) |>
    distinct() |>
    collect() |>
    mutate(outcome_type = ifelse(is.na(outcome_type), "NA", outcome_type))

  # All countries as a dataframe
  all_countries <- d |>
    select(name.country) |>
    distinct() |>
    collect() |>
    arrange(name.country) |>
    mutate(name.country = ifelse(is.na(name.country), "NA", name.country))


  # Define UI for application
  ui <- fluidPage(

    # Application title
    titlePanel("Clinical Trials Query"),

    # Sidebar with interactive buttons
    sidebarLayout(
      sidebarPanel(
        textInput("brief_title_kw", "Brief Title Keywords"),
        selectInput("sponsor_type",
                    "Select Sponsor",
                    c("All", all_sponsors),
                    "All"),
        # Dohyun's feature no. 1
        selectInput("outcome_type",
                    "Outcome Type",
                    c("All", all_outcomes),
                    "All"),
        # Dohyun's feature no. 2
        selectInput("country",
                    "Country",
                    c("All", all_countries),
                    "All")
      ),


      # Show plots
      mainPanel(
        tabsetPanel(
          type = "tabs",
          tabPanel("Phase", plotOutput("phase_plot")),
          tabPanel("Conditions", plotOutput("condition_plot")),
          tabPanel("Concurrent", plotOutput("concurrent_plot")),
          tabPanel("Sponsor Type Overview", plotOutput("sponsor_type_plot")),

          # Sahil's feature no. 2
          tabPanel("Dynamic Date Range", sliderInput("date_range", "Date Range",
                                                     min = as.Date(min_date),
                                                     max = as.Date(max_date),
                                                     value = c(as.Date(min_date), as.Date(max_date))))
        ),

        dataTableOutput("trial_table")
      )
    )
  )


  max_num_studies <- 1000

  # Define server logic required to draw a histogram
  server <- function(input, output) {

    get_studies = reactive({

      # Subset sponsors
      if (input$sponsor_type == "All") {
        data <- d
      } else if (input$sponsor_type == "NA"){
        data <- d %>% filter(!is.na(agency_class))
      } else {
        data <- query_kwds(d, input$sponsor_type, "agency_class")
      }

      # Subset outcome types
      # Dohyun's feature no. 1
      if(input$outcome_type == "All") {
        data <- d
      } else if (input$outcome_type == "NA") {
        data <- d %>% filter(!is.na(outcome_type))
      } else {
        data <- query_kwds(d, input$outcome_type, "outcome_type")
      }

      # Subset countries
      # Dohyun's feature no. 2
      if(input$country == "All") {
        data <- d
      } else if (input$country == "NA") {
        data <- d %>% filter(!is.na(country))
      } else {
        data <- query_kwds(d, input$country, "country")
      }

      data2 <- d

      if (input$brief_title_kw != "") {
        si = input$brief_title_kw |>
          strsplit(",") |>
          unlist() |>
          trimws()
        ret <- query_kwds(data2, si, "brief_title", match_all = TRUE)
      } else {
        ret <- data2
      }

      ret |>
        head(max_num_studies) |>
        collect()
    })

    output$phase_plot = renderPlot({
      get_studies() |>
        plot_phase_histogram()
    })

    output$condition_plot = renderPlot({
      get_studies |>
        plot_condition_histogram()
    })

    output$concurrent_plot = renderPlot({
      get_studies() |>
        plot_concurrent_trials()
    })

    output$trial_table = renderDataTable({
      get_studies() |>
        select(nct_id, brief_title, start_date, completion_date) |>
        rename(`NCT ID` = nct_id,
               `Brief Title` = brief_title,
               `Start Date` = start_date,
               `Completion Date` = completion_date)
    })

    # Sahil's feature no. 1
    output$sponsor_type_plot = renderPlot({
      get_studies() |>
        plot_sponsor_type_overview()
    })


    filtered_studies <- reactive({
      date_range <- input$date_range
      filter(get_studies(), start_date >= date_range[1] & start_date <= date_range[2])
    })

    output$trial_table <- renderDataTable({
      filtered_studies() |>
        select(nct_id, brief_title, start_date, completion_date) |>
        rename(`NCT ID` = nct_id,
               `Brief Title` = brief_title,
               `Start Date` = start_date,
               `Completion Date` = completion_date)
    })

  }

  # Run the application
  shinyApp(ui = ui, server = server)

}
