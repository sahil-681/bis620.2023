#' Adverse Events by Gender (Top 10)
#'
#' This function generates a bar plot showing the distribution of top 10 adverse events
#' by gender.
#'
#' @param data A data frame containing adverse events and gender information.
#'
#' @return A bar plot visualizing the distribution of top 10 adverse events by gender.
#'
#' @examples
#' adverse_events_by_gender_top_10(my_data)
#'
#' @export
adverse_events_by_gender_top_10 <- function(data) {
  # Identify the top 10 adverse events
  top_10_ae <- data %>%
    count(AEPT, sort = TRUE) %>%
    top_n(10)

  # Filter the dataset to include only the top 10 adverse events
  filtered_data <- data %>%
    filter(AEPT %in% top_10_ae$AEPT)

  # Plot for gender and top 10 adverse events with frequencies
  ggplot(filtered_data, aes(x = SEX, fill = SEX)) +
    geom_bar(position = "dodge") +
    labs(title = "Adverse Events Distribution by Gender for Top 10 Adverse Events",
         y = "Frequency",
         x = "Adverse Event") +
    scale_fill_discrete(name = "Gender") +
    facet_wrap(~AEPT)
}
