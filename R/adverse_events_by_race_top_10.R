#' Adverse Events by Race (Top 10)
#'
#' This function generates a bar plot showing the distribution of top 10 adverse events
#' by race.
#'
#' @param data A data frame containing adverse events and race information.
#'
#' @return A bar plot visualizing the distribution of top 10 adverse events by race.
#'
#' @examples
#' adverse_events_by_race_top_10(my_data)
#'
#' @export
adverse_events_by_race_top_10 <- function(data) {
  # Identify the top 10 adverse events
  top_10_ae <- data %>%
    count(AEPT, sort = TRUE) %>%
    top_n(10)

  # Filter the dataset to include only the top 10 adverse events
  filtered_data <- data %>%
    filter(AEPT %in% top_10_ae$AEPT)

  # Plot for gender and top 10 adverse events with frequencies
  ggplot(filtered_data, aes(x = RACE, fill = RACE)) +
    geom_bar(position = "dodge") +
    labs(title = "Adverse Events Distribution by Race for Top 10 Adverse Events",
         y = "Frequency",
         x = "Adverse Event") +
    scale_fill_discrete(name = "Race") +
    facet_wrap(~AEPT, scales = "free_y", ncol = 3) +
    theme(axis.text.x = element_text(hjust = 1, angle = 45),
          strip.text = element_text(size = 10, hjust = 0.5),
          plot.title = element_text(hjust = 0.5))
}
