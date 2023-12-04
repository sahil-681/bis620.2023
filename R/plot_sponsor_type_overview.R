#' Create a sponsor type overview plot
#'
#' @param x The database table.
#' @param brief_title_kw The brief title keywords to look for.
#' @return A ggplot object showing the sponsor type overview.
#' @importFrom ggplot2 ggplot geom_bar theme_minimal xlab ylab theme element_text
#' @importFrom dplyr select filter
#' @export
plot_sponsor_type_overview <- function(x, brief_title_kw) {
  x = x |>
    select(agency_class) |>
    filter(!is.na(agency_class))

  ggplot(x, aes(x = agency_class)) +
    geom_bar() +
    theme_minimal() +
    xlab("Sponsor Type") +
    ylab("Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
