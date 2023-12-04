#' Generate a plot of concurrent studies
#'
#' @param x The database table.
#' @return A ggplot object showing the concurrent studies.
#' @importFrom ggplot2 ggplot geom_line xlab ylab theme_bw
#' @importFrom dplyr select
#' @export
plot_concurrent_trials <- function(x) {
  x |>
    select(start_date, completion_date) |>
    get_concurrent_trials() |>
    ggplot(aes(x = date, y = count)) +
    geom_line() +
    xlab("Date") +
    ylab("Count") +
    theme_bw()
}
