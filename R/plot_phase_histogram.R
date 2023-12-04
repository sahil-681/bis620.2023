#' Create a histogram of the phases returned by a brief title keyword search
#'
#' @param x The database table.
#' @param brief_title_kw The brief title keywords to look for.
#' @return A ggplot object showing the histogram of phases.
#' @importFrom ggplot2 ggplot geom_col theme_bw xlab ylab scale_x_discrete
#' @importFrom dplyr select group_by summarize
#' @export
plot_phase_histogram <- function(x, brief_title_kw) {
  x = x |>
    select(phase) |>
    group_by(phase) |>
    summarize(n = n())

  ggplot(x, aes(x = phase, y = n)) +
    geom_col() +
    theme_bw() +
    xlab("Phase") +
    ylab("Count") +
    scale_x_discrete(breaks = all_phases$phase,
                     labels = all_phases$phase)
}
