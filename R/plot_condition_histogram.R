#' Create a histogram of the conditions returned by a brief title keyword search
#'
#' @param x The database table.
#' @param brief_title_kw The brief title keywords to look for.
#' @return A ggplot object showing the histogram of conditions.
#' @importFrom ggplot2 ggplot geom_col theme_bw xlab ylab
#' @importFrom dplyr select group_by summarize collect
#' @export
plot_condition_histogram <- function(x, brief_title_kw) {
  z = x() |>
    select(study_type) |>
    group_by(study_type) |>
    summarize(n = n()) |>
    collect()

  # Set NA as string
  z$study_type[is.na(z$study_type)] = "NA"

  ggplot(z, aes(x = study_type, y = n)) +
    geom_col() +
    theme_bw() +
    xlab("Conditions") +
    ylab("Count")
}
