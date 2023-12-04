#' Query keywords from a database table
#'
#' @description Data is queried based on a typed keyword input.
#' @param d The database table.
#' @param kwds The keywords to look for.
#' @param column The column to look for the keywords in.
#' @param ignore_case Should the case be ignored when searching for a keyword? (default TRUE)
#' @param match_all Should we look for values that match all of the keywords (intersection) or any of the keywords (union)? (default FALSE; union).
#' @return A filtered dataset based on keyword search.
#' @export
#' @importFrom dplyr filter
query_kwds <- function(d, kwds, column, ignore_case = TRUE, match_all = FALSE) {
  kwds = kwds[kwds != ""]
  kwds = paste0("%", kwds, "%") |>
    gsub("'", "''", x = _)
  if (ignore_case) {
    like <- " ilike "
  } else{
    like <- " like "
  }
  query = paste(
    paste0(column, like, "'", kwds, "'"),
    collapse = ifelse(match_all, " AND ", " OR ")
  )
  filter(d, sql(query))
}
