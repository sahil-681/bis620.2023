#' Chi-squared Analysis
#'
#' This function performs a chi-squared test of independence between two categorical variables.
#'
#' @param data A data frame containing the variables to analyze.
#' @param column1 The name of the first categorical variable.
#' @param column2 The name of the second categorical variable.
#'
#' @return The chi-squared test result and the contingency table.
#'
#' @examples
#' chisq_analysis(my_data, "Variable1", "Variable2")
#'
#' @export
chisq_analysis <- function(data, column1, column2) {
  # Ensure the columns are treated as factors
  data[[column1]] <- as.factor(data[[column1]])
  data[[column2]] <- as.factor(data[[column2]])

  # Chi-squared test of independence
  chi_result <- chisq.test(table(data[[column1]], data[[column2]]))

  # Print the result
  print(chi_result)

  # Create the contingency table
  contingency_table <- table(data[[column1]], data[[column2]])

  # Print the contingency table
  print(contingency_table)

  # Calculate proportions and percentages
  prop_table <- prop.table(contingency_table, margin = 1) # Proportions by row
  percent_table <- round(100 * prop_table, 2) # Convert to percentages

  # Print the table of proportions/percentages
  print(percent_table)
}
