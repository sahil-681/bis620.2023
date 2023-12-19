#' Demographic Analysis
#'
#' This function conducts a chi-square test for the association between demographic variables
#' (sex, race) and adverse events.
#'
#' @param data A data frame containing demographic and adverse events information.
#'
#' @return Chi-square test results for sex and adverse events, and race and adverse events.
#'
#' @examples
#' demographic_analysis(my_data)
#'
#' @export
demographic_analysis <- function(data) {
  # Convert variables to factors
  data$SEX <- as.factor(data$SEX)
  data$RACE <- as.factor(data$RACE)
  data$AEPT <- as.factor(data$AEPT)

  # Chi-square test for association between sex and adverse events
  print("Chi-square test for association between sex and adverse events")
  chi_sex_ae <- chisq.test(table(data$SEX, data$AEPT))
  print(chi_sex_ae)

  # Chi-square test for association between race and adverse events
  print("Chi-square test for association between race and adverse events")
  chi_race_ae <- chisq.test(table(data$RACE, data$AEPT))
  print(chi_race_ae)
}
