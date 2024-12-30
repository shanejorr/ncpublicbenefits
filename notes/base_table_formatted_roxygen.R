###############################################################################
#
# This script creates the base table that will be used for all benefits tables
# It creates a table of various family types and incomes from 0 to 7000 per month
#
###############################################################################

#' @title Calculate Base Table
#' @description Calculates base table specific to the state of North Carolina.
#' @param income_increment = 10 Description of the parameter `income_increment = 10`.
#' @return The calculated benefit value.
#' @export
#'
base_composition <- function(income_increment = 10) {

  # create a base data frame that is the household size, household composition, and income
  # columns for all benefit data frames
  incomes <- seq(0, 7000, by = income_increment)

  # create base composition, and then we will paste sizes on to this
  composition <- family_benefit_values('family')

  # dataframe for number of adults and children
  adults_children <- data.frame(adults = c(rep(1, 4), rep(2, 4)),
                                children = c(rep(c(0, 1, 2, 3), 2)))

  # sizes should match composition levels
  sizes <- c(seq(1, 4), seq(2, 5))

  # we now need a data frame that lists every income for all composition levels

  # create data frame that is just the composition and size, and we will add incomes later
  comp_size <- data.frame(composition = composition,
                          size = sizes) |>
    dplyr::bind_cols(adults_children)

  # iterate through each income value, adding that value to the comp_size data frame,
  # then add the data frame to the main data frame containing all incomes
  base <- purrr::map(incomes, function(x) dplyr::mutate(comp_size, monthly_income = x)) |>
    purrr::list_rbind() |>
    tibble::as_tibble()

  return(base)

}
