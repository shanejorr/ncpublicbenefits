###############################################################
#
# State and federal child care subsidies
#
##############################################################

#' @title Calculate Child Care Subsidy Benefits
#' @description Computes the state and federal child care subsidies based on family composition, income, and other factors. 
#' The calculation uses the 2019 North Carolina child care market rates for Forsyth County 4-star child care centers.
#' @param base_table A tibble containing the base table with family compositions, household sizes, and monthly incomes.
#'    The table should be generated using the `base_composition` function.
#' @return A tibble with the calculated child care subsidy payments, including details such as family composition, income, and benefits.
#' @export
child_care <- function(base_table) {

   message("Calculating child care benefits")

  current_year <- 2019

  # the market value of subsidies are based on the 2019 NC subsidized child care
  # market rates for Forsyth County 4-star child care centers
  # https://ncchildcare.ncdhhs.gov/Portals/0/documents/pdf/R/Revised-8-16-Market_Rate_Centers_Eff-10-1-18.pdf?ver=2018-08-28-105655-863

  # Forsyth County market rates for $855 for infant and $750 for 3-5
  # we will assume that families with 2 or more children have an infant and 3-5 year old
  # while families with one child only have a 3-5 year old
  # create named vector to map number of children to total market rate amounts
  market_rates <- tibble::tribble(
    ~children, ~payment,
    0, 0,
    1, 750,
    2, 1605,
    3, 1605
  )

  # create new column for market rates, which signifies benefit level
  care <- base_table |>
    dplyr::left_join(market_rates, by = "children", relationship = "many-to-many") |>
    dplyr::mutate(
      # recipients have to pay 10% of income
      # so subtract this amount from
      payment = round(.data$payment - (.data$monthly_income * .10), 2),
      # if payment is negative, convert to 0
      payment = dplyr::if_else(.data$payment < 0, 0, .data$payment)
    )

  # can receive benefits up to 200% fpg

  # get federal poverty guidelines
  # only want family sizes that we have in our base table

  family_sizes <- unique(care$size)

  fpg <- get_poverty_guidelines(current_year, 'us', family_sizes, by_month = TRUE) |>
    dplyr::select(dplyr::all_of(c('household_size', 'poverty_threshold')))

  fpg <- fpg |>
    # convert guideline amounts to 200% and calculate by month
    dplyr::mutate(income_limit = round(.data$poverty_threshold * 2, 0)) |>
    dplyr::rename(size = .data$household_size) |>
    dplyr::select(dplyr::all_of(c('size', 'income_limit')))

  # add 200% fpg to child care data set
  care <- care |>
    dplyr::left_join(fpg, by = "size") |>
    # set payment to 0 if income is greater than 200% of poverty guideline
    dplyr::mutate(
      payment = dplyr::if_else(.data$monthly_income > .data$income_limit, 0, .data$payment),
      benefit = "NC Child Care Subsidy"
    ) |>
  dplyr::select(dplyr::all_of(c("composition", "adults", "children", "monthly_income", "payment", "benefit")))
  
  return(care)

}
