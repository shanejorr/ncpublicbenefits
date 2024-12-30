#############################################################################
#
# create table of TANF (work first) benefits and income thresholds
# Source: https://www.ncdhhs.gov/documents/files/dss/draft-tanf-state-plan-2016-2019/download
#
#############################################################################

#' @title Calculate TANF (Work First) Benefits
#' @description Computes the Temporary Assistance for Needy Families (TANF) benefits, also known as Work First in North Carolina. 
#' The calculation uses household income, size, and need standards to determine eligibility and payment amounts based on state guidelines.
#' @param base_table A tibble containing the base household data, including family composition, income, and household size.
#'    The table should be generated using the `base_composition` function.
#' @return A tibble with calculated TANF benefits, including payment amounts, family composition, and income thresholds.
#' @export
tanf <- function(base_table) {

  message("Calculating TANF benefits")

  # monthly payment is 50% difference between total countable income and need standard (pg. 34)

  # table below is need standard
  tanf_need_std <- data.frame(
    household_size = c(1, 2, 3, 4, 5),
    need_std = c(362, 472, 544, 594, 648)
  )

  # merge need standard to tanf dataset
  tanf <- base_table |>
    dplyr::left_join(tanf_need_std, by=c('size'='household_size'), relationship = 'many-to-one')

  # all values should match
  if (any(is.na(tanf$need_std))) stop("TANF need standards are not all defined", call. = FALSE)

  tanf <- tanf |>
    dplyr::mutate(
      # can deduct 27.5% of gross earned income
      tanf_monthly_income = monthly_income - (monthly_income * .275),
      # calculate payment as 50% difference between income and need std
      payment = round((need_std - tanf_monthly_income)*.5, 0),
      # payment must be $25 or more to receive benefits
      payment = ifelse(payment >= 25, payment, 0),
      # cannot receive benefits if you don't have children
      payment = ifelse(children == 0, 0, payment)
    )

  # create final data set
  tanf <- tanf |>
    dplyr::arrange(monthly_income, adults, children) |>
    dplyr::select(composition, adults, children, monthly_income, payment) |>
    dplyr::mutate(benefit = 'Work First (TANF)')

  return(tanf)

}
