################################################################################
#
# NC Housing choice vouchers
#
###############################################################################

# create function to calculate total tenant rent, which is
# the amount of rent the tenant has to pay
# this is not the function that calculates total housing voucher
tenant_rent <- function(kids, income) {

  message("Calculating housing benefits")

  # total tenant payment (ttp) is the amount tenants have to pay in rent
  # it is the greater of 30% of their monthly adjusted income,
  # 10% of their monthly gross income, or $24
  # this function calcualtes the amount
  # source: https://www.hud.gov/sites/documents/43503C5HSGH.PDF

  # first, we'll calculate monthly adjusted income and multiply by .3
  # calculate adjsuted income by starting with income and subtracting the following deductions:
  #    dependent deduction of $480 (40 / month) for children 18 and under
  #    child care deduction for child care costs
  #         we'll assume $4000 (333.33 / month) per child, based on the example on pg. 43 of above link
  #         but, 10 year old in 3 person home will not have any chiold care costs

  # calculate adjusted income and multiply by .3
  adjusted_income <-(income-(40*kids)-(333.33*kids)) * .3

  # multiply gross income by .1
  gross_income <- income * .1

  # figure out which number is higher:
  # adjusted income or gross income and return that value
  ttp <- ifelse(adjusted_income  > gross_income, adjusted_income, gross_income)

  # if this amount is less than $25, make it $25
  ttp <- ifelse(ttp < 25, 25, ttp)

  # to calculate tentant rent, you subtract a utility allowance from ttp
  # $177 is the average Forsyth County utility allowance, so we will use it
  # https://www.huduser.gov/portal/datasets/assthsg.html#null
  # if ttp is smaller than the allowance, the tenant gets a utility reimbursement
  # due to this, we will keep negative numbers negative
  rent_payment <- ttp - 177

  return(rent_payment)

}

#' @title Calculate Housing Choice Voucher Benefits
#' @description Computes housing choice voucher benefits for tenants in North Carolina. 
#' The calculation uses 2019 fair market rent values, tenant payments, and utility allowances to determine the final benefit.
#' @param base_table A tibble containing the base household data, including family composition, income, and household size.
#'    The table should be generated using the `base_composition` function.
#' @return A tibble with the calculated housing voucher benefits, including payment amounts, family composition, and income details.
#' @export
housing_voucher <- function(base_table) {

  # create data frame of fair market rent values in 2019 based on family size
  # this is the max rent that can be reimbursed
  # https://www.huduser.gov/portal/datasets/fmr.html#2019_data
  fmr <- base_table |>
    dplyr::select(.data$adults, .data$children) |>
    dplyr::distinct() |>
    dplyr::mutate(fmr = c(583, 729, 729, 985, 583, 729, 729, 985),
    # we will assume people's rent amount is 80% of fmr
          rent = round(.data$fmr * .8 , 0)) |>
    dplyr::select(-.data$fmr)

  # add fmr to data set
  housing <- base_table |>
    dplyr::left_join(fmr, by=c('adults', 'children'), relationship = "many-to-one")

  # calculate total tenant payment
  # we're using an ifelse statement for kids because when there are
  # three kids, we're only taking a deduction for two of the kids
  # the third kid, who is ten, is assumed not to have any child care costs
  housing$tenant_payment <- tenant_rent(
    ifelse(housing$children == 3, 2, housing$children),
    housing$monthly_income
  )

  housing <- housing |>
    # benefit amount (payment) is rent minus ttp
    dplyr::mutate(
      payment = .data$rent - .data$tenant_payment,
      # if payment is negative, make it 0
      payment = ifelse(.data$payment < 0, 0, .data$payment),
      payment = round(.data$payment, 2),
      benefit = "Housing Choice Voucher"
    ) |>
    dplyr::select(.data$composition, .data$adults, .data$children, .data$monthly_income, .data$payment, .data$benefit)

  return(housing)

}