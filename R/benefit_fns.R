#############################################################################
#
# Create table of FNS (food stamp) benefits for 2019
# FNS is called SNAP federally, and is often called SNAP in this script
#
# the source of the information for the calcualtion is primarily from:
# https://www2.ncdhhs.gov/info/olm/manuals/dss/ei-30/man/FSs285.pdf
# https://www.fns.usda.gov/snap/allotment/cola
# https://fns-prod.azureedge.us/sites/default/files/media/file/FY19-Maximum-Allotments-Deductions.pdf
#
#############################################################################

#' @title Calculate FNS (Food and Nutrition Services) Benefits
#' @description Computes the Food and Nutrition Services (FNS) benefits, also known as SNAP federally, for households in North Carolina for the year 2019. 
#' The calculation incorporates income, household size, shelter costs, and deductions based on federal guidelines.
#' @param base_table A tibble containing the base household data, including family composition, size, and monthly income.
#'    The table should be generated using the `base_composition` function.
#' @return A tibble with calculated FNS benefits, including adjusted payments, deductions, and household-specific details.
#' @export
fns_snap <- function(base_table) {

  message("Calculating SNAP / FNS benefits")

  current_year <- 2019

  snap <- base_table |>
    dplyr::mutate(benefit = "FNS (Food Stamps)")

  # utility allowance based on family size
  # https://www.fns.usda.gov/snap/eligibility/deduction/standard-utility-allowances
  shelter_costs <- tibble::tibble(
    size = seq(1, 5),
    sua = c(437, 480, 528, 576, 628),
    bua = c(246, 270, 297, 324, 353),
    tua = 38,
    # rent starts at $600 and each additional person adds $200
    rent = 600 + (200*size)
  ) |>
    # make the shelter deduction the standard utility deduction and rent
    dplyr::mutate(shelter = sua + rent) |>
    dplyr::select(size, shelter)

  # merge utilitiy allowances to snap dataset
  snap <- snap |>
    dplyr::left_join(shelter_costs, by="size")

  # add column to dataset showing standard deduction amount
  snap <- snap |>
    dplyr::mutate(
      std_ded = dplyr::case_match(
        size,
        # standard deductions based on family size
        # https://fns-prod.azureedge.us/sites/default/files/media/file/FY19-Maximum-Allotments-Deductions.pdf
        1 ~ 164,
        2 ~ 164,
        3 ~ 164,
        4 ~ 174,
        5 ~ 204,
        6 ~ 234
      )
    )

  # all values should match
  if (any(is.na(snap$std_ded))) stop("SNAP standard deductions are not all defined", call. = FALSE)

  snap <- snap |>
    dplyr::mutate(
      # 20 percent of earned income is deducted,
      # so add column showing this amount
      ded_20 = monthly_income * .2,
      # for dependent care deduction, assume $200 per child per month
      dep_care = children * 400
    )

  # calculate SNAP amounts
  snap <- snap |>
    # calculate net income:
    # subtract standard deduction, earnings deducting, and child care deduction
    dplyr::mutate(
      net_income = monthly_income - std_ded - dep_care - ded_20,
      # deduct shelter expenses that exceed half of net income
      shelter_ded = shelter - (net_income/2),
      # shelter deduction is maxed out at 552
      # https://fns-prod.azureedge.us/sites/default/files/media/file/FY19-Maximum-Allotments-Deductions.pdf
      shelter_ded = ifelse(shelter_ded > 552, 552, shelter_ded),
      # subtract shelter deduction from net income
      net_income = net_income - shelter_ded,
      # family is expected to contribute 30% of income to food
      family_contribution = net_income * .3,
      # convert this amount to 0 if it is negative
      family_contribution = ifelse(family_contribution < 0, 0, family_contribution)
    )

  # maximum income is set at 200% of federal poverty guideline
  # read in federal poverty guidelines

  family_sizes <- unique(base_table$size)

  fpg <- get_poverty_guidelines(current_year, 'us', family_sizes, by_month = TRUE) |>
    dplyr::select(household_size, poverty_threshold)

  # convert guideline amounts to 200%
  snap_income_limit <- fpg |>
    dplyr::mutate(snap_income_limit = round(poverty_threshold * 2, 0)) |>
    dplyr::select(size = household_size, snap_income_limit)

  # add benefit and income limit amounts to dataset
  # https://fns-prod.azureedge.us/sites/default/files/media/file/FY19-Maximum-Allotments-Deductions.pdf
  snap <- snap |>
    dplyr::arrange(monthly_income, adults, children) |>
    dplyr::mutate(max_allotment = dplyr::case_match(
      size,
      1 ~ 192,
      2 ~ 353,
      3 ~ 505,
      4 ~ 642,
      5 ~ 762,
      6 ~ 914,
      7 ~ 1011,
      8 ~ 1155
    ))

  # add income limits and clean
  snap <- snap |>
    dplyr::left_join(snap_income_limit, by = "size", relationship = "many-to-one") |>
    # find benefit amount by subtracting family contribution from maximum benefit
    dplyr::mutate(
      snap_amount = max_allotment - family_contribution,
      # for families over 200% of federal poverty line, make benefit 0
      payment = ifelse(monthly_income > snap_income_limit, 0, snap_amount),
      # families with negative values for payment get zero in benefits
      payment = ifelse(payment < 0, 0, payment),
      # one and two person families must have at least $15 in benefits
      payment = ifelse((size %in% c(1,2) & payment < 15), 0, payment)) |>
    dplyr::select(composition, adults, children, monthly_income, payment, benefit)

  return(snap)

}
