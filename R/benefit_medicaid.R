###############################################################################
#
# This script calculates Medicaid for Infants and Children (mic),
# Medicaid for Families with Dependent Children (maf),
# and NC Health Choices
#
# All eligibility limits are here:
# https://files.nc.gov/ncdma/documents/files/Basic-Medicaid-Eligibility-Chart-2019_0.pdf
#
###############################################################################

#' @title Calculate Medicaid Benefits
#' @description Computes Medicaid benefits for infants, children, and families with dependent children, 
#' as well as NC Health Choice benefits, based on the 2019 eligibility guidelines for North Carolina. 
#' The function determines benefit values using federal poverty levels and ACA silver plan prices.
#' @param base_table A tibble containing the base household data, including family composition, household size, and monthly income.
#'    The table should be generated using the `base_composition` function.
#' @return A tibble with calculated Medicaid and Health Choice benefits, including payments 
#' for children and adults, income thresholds, and total benefit amounts.
#' @export
medicaid <- function(base_table) {

  message("Calculating medicaid benefits")

  current_year <- 2019

  family_sizes <- unique(base_table$size)

  # read in poverty guidelines and filter for 2019
  fpl <- get_poverty_guidelines(current_year, 'us', family_sizes, by_month = TRUE) |>
    dplyr::select(household_size = .data$household_size, guidelines_month = .data$poverty_threshold)

  # medicaid for children and NC health choices provide generally the same
  # benefits, and they create a continuous eligibility stream up to 210%
  # of the federal poverty level
  # therefore, convert the poverty level to 210%, as a ceiling
  fpl$guidelines_month <- fpl$guidelines_month * 2.1

  # the value of the health benefit is the price of a silver plan on the ACA market
  # silver plan prices for Forsyth County in 2019 were retrieved from:
  # https://www.kff.org/interactive/subsidy-calculator/

  # since children and adults qualify for different programs, calcualte the value
  # of their silver plans separately

  # create columns in dataset for the market value of mic and maf
  # and column for maf income thresholds
  medical <- base_table |>
    dplyr::mutate(
      # calculate value of children's silver plans, based on number of children
      payment_mic = dplyr::case_match(
        .data$children,
        0 ~ 0,
        1 ~ 358, # one child: 2 years old
        2 ~ 716, # two children: 2 and 4 years old
        3 ~ 1074,  # three children: 2, 4, and 10 years old
        .default = NA
      ),
      # calculate value of adults silver plan; adults are non-tobacco users
      payment_maf = dplyr::case_match(
        .data$adults,
        1 ~ 531, # one adult: 30 years old
        2 ~ 1062, # two adults: both 30 years old
        .default = NA
      ),
      # calculate income limits of maf based on number of caretakers
      maf_threshold = dplyr::case_match(
        .data$size,
        1 ~ 434,
        2 ~ 569,
        3 ~ 667,
        4 ~ 744,
        5 ~ 824,
        .default = NA
      )
    )

  # merge fpl thresholds
  medical <- medical |>
    dplyr::left_join(fpl, by = c("size" = "household_size"), relationship = 'many-to-one') |>
    # if family does not qualify for mic or health choice due to high income, make payment 0
    dplyr::mutate(
      payment_mic = ifelse(.data$monthly_income > .data$guidelines_month, 0, .data$payment_mic),
      # if family does not qualify for maf due to high income, make payment 0
      payment_maf = ifelse(.data$monthly_income > .data$maf_threshold, 0, .data$payment_maf),
      # cannot receive maf without children
      payment_maf = ifelse(.data$children == 0, 0, .data$payment_maf),
      # sum mic and maf into one payment column
      payment = .data$payment_mic + .data$payment_maf,
      benefit = "NC Medicaid / Health Choice"
    )

  if (any(is.na(medical$guidelines_month))) stop("FPG for medicaid not all defined", call. = FALSE)
  if (any(is.na(medical$payment))) stop("Not all medicaid payments defined", call. = FALSE)

  medical <- medical |>
    dplyr::select(.data$composition, .data$adults, .data$children, .data$monthly_income, .data$payment, .data$benefit)

  return(medical)

}
