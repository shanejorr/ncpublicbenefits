############################################################################
#
# This script combines the individual benefit datasets to create a master dataset
#
##############################################################################

#' @title Calculate Total Take-Home Income
#' @description Computes the total take-home income for a household after considering all applicable taxes and benefits.
#' The calculation accounts for gross income, tax liabilities, and received benefits.
#' @param benefits_table A tibble containing calculated benefits data for the household.
#' @param family_composition A character vector of family compositions to include in the analysis.
#' @param unique_benefits A character vector of unique benefits to include in the analysis.
#' @return A tibble with the calculated total take-home income, including detailed breakdowns of income, taxes, and benefits.
#' @export
total_take_home_income <- function(benefits_table, family_composition, unique_benefits) {

  # calculate total take home pay, which is the sum of income, benefits, and taxes

  # check all parameters and values within dataset
  check_parameters(family_composition, family_benefit_values('family'), 'family_composition')
  check_parameters(unique_benefits, family_benefit_values('benefits'), 'unique_benefits')
  check_parameters(unique(benefits_table$composition), family_benefit_values('family'), 'composition column')
  check_parameters(unique(benefits_table$benefit), family_benefit_values('benefits'), 'benefits column')

  # only keep the needed family compositions and benefits
  filtered_summed_benefits <- benefits_table |>
    dplyr::filter(
      .data$composition %in% !!family_composition,
      .data$benefit %in% !!unique_benefits
    ) |>
    # calculate total benefit amounts for each family composition and income
    dplyr::summarize(
      total_benefit = sum(.data$payment),
      .by = c(.data$composition, .data$adults, .data$children, .data$monthly_income)
    )

  # add taxes
  tax_values <- calculate_taxes(filtered_summed_benefits)

  filtered_summed_benefits |>
    dplyr::left_join(tax_values, by = c("monthly_income", "adults", "children"), relationship = 'one-to-one') |>
    dplyr::mutate(
      income_minus_taxes = .data$monthly_income - .data$total_taxes,
      take_home = .data$monthly_income + .data$total_benefit - .data$total_taxes
    ) |>
    dplyr::select(-.data$adults, -.data$children, -.data$taxsimid)

}

#' @title Calculate Taxes
#' @description Computes federal and state taxes for a household based on their income and family composition.
#' The calculation includes adjustments for deductions, credits, and other tax-related considerations.
#' @param family_types A tibble containing family compositions, sizes, and monthly incomes.
#'    Use `create_benefits_table()` to calculate family types
#' @return A tibble with calculated federal and state taxes, including details of deductions and credits applied.
#' @export
calculate_taxes <- function(family_types) {

distinct_families <- family_types |>
    dplyr::distinct(.data$monthly_income, .data$adults, .data$children) |>
    dplyr::mutate(taxsimid = dplyr::row_number())

  tax_data <- distinct_families |>
    dplyr::mutate(yearly_income = .data$monthly_income * 12) |>
    dplyr::distinct(.data$yearly_income, .data$adults, .data$children) |>
    dplyr::rename(depx = .data$children) |>
    dplyr::mutate(
      taxsimid = dplyr::row_number(),
      year = 2019,
      mstat = dplyr::if_else(.data$adults == 1, 1, 2),
      state = "NC",
      page = 30,
      sage = dplyr::if_else(.data$adults == 1, 0, 30),
      age1 = dplyr::if_else(.data$depx >= 1, 5, 0),
      age2 = dplyr::if_else(.data$depx >= 2, 10, 0),
      age3 = dplyr::if_else(.data$depx >= 3, 15, 0),
      pwages = dplyr::if_else(.data$adults == 1, .data$yearly_income, .data$yearly_income / 2),
      swages = dplyr::if_else(.data$adults == 1, 0, .data$yearly_income / 2)
    ) |>
    dplyr::select(dplyr::all_of(c("taxsimid", "depx", "year", "mstat", "state", "page", "sage", "age1", "age2", "age3", "pwages", "swages")))

  tax_amounts <- usincometaxes::taxsim_calculate_taxes(
    .data = tax_data,
    marginal_tax_rates = 'Wages',
    return_all_information = FALSE
  ) |>
    dplyr::mutate(total_taxes = (.data$fiitax + .data$siitax + (.data$tfica / 2)) / 12) |>
  dplyr::select(dplyr::all_of(c("taxsimid", "total_taxes")))
  
  if (nrow(tax_amounts) != nrow(distinct_families)) stop("Problems calculating taxes", call. = FALSE)
  if (!all(tax_amounts$taxsimid == distinct_families$taxsimid)) stop("Problems calculating taxes", call. = FALSE)

  distinct_families |>
    dplyr::left_join(tax_amounts, by = "taxsimid", relationship = "one-to-one")

}

#' @title Create Benefits Table
#' @description Generates a comprehensive benefits table by combining multiple calculated benefits (e.g., TANF, SNAP, Medicaid) for a household.
#' The table includes details on household income, family composition, and total benefits received.
#' @param income_increment Numeric. The increment in monthly income for each row in the base table. Default is 10.
#' @return A tibble that aggregates all benefits for each household, with detailed breakdowns by benefit type.
#' @export
create_benefits_table <- function(income_increment = 10) {

  message("Create base table")

  base_table <- base_composition(income_increment)

  # check for proper column and family compositions in base table
  required_cols_base_table <- c('composition', 'size', 'adults', 'children', 'monthly_income')
  check_parameters(colnames(base_table), required_cols_base_table, 'base columns')
  check_parameters(unique(base_table$composition), family_benefit_values('family'), 'composition column')

  .data <- dplyr::bind_rows(
    list(
      child_care(base_table),
      fns_snap(base_table),
      housing_voucher(base_table),
      medicaid(base_table),
      tanf(base_table)
    )) |>
    dplyr::arrange(.data$benefit, .data$monthly_income, .data$adults, .data$children)

  return(.data)

}
