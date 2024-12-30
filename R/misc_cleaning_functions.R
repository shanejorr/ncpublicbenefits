#' @title Retrieve Family Benefit Values
#' @description Returns a list of valid family types or benefit types based on the specified value name. 
#' This function ensures the provided value name is valid and retrieves the corresponding values.
#' @param value_name Character. The name of the value to retrieve. Must be one of "family" or "benefits".
#' @return A character vector containing the requested family types or benefit types.
#' @export
family_benefit_values <- function(value_name) {

  value_name_params <- c('family', 'benefits')

  check_parameters(value_name, value_name_params)

  family_and_benefits <- list(
    family = c(
      "1 adult", "1 adult, 1 child", "1 adult, 2 children", "1 adult, 3 children",
      "2 adults", "2 adults, 1 child", "2 adults, 2 children", "2 adults, 3 children"
    ),
    benefits = c(
      "FNS (Food Stamps)", "Housing Choice Voucher", "NC Child Care Subsidy", "NC Medicaid / Health Choice", "Work First (TANF)"
    )
  )

  family_and_benefits[[value_name]]

}

#' @title Validate Parameter Values
#' @description Checks if all elements in a data vector are valid based on a reference vector. 
#' If any element is invalid, the function throws an error with a descriptive message.
#' @param actual_data A vector of values to be validated.
#' @param test_data A vector of valid reference values.
#' @param test_name Character. A name used to describe the type of data being tested (e.g., "value_name").
#' @return Throws an error if validation fails; otherwise, the function returns nothing.
#' @export
check_parameters <- function(actual_data, test_data, test_name) {

  if (!all(actual_data %in% test_data)) {
    stop(
      glue::glue("Error: Some values in {test_name} are not valid. Valid values are:\n"),
      paste(sprintf("'%s'", test_data), collapse = ", "),
      call. = FALSE
    )
  }

}
