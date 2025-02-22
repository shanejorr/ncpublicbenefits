################################################################################
#
# functions to pull in federal poverty guidelines from the HHS API
# API documentation: https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines/poverty-guidelines-api
#
###############################################################################

#' @title Fetch Federal Poverty Guidelines for single household size
#' @description Fetches the federal poverty guideline for a given year, state, and household size from the HHS API.
#'    The poverty guideline is the same for all states except Hawaii and Alaska.
#' @param year An integer representing the year. Valid years are between 1983 and 2024.
#' @param state A character string representing the state. Valid values are 'us' (contiguous U.S. and D.C.), 'hi' (Hawaii), and 'ak' (Alaska).
#' @param household_size An integer representing the household size. Valid values are between 1 and 8.
#' @param by_month A logical value indicating whether to return the poverty threshold as a monthly value. Default is FALSE.
#' @return A tibble with columns 'year', 'state', 'household_size', and 'poverty_threshold'.
#' @export
get_single_poverty_guidelines <- function(year, state, household_size, by_month = FALSE) {

  current_year <- lubridate::year(Sys.Date())

  # Validate inputs
  valid_states <- c("us", "hi", "ak")
  valid_years <- 1983:current_year
  valid_household_sizes <- 1:8

  if (!state %in% valid_states) {
    stop("Invalid state. Valid values are 'us', 'hi', and 'ak'.")
  }

  if (!year %in% valid_years) {
    stop("Invalid year. Valid years are between 1983 and 2024.")
  }

  if (!household_size %in% valid_household_sizes) {
    stop("Invalid household size. Valid values are between 1 and 8.")
  }

  # Build the API request URL
  url <- paste0("https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines/api/",
                year, "/", state, "/", household_size)

  # Make the GET request
  response <- httr2::request(url) |> httr2::req_perform()

  # Check if the request was successful
  if (httr2::resp_status(response) != 200) {
    stop("Failed to fetch data. Status code: ", httr2::resp_status(response))
  }

  # Extract the data from the response
  data <- httr2::resp_body_json(response)$data

  # Return the data as a data frame
  data <- tibble::tibble(
    year = as.numeric(data$year),
    state = data$state,
    household_size = as.numeric(data$household_size),
    poverty_threshold = as.numeric(data$income)
  )

  if (by_month) {
    data$poverty_threshold <- data$poverty_threshold / 12
  }

  return(data)

}

#' @title Fetch Multiple Federal Poverty Guidelines for multiple household sizes
#' @description Fetches federal poverty guidelines for multiple years and/or household sizes from the HHS API.
#'    The poverty guideline is the same for all states except Hawaii and Alaska.
#'    Guideline values are per year.
#' @param years A vector of integers representing the years. Valid years are between 1983 and 2024.
#' @param states A character string representing the state. Valid values are 'us' (contiguous U.S. and D.C.), 'hi' (Hawaii), and 'ak' (Alaska).
#' @param household_sizes A vector of integers representing household sizes. Valid values are between 1 and 8.
#' @param by_month A logical value indicating whether to return the poverty threshold as a monthly value. Default is FALSE.
#' @return A tibble with columns 'year', 'state', 'household_size', and 'poverty_threshold' for each combination of year and household size.
#' @export
get_poverty_guidelines <- function(years, states, household_sizes, by_month = FALSE) {

  # Create list that contains all combinations of parameters
  # each list element contains one combination of parameters
  params <- tidyr::expand_grid(year = years, state = states, household_size = household_sizes) |>
    dplyr::mutate(.id = dplyr::row_number()) |>
    dplyr::group_by(.data$.id) |>
    dplyr::group_split() |>
    purrr::map(as.list)

  # Map over the parameters and call get_poverty_guidelines for each combination
  results <- purrr::map(
    params,
    function(x) get_single_poverty_guidelines(x$year, x$state, x$household_size)
  ) |>
    purrr::list_rbind()

  return(results)
}

