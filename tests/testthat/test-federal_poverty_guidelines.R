test_that("get_single_poverty_guidelines returns correct tibble for valid input", {
  skip_on_cran()
  skip_if_offline()
  
  # Use a specific year likely to be valid (ensure current_year >= chosen year)
  year <- 2020
  state <- "us"
  household_size <- 3
  
  result <- get_single_poverty_guidelines(year, state, household_size)
  
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("year", "state", "household_size", "poverty_threshold") %in% colnames(result)))
  
  # Check that the year and household_size are correctly set
  expect_equal(result$year, as.numeric(year))
  expect_equal(result$household_size, as.numeric(household_size))
  expect_equal(result$state, state)
})

test_that("get_single_poverty_guidelines fails with invalid inputs", {
  skip_on_cran()
  # Invalid state
  expect_error(get_single_poverty_guidelines(2020, "xx", 3), "Invalid state")
  # Invalid year
  expect_error(get_single_poverty_guidelines(1800, "us", 3), "Invalid year")
  # Invalid household size
  expect_error(get_single_poverty_guidelines(2020, "us", 10), "Invalid household size")
})

test_that("get_single_poverty_guidelines handles by_month correctly", {
  skip_on_cran()
  skip_if_offline()
  
  year <- 2020
  state <- "us"
  household_size <- 3
  
  result_annual <- get_single_poverty_guidelines(year, state, household_size, by_month = FALSE)
  result_monthly <- get_single_poverty_guidelines(year, state, household_size, by_month = TRUE)
  
  # Check that monthly value is exactly annual divided by 12
  expect_equal(result_monthly$poverty_threshold, result_annual$poverty_threshold / 12)
})

test_that("get_poverty_guidelines returns data for multiple parameters", {
  skip_on_cran()
  skip_if_offline()
  
  years <- c(2020, 2021)
  states <- "us"
  household_sizes <- c(1, 2)
  
  result <- get_poverty_guidelines(years, states, household_sizes)
  
  # There should be length(years) * length(household_sizes) rows
  expect_equal(nrow(result), length(years) * length(household_sizes))
  expect_true(all(c("year", "state", "household_size", "poverty_threshold") %in% colnames(result)))
})
