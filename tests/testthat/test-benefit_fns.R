test_that("fns_snap returns proper output", {
  # Temporarily replace get_poverty_guidelines in the package namespace with a mock
  local_mocked_bindings(
    get_poverty_guidelines = function(year, state, family_sizes, by_month = FALSE) {
      tibble::tibble(
        household_size = family_sizes,
        poverty_threshold = rep(500, length(family_sizes))
      )
    },
    .package = "ncpublicbenefits"
  )
  
  # Create a dummy base_table suitable for fns_snap()
  base_table <- tibble::tibble(
    composition = "TestFamily",
    adults = 2L,
    children = 1L,
    monthly_income = 1000,
    size = 2L
  )
  
  # Execute the function
  result <- fns_snap(base_table)
  
  # Check that the result is a tibble with expected columns
  expect_s3_class(result, "tbl_df")
  expected_columns <- c("composition", "adults", "children", "monthly_income", "payment", "benefit")
  expect_equal(colnames(result), expected_columns)
  
  # Check that the benefit column has the expected value
  expect_true(all(result$benefit == "FNS (Food Stamps)"))
  
  # Check that the payment values are numeric and non-negative
  expect_true(is.numeric(result$payment))
  expect_true(all(result$payment >= 0))

  # test value amount of payment
  expect_equal(result$payment[1], 353)
})