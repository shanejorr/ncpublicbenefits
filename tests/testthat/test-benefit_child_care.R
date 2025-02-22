test_that("child_care returns correct output", {
  local_mocked_bindings(
    get_poverty_guidelines = function(year, state, family_sizes, by_month = FALSE) {
      tibble(
        household_size = family_sizes,
        poverty_threshold = rep(500, length(family_sizes))
      )
    },
    .package = "ncpublicbenefits"
  )
  
  # Create a dummy base_table
  base_table <- tibble(
    composition = "Family1",
    adults = 1L,
    children = 1L,
    monthly_income = 100,
    size = 1L
  )
  
  # Execute the function
  result <- child_care(base_table)
  
  # Test that the output is a tibble with correct columns
  expect_s3_class(result, "tbl_df")
  expect_equal(
    colnames(result),
    c("composition", "adults", "children", "monthly_income", "payment", "benefit")
  )
  
  # With one child, market rate is 750. Subtracting 10% of 100 (i.e. 10) gives 740.
  # The mocked get_poverty_guidelines returns an income_limit of round(500 * 2) = 1000,
  # so 100 < 1000 and therefore payment remains 740.
  expect_equal(result$payment, 740)
  expect_true(all(result$benefit == "NC Child Care Subsidy"))
})