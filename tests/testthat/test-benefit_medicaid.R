test_that("medicaid returns correct output", {
  local_mocked_bindings(
    get_poverty_guidelines = function(year, state, family_sizes, by_month = TRUE) {
      tibble(
        household_size = family_sizes,
        poverty_threshold = rep(1000, length(family_sizes))
      )
    },
    .package = "ncpublicbenefits"
  )
  
  # Create a dummy base_table
  base_table <- tibble(
    composition = c("A", "B", "C"),
    adults = c(1L, 1L, 2L),
    children = c(1L, 1L, 0L),
    monthly_income = c(1500, 500, 500),
    size = c(2L, 2L, 2L)
  )
  
  result <- medicaid(base_table)
  
  # Expected payment calculations:
  # fpl guidelines: guidelines_month = 1000 * 2.1 = 2100.
  # Maf threshold for size 2 is set to 569.
  #
  # Row 1 (composition "A"):
  #   monthly_income 1500 < 2100 so payment_mic = 358,
  #   but 1500 > 569 so payment_maf = 0;
  #   Total expected payment = 358.
  #
  # Row 2 (composition "B"):
  #   monthly_income 500 < 2100 so payment_mic = 358,
  #   and 500 < 569 so payment_maf = 531;
  #   Total expected payment = 358 + 531 = 889.
  #
  # Row 3 (composition "C"):
  #   No children (children == 0) forces both payment_mic and payment_maf to 0;
  #   Total expected payment = 0.
  
  expect_equal(result$payment, c(358, 889, 0))
  expect_equal(result$benefit, rep("NC Medicaid / Health Choice", 3))
  expect_named(result, c("composition", "adults", "children", "monthly_income", "payment", "benefit"))
})