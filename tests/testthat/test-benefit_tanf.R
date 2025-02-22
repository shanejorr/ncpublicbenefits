test_that("tanf returns correct output", {
  # Create a dummy base_table with several scenarios
  base_table <- tibble(
    composition = c("Family1", "Family2", "Family3", "Family4"),
    adults = c(1L, 1L, 2L, 2L),
    children = c(1L, 0L, 2L, 2L),
    monthly_income = c(300, 200, 1000, 600),
    size = c(1L, 1L, 4L, 3L)
  )
  
  # Hand calculation:
  # Row 1 ("Family1", size 1, 300, 1 child):
  #   tanf_monthly_income = 300 - 27.5% * 300 = 300 * 0.725 = 217.5
  #   need_std for size 1 = 362
  #   raw payment = (362 - 217.5) * 0.5 = 144.5 * 0.5 = 72.25 → round(72.25) = 72
  #   Payment is >=25 and children > 0, so payment = 72.
  #
  # Row 2 ("Family2", size 1, 200, 0 children):
  #   Regardless of the income calculation, no children means payment = 0.
  #
  # Row 3 ("Family3", size 4, 1000, 2 children):
  #   tanf_monthly_income = 1000 * 0.725 = 725
  #   need_std for size 4 = 594
  #   raw payment = (594 - 725) * 0.5 = (-131) * 0.5 = -65.5 → round(-65.5) = -66
  #   Since payment < 25, set to 0.
  #
  # Row 4 ("Family4", size 3, 600, 2 children):
  #   tanf_monthly_income = 600 * 0.725 = 435
  #   need_std for size 3 = 544
  #   raw payment = (544 - 435) * 0.5 = 109 * 0.5 = 54.5 → round(54.5) = 54
  #   Payment is >=25, so remains 54.
  
  result <- tanf(base_table)
  
  # The final dataset is arranged by monthly_income, adults, and children (ascending)
  # Expected order and values:
  # - Row with monthly_income = 200 ("Family2"): payment should be 0.
  # - Row with monthly_income = 300 ("Family1"): payment should be 72.
  # - Row with monthly_income = 600 ("Family4"): payment should be 54.
  # - Row with monthly_income = 1000 ("Family3"): payment should be 0.
  
  expected <- tibble(
    composition = c("Family2", "Family1", "Family4", "Family3"),
    adults = c(1L, 1L, 2L, 2L),
    children = c(0L, 1L, 2L, 2L),
    monthly_income = c(200, 300, 600, 1000),
    payment = c(0L, 72L, 54L, 0L)
  ) %>%
    mutate(benefit = "Work First (TANF)")
  
  expect_s3_class(result, "tbl_df")
  expect_named(result,
               c("composition", "adults", "children", "monthly_income", "payment", "benefit"))
  
  expect_equal(result$payment, expected$payment)
  expect_equal(result$benefit, expected$benefit)
  
  # Also check for complete equality of the resulting data frame
  expect_equal(result, expected)
})
