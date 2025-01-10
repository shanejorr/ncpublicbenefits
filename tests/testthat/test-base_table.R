test_that("base_composition function works as expected", {
    
  # Test with default income_increment
  result <- base_composition()
  
  # Check that the result is a tibble
  expect_s3_class(result, "tbl_df")
  
  # Check that the result has the expected columns
  expected_columns <- c("composition", "size", "adults", "children", "monthly_income")
  expect_equal(colnames(result), expected_columns)
  
  # Check that the result has the expected number of rows
  # There are 8 compositions and 701 income levels (0 to 7000 by 10)
  expected_rows <- 8 * 701
  expect_equal(nrow(result), expected_rows)
  
  # Check that the income values are correct
  expect_equal(unique(result$monthly_income), seq(0, 7000, by = 10))
  
  # Check that the compositions are correct
  expected_compositions <- family_benefit_values('family')
  expect_equal(unique(result$composition), expected_compositions)
})