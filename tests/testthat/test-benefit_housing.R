test_that("housing_voucher computes correct housing benefits", {
  # Create a dummy base_table with required columns
  base_table <- base_composition()
  
  # Execute the housing_voucher function
  result <- housing_voucher(base_table)

  # test case  
  result <- result |>
    filter(
        adults == 1, 
        children == 1, 
        monthly_income == 100
    )

  
  # Expected columns in the output
  expected_cols <- c("composition", "adults", "children", "monthly_income", "payment", "benefit")
  expect_equal(colnames(result), expected_cols)
  
  # Explanation:
  # For a 1-adult, 1-child family:
  # 1. fmr is 729.
  # 2. Rent = round(729 * 0.8, 0) = 583.
  # 3. tenant_rent() (with kids=1, income=100) yields a tenant payment of -152.
  # 4. Voucher payment = 583 - (-152) = 735.
  expect_equal(result$payment, 735)
  expect_true(all(result$benefit == "Housing Choice Voucher"))
  
  # Check that the result is a tibble (i.e. a data frame with class "tbl_df")
  expect_s3_class(result, "tbl_df")
})