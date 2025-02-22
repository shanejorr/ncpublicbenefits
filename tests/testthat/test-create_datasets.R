test_that("create_benefits_table returns a tibble with expected columns", {
  benefits <- create_benefits_table()
  expected_cols <- c("composition", "adults", "children", "monthly_income", "payment", "benefit")
  expect_s3_class(benefits, "tbl_df")
  expect_equal(sort(colnames(benefits)), sort(expected_cols))
})


test_that("total_take_home_income calculates take-home pay correctly", {
  # Create a dummy benefits table containing two benefit types and simple values.
  dummy <- tibble(
    composition = rep("1 adult, 1 child", 4),
    adults = rep(1, 4),
    children = rep(1, 4),
    monthly_income = c(1000, 1500, 2000, 2500),
    payment = c(100, 200, 300, 400),
    benefit = rep("NC Medicaid / Health Choice", 4)
  )
  
  # Generate a dummy taxes tibble based on the dummy benefits table.
  tax_dummy <- dummy %>%
    distinct(monthly_income, adults, children) %>%
    mutate(taxsimid = row_number(),
           total_taxes = 50)
  
  # Stub out calculate_taxes to return our dummy tax tibble.
  local_mocked_bindings(
    calculate_taxes = function(x) tax_dummy,
    .package = "ncpublicbenefits"
  )
  
  # Call the function under test.
  result <- total_take_home_income(dummy, 
                                   family_composition = "1 adult, 1 child", 
                                   unique_benefits = "NC Medicaid / Health Choice")
  
  # Check that the computed columns exist.
  expect_true("income_minus_taxes" %in% colnames(result))
  expect_true("take_home" %in% colnames(result))
  
  # For monthly_income = 1500 and payment = 200, we expect:
  # income_minus_taxes = 1500 - 50 = 1450
  # take_home = 1500 + 200 - 50 = 1650.
  selected <- result %>% filter(monthly_income == 1500)
  expect_equal(selected$income_minus_taxes, 1450)
  expect_equal(selected$take_home, 1650)
})

test_that("calculate_taxes returns expected structure", {
  # Create a dummy family_types tibble similar to the output of create_benefits_table
  dummy_family <- tibble(
    composition = rep("2 adults, 1 child", 3),
    adults = rep(2, 3),
    children = rep(1, 3),
    monthly_income = c(1200, 1800, 2400),
    payment = c(150, 250, 350),
    benefit = rep("FNS (Food Stamps)", 3)
  )
  
  taxes <- calculate_taxes(dummy_family)
  
  expected_cols <- c("taxsimid", "monthly_income", "adults", "children", "total_taxes")
  expect_true(all(expected_cols %in% colnames(taxes)))
  
  # Ensure that total_taxes is numeric and has length equal to the number of distinct income/adults/children combinations
  distinct_count <- dummy_family %>%
    distinct(monthly_income, adults, children) %>%
    nrow()
  
  expect_equal(nrow(taxes), distinct_count)
  expect_type(taxes$total_taxes, "double")
})
