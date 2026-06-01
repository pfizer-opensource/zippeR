# create test data ------------------------------------------------

incorrect_year_str <- "2010"
incorrect_year_num <- 2009
out_of_range_year <- 2025
unavailable_year <- 2024
incorrect_method <- "ham"

correct_year <- 2010
correct_method <- "centroid"
states = "WA"

# test errors ------------------------------------------------

test_that("missing parameters trigger appropriate errors", {
  expect_error(zi_list_zctas(),
               "`year` is required", fixed = TRUE)
})

test_that("incorrectly specified parameters trigger appropriate errors", {
  expect_error(zi_list_zctas(year = incorrect_year_str, method = correct_method, state = states),
               "`year` must be numeric", fixed = TRUE)
  expect_error(zi_list_zctas(year = incorrect_year_num, method = correct_method, state = states),
               "`year` must be between", fixed = TRUE)
  expect_error(zi_list_zctas(year = out_of_range_year, method = correct_method, state = states),
               "`year` must be between", fixed = TRUE)
  expect_error(zi_list_zctas(year = unavailable_year, method = correct_method, state = states),
               "is not yet available", fixed = TRUE)
  expect_error(zi_list_zctas(method = incorrect_method, year = correct_year, state = states),
               "`method` must be", fixed = TRUE)
  expect_warning(
    expect_error(zi_list_zctas(year = correct_year, method = correct_method, state = "ZZ"),
                 "No valid states found", fixed = TRUE),
    "not a valid FIPS"
  )
})


# test inputs ------------------------------------------------

test_that("correctly specified functions execute without error", {
  skip_if_no_integration()
  expect_error(zi_list_zctas(year = correct_year, method = correct_method, state = states), NA)
})

# test outputs ------------------------------------------------

test_that("correctly specified functions produce expected classes", {
  skip_if_no_integration()
  t <- zi_list_zctas(year = correct_year, method = correct_method, state = states)
  expect_type(t, "character")
})

test_that("correctly specified functions produce expected result length", {
  skip_if_no_integration()
  t <- zi_list_zctas(year = correct_year, method = correct_method, state = states)
  r <- strsplit(t[1], "")
  expect_length(r[[1]], 5)
})
