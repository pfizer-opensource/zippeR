context("test zi_list_zctas function")

# create test data ------------------------------------------------

incorrect_year_str <- "2010"
incorrect_year_num <- 2009
incorrect_method <- "ham"

correct_year <- 2010
correct_method <- "centroid"
states = "WA"

# test errors ------------------------------------------------

test_that("missing parameters trigger appropriate errors", {
  expect_error(zi_list_zctas(),
               "The 'year' value is missing. Please provide a numeric value between 2010 and 2023.")
})

test_that("incorrectly specified parameters trigger appropriate errors", {
  expect_error(zi_list_zctas(year = incorrect_year_str, method = correct_method, state = states),
               "The 'year' value provided is invalid. Please provide a numeric value between years 2010 and 2023.")
  expect_error(zi_list_zctas(year = incorrect_year_num, method = correct_method, state = states),
               "The 'year' value provided is invalid. Please provide a numeric value between years 2010 and 2023.")
  expect_error(zi_list_zctas(method = incorrect_method, year = correct_year, state = states),
               "The two valid methods for returning ZCTA values are 'centroid' and 'intersect'. See documentation for details.")
})


# test inputs ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(zi_list_zctas(year = correct_year, method = correct_method, state = states), NA)
})

# test outputs ------------------------------------------------

t <- zi_list_zctas(year = correct_year, method = correct_method, state = states)
r <- strsplit(t[1], "")

test_that("correctly specified functions produce expected classes", {
  expect_type(t, "character")
})

test_that("correctly specified functions produce expected result length", {
  expect_length(r[[1]], 5)
})
