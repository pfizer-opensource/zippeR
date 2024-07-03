context("test zi_repair function")

# create test data ------------------------------------------------

correct_zips_1 <- c("63088", "63108", "63139")
correct_zips_2 <- c("630", "631", "632")

incorrect_zips_1 <- c("63088", "63108", "631397")
incorrect_zips_2 <- c("63088", "63108", "456")
incorrect_zips_3 <- c("630", "631", "4")
incorrect_zips_4 <- c("63088", "63108", "ham")
incorrect_zips_5 <- c(63088, 63108, 63139)

incorrect_df <- data.frame(
  id = c(1:3),
  zips = correct_zips_1
)

# test errors ------------------------------------------------

test_that("missing parameters trigger appropriate errors", {
  expect_error(zi_repair(),
               "Please provide a vector of data for validation.")
})

test_that("incorrectly specified parameters trigger appropriate errors", {
  expect_error(zi_validate(incorrect_df),
               "Please provide a vector of data, instead of a data frame, for validation.")
  expect_error(zi_validate(correct_zips_1, style = "ham"),
               "The 'style' value provided is invalid. Please select either 'zcta5' or 'zcta3'.")
})

# test inputs ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(zi_repair(incorrect_zips_2), NA)
  expect_error(zi_repair(incorrect_zips_3, style = "zcta3"), NA)
})

# test outputs ------------------------------------------------

test_that("correctly specified functions produce expected classes", {
  expect_type(zi_repair(incorrect_zips_2), "character")
  expect_type(zi_repair(incorrect_zips_3, style = "zcta3"), "character")
})

test_that("correctly specified functions produce messages", {
  expect_message(zi_repair(correct_zips_1), "This is a valid vector of ZIP or ZCTA codes - nothing to repair!")
  expect_message(zi_repair(correct_zips_2, style = "zcta3"), "This is a valid vector of ZIP or ZCTA codes - nothing to repair!")
})

test_that("correctly specified functions produce warnings", {
  expect_warning(zi_repair(incorrect_zips_1), "NAs introduced by coercion")
  expect_warning(zi_repair(incorrect_zips_4), "NAs introduced by coercion")
})

result1 <- suppressWarnings(zi_repair(incorrect_zips_1))
result2 <- zi_repair(incorrect_zips_2)
result3 <- zi_repair(incorrect_zips_3, style = "zcta3")
result4 <- suppressWarnings(zi_repair(incorrect_zips_4))
result5 <- zi_repair(incorrect_zips_5)

test_that("correctly specified functions produce expected output", {
  expect_equal(result1, c("63088", "63108", NA))
  expect_equal(result2, c("63088", "63108", "00456"))
  expect_equal(result3, c("630", "631", "004"))
  expect_equal(result4, c("63088", "63108", NA))
  expect_equal(result5, c("63088", "63108", "63139"))
})
