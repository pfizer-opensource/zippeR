context("test zi_validate function")

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
  expect_error(zi_validate(),
               "Please provide a vector of data for validation.")
})

test_that("incorrectly specified parameters trigger appropriate errors", {
  expect_error(zi_validate(incorrect_df),
               "Please provide a vector of data, instead of a data frame, for validation.")
  expect_error(zi_validate(correct_zips_1, style = "ham"),
               "The 'style' value provided is invalid. Please select either 'zcta5' or 'zcta3'.")
  expect_error(zi_validate(correct_zips_1, verbose = "ham"),
               "The 'verbose' value provided is invalid. Please select either 'TRUE' or 'FALSE'.")
})

# test inputs ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(zi_validate(correct_zips_1), NA)
  expect_error(zi_validate(correct_zips_1, verbose = TRUE), NA)
  expect_error(zi_validate(correct_zips_2, style = "zcta3"), NA)
})

# test outputs ------------------------------------------------

test_that("correctly specified functions produce expected classes", {
  expect_type(zi_validate(correct_zips_1), "logical")
  expect_s3_class(zi_validate(correct_zips_1, verbose = TRUE), "tbl_df")
})

result1 <- zi_validate(correct_zips_1)
result2 <- zi_validate(correct_zips_2, style = "zcta3")
result3 <- zi_validate(incorrect_zips_1)
result4 <- zi_validate(incorrect_zips_2)
result5 <- zi_validate(incorrect_zips_3, style = "zcta3")
result6 <- zi_validate(incorrect_zips_4)
result12 <- zi_validate(incorrect_zips_5)

test_that("correctly specified functions produce expected output", {
  expect_equal(result1, TRUE)
  expect_equal(result2, TRUE)
  expect_equal(result3, FALSE)
  expect_equal(result4, FALSE)
  expect_equal(result5, FALSE)
  expect_equal(result6, FALSE)
  expect_equal(result12, FALSE)
})

result7 <- zi_validate(incorrect_zips_1, verbose = TRUE)
result8 <- zi_validate(incorrect_zips_2, verbose = TRUE)
result9 <- zi_validate(incorrect_zips_3, verbose = TRUE, style = "zcta3")
result10 <- zi_validate(incorrect_zips_4, verbose = TRUE)
result11 <- zi_validate(incorrect_zips_5, verbose = TRUE)

test_that("correctly specified functions produce expected output", {
  expect_equal(result7$result, c(TRUE, FALSE, FALSE, TRUE))
  expect_equal(result8$result, c(TRUE, FALSE, TRUE, TRUE))
  expect_equal(result9$result, c(TRUE, FALSE, TRUE, TRUE))
  expect_equal(result10$result, c(TRUE, FALSE, TRUE, FALSE))
  expect_equal(result11$result, c(FALSE, TRUE, TRUE, TRUE))
})
