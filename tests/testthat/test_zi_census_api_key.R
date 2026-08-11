# create test data ------------------------------------------------

correct_key <- "111111abc"
incorrect_key_num <- 111111
incorrect_key_vec <- c("111111abc", "222222def")

# test errors ------------------------------------------------

test_that("missing parameters trigger appropriate errors", {
  expect_error(zi_census_api_key(),
               "`key` is required", fixed = TRUE)
})

test_that("incorrectly specified parameters trigger appropriate errors", {
  expect_error(zi_census_api_key(key = incorrect_key_vec),
               "`key` must be a single value", fixed = TRUE)
  expect_error(zi_census_api_key(key = incorrect_key_num),
               "`key` must be a character scalar", fixed = TRUE)
  expect_error(zi_census_api_key(key = correct_key, overwrite = "yes"),
               "`overwrite` must be", fixed = TRUE)
  expect_error(zi_census_api_key(key = correct_key, overwrite = c(TRUE, FALSE)),
               "`overwrite` must be a single value", fixed = TRUE)
  expect_error(zi_census_api_key(key = correct_key, install = "yes"),
               "`install` must be", fixed = TRUE)
  expect_error(zi_census_api_key(key = correct_key, install = c(TRUE, FALSE)),
               "`install` must be a single value", fixed = TRUE)
})
