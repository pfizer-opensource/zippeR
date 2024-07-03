context("test zi_load_crosswalk function")

# test errors ------------------------------------------------

test_that("incorrectly specified parameters trigger appropriate errors", {
  expect_error(zi_load_crosswalk(zip_source = "ham", year = 2022),
               "The 'zip_source' value provided is invalid. Please input either 'UDS' or 'HUD'.")

})
