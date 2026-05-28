# test zi_load_labels ------------------------------------------------

# test errors ------------------------------------------------

test_that("incorrectly specified parameters trigger appropriate errors", {
  expect_error(zi_load_labels(source = "INVALID"),
               "must be", fixed = TRUE)
  expect_error(zi_load_labels(source = "UDS", type = "bad"),
               "must be", fixed = TRUE)
  expect_error(zi_load_labels(source = "UDS", type = "zip3"),
               "must be", fixed = TRUE)
  expect_error(zi_load_labels(source = "USPS", type = "zip5"),
               "must be", fixed = TRUE)
  expect_error(zi_load_labels(source = "UDS", vintage = 2000),
               "must be between", fixed = TRUE)
})

test_that("include_scf with zip5 produces a warning", {
  skip_on_cran()
  skip_if_offline()
  expect_warning(zi_load_labels(source = "UDS", type = "zip5", include_scf = TRUE, vintage = 2022),
                 "include_scf", fixed = TRUE)
})
