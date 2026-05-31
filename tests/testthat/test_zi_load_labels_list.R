# test zi_load_labels_list ------------------------------------------------

# test errors ------------------------------------------------

test_that("incorrectly specified parameters trigger appropriate errors", {
  expect_error(zi_load_labels_list(type = "bad"),
               "must be", fixed = TRUE)
  expect_error(zi_load_labels_list(type = "zip5"),
               "must be", fixed = TRUE)
})

# test inputs ------------------------------------------------

test_that("correctly specified functions execute without error", {
  skip_if_no_integration()
  expect_no_error(zi_load_labels_list(type = "zip3"))
})

# test outputs ------------------------------------------------

test_that("correctly specified functions produce expected output", {
  skip_if_no_integration()
  result <- zi_load_labels_list(type = "zip3")
  expect_s3_class(result, "tbl_df")
  expect_true("date" %in% names(result))
  expect_true(nrow(result) > 0)
})
