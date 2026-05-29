# test errors ------------------------------------------------

test_that("incorrectly specified parameters trigger appropriate errors", {
  expect_error(zi_load_crosswalk(zip_source = "ham", year = 2022),
               "`zip_source` must be", fixed = TRUE)

})

# test UDS crosswalk loading (requires network) ------------------------------

test_that("2015 UDS crosswalk loads successfully with normalized columns", {
  skip_on_cran()
  skip_if_offline()

  result <- zi_load_crosswalk(zip_source = "UDS", year = 2015)
  expect_s3_class(result, "tbl_df")
  expect_true("ZIP" %in% names(result))
  expect_true("ZCTA" %in% names(result))
  expect_true(nrow(result) > 0)
  expect_true(all(nchar(result$ZIP) == 5))
  expect_true(all(nchar(result$ZCTA) == 5))
})
