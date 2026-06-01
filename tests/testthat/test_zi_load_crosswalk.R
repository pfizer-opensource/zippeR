# test errors ------------------------------------------------

test_that("incorrectly specified parameters trigger appropriate errors", {

  expect_error(zi_load_crosswalk(zip_source = "ham", year = 2022),
               "`zip_source` must be", fixed = TRUE)

})

test_that("non-numeric year triggers appropriate error", {
  expect_error(zi_load_crosswalk(zip_source = "UDS", year = "2020"),
               "`year` must be numeric", fixed = TRUE)
})

# test HUD input validation ------------------------------------------------

test_that("HUD year out of range triggers appropriate error", {
  expect_error(
    zi_load_crosswalk(zip_source = "HUD", year = 2009, qtr = 1, target = "COUNTY", query = "63139"),
    "`year` must be between", fixed = TRUE
  )
  expect_error(
    zi_load_crosswalk(zip_source = "HUD", year = 2025, qtr = 1, target = "COUNTY", query = "63139"),
    "`year` must be between", fixed = TRUE
  )
})

test_that("HUD invalid quarter triggers appropriate error", {
  expect_error(
    zi_load_crosswalk(zip_source = "HUD", year = 2023, qtr = 5, target = "COUNTY", query = "63139"),
    "`qtr` must be between", fixed = TRUE
  )
  expect_error(
    zi_load_crosswalk(zip_source = "HUD", year = 2023, qtr = 0, target = "COUNTY", query = "63139"),
    "`qtr` must be between", fixed = TRUE
  )
})

test_that("HUD invalid target triggers appropriate error", {
  expect_error(
    zi_load_crosswalk(zip_source = "HUD", year = 2023, qtr = 1, target = "INVALID", query = "63139"),
    "`target` is invalid", fixed = TRUE
  )
  expect_error(
    zi_load_crosswalk(zip_source = "HUD", year = 2023, qtr = 1, target = "ZIP", query = "63139"),
    "`target` is invalid", fixed = TRUE
  )
})

test_that("HUD valid targets are accepted without target error", {
  valid_targets <- c("TRACT", "COUNTY", "CBSA", "CBSADIV", "CD", "COUNTYSUB")
  for (tgt in valid_targets) {
    # These should pass target validation — any error thrown should NOT be about target
    err <- tryCatch(
      zi_load_crosswalk(zip_source = "HUD", year = 2023, qtr = 1, target = tgt, query = "63139"),
      error = function(e) conditionMessage(e)
    )
    # If an error occurs, it should be about the API key or something downstream, not target
    if (is.character(err)) {
      expect_false(grepl("`target` is invalid", err, fixed = TRUE),
                   info = paste("target =", tgt, "errored with target validation"))
    }
  }
})

test_that("HUD missing query triggers appropriate error", {
  expect_error(
    zi_load_crosswalk(zip_source = "HUD", year = 2023, qtr = 1, target = "COUNTY", query = NULL),
    "`query` is required", fixed = TRUE
  )
})

# test UDS input validation ------------------------------------------------

test_that("UDS year out of range triggers appropriate error", {
  expect_error(
    zi_load_crosswalk(zip_source = "UDS", year = 2008),
    "`year` must be between", fixed = TRUE
  )
  expect_error(
    zi_load_crosswalk(zip_source = "UDS", year = 2023),
    "`year` must be between", fixed = TRUE
  )
})

# test UDS crosswalk loading (no network required — uses bundled data) ---------

test_that("2015 UDS crosswalk loads successfully with normalized columns", {
  result <- zi_load_crosswalk(zip_source = "UDS", year = 2015)
  expect_s3_class(result, "tbl_df")
  expect_true("ZIP" %in% names(result))
  expect_true("ZCTA" %in% names(result))
  expect_true(nrow(result) > 0)
  expect_true(all(nchar(result$ZIP) == 5))
  expect_true(all(nchar(result$ZCTA) == 5))
})

test_that("first and last UDS years load from bundled data", {
  first <- zi_load_crosswalk(zip_source = "UDS", year = 2009)
  expect_s3_class(first, "tbl_df")
  expect_gt(nrow(first), 0)

  last <- zi_load_crosswalk(zip_source = "UDS", year = 2022)
  expect_s3_class(last, "tbl_df")
  expect_gt(nrow(last), 0)
})
