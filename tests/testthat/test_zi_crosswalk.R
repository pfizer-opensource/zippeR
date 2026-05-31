# create test data ------------------------------------------------

df_data_bad <- data.frame(
  bad_zip1 = c(1:4),
  bad_zip2 = c("63088", "63108", "631399", "ham")
)

df_data_good <- data.frame(
  id = c(1:3),
  good_zip = c("63088", "63108", "63139")
)

hud_dict <- zi_prep_hud(zi_mo_hud, by = "residential")

# test data validation ------------------------------------------------

test_that("invalid data trigger appropriate errors", {
  expect_error(zi_crosswalk(df_data_bad, input_var = bad_zip1, zip_source = hud_dict, source_var = zip5, source_result = geoid),
               "Input ZIP Code data in `bad_zip1` are invalid.", fixed = TRUE)
  expect_error(zi_crosswalk(df_data_bad, input_var = bad_zip2, zip_source = hud_dict, source_var = zip5, source_result = geoid),
               "Input ZIP Code data in `bad_zip2` are invalid.", fixed = TRUE)
})

# test errors ------------------------------------------------

test_that("invalid zip_source triggers appropriate error", {
  expect_error(
    zi_crosswalk(df_data_good, input_var = good_zip, zip_source = "INVALID"),
    "`zip_source` must be", fixed = TRUE
  )
})

test_that("non-data-frame .data triggers appropriate error", {
  expect_error(
    zi_crosswalk("not a data frame", input_var = good_zip, zip_source = "UDS", year = 2020),
    "must be a data frame"
  )
})

test_that("missing input_var triggers appropriate error", {
  expect_error(
    zi_crosswalk(df_data_good, zip_source = "UDS", year = 2020),
    "`input_var` is required", fixed = TRUE
  )
})

test_that("input_var not found in .data triggers appropriate error", {
  expect_error(
    zi_crosswalk(df_data_good, input_var = nonexistent_col, zip_source = "UDS", year = 2020),
    "was not found in", fixed = TRUE
  )
})

test_that("invalid return value triggers appropriate error", {
  expect_error(
    zi_crosswalk(df_data_good, input_var = good_zip, zip_source = hud_dict,
                 source_var = zip5, source_result = geoid, return = "invalid"),
    "`return` must be", fixed = TRUE
  )
})

# test HUD API workflow validation ------------------------------------------------

test_that("HUD workflow requires year as numeric", {
  expect_error(
    zi_crosswalk(df_data_good, input_var = good_zip, zip_source = "HUD",
                 year = "2023", qtr = 1, target = "COUNTY", query = "MO",
                 by = "residential", return_max = TRUE),
    "`year` must be numeric", fixed = TRUE
  )
})

test_that("HUD workflow validates year range", {
  expect_error(
    zi_crosswalk(df_data_good, input_var = good_zip, zip_source = "HUD",
                 year = 2009, qtr = 1, target = "COUNTY", query = "MO",
                 by = "residential", return_max = TRUE),
    "`year` must be between", fixed = TRUE
  )
})

test_that("HUD workflow validates quarter", {
  expect_error(
    zi_crosswalk(df_data_good, input_var = good_zip, zip_source = "HUD",
                 year = 2023, qtr = 5, target = "COUNTY", query = "MO",
                 by = "residential", return_max = TRUE),
    "`qtr` must be between", fixed = TRUE
  )
})

test_that("HUD workflow validates target", {
  expect_error(
    zi_crosswalk(df_data_good, input_var = good_zip, zip_source = "HUD",
                 year = 2023, qtr = 1, target = "INVALID", query = "MO",
                 by = "residential", return_max = TRUE),
    "`target` is invalid", fixed = TRUE
  )
})

test_that("HUD workflow requires query", {
  expect_error(
    zi_crosswalk(df_data_good, input_var = good_zip, zip_source = "HUD",
                 year = 2023, qtr = 1, target = "COUNTY", query = NULL,
                 by = "residential", return_max = TRUE),
    "`query` is required", fixed = TRUE
  )
})

test_that("HUD workflow requires by parameter", {
  expect_error(
    zi_crosswalk(df_data_good, input_var = good_zip, zip_source = "HUD",
                 year = 2023, qtr = 1, target = "COUNTY", query = "MO",
                 by = NULL, return_max = TRUE),
    "`by` is required", fixed = TRUE
  )
})

test_that("HUD workflow validates by parameter values", {
  expect_error(
    zi_crosswalk(df_data_good, input_var = good_zip, zip_source = "HUD",
                 year = 2023, qtr = 1, target = "COUNTY", query = "MO",
                 by = "invalid", return_max = TRUE),
    "`by` must be", fixed = TRUE
  )
})

test_that("HUD workflow validates return_max as logical", {
  expect_error(
    zi_crosswalk(df_data_good, input_var = good_zip, zip_source = "HUD",
                 year = 2023, qtr = 1, target = "COUNTY", query = "MO",
                 by = "residential", return_max = "yes"),
    "`return_max` must be", fixed = TRUE
  )
})


# test inputs ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(zi_crosswalk(df_data_good, input_var = good_zip, zip_source = hud_dict, source_var = zip5, source_result = geoid), NA)
})

# test outputs ------------------------------------------------

test_that("correctly specified functions produce expected output", {
  result <- zi_crosswalk(df_data_good, input_var = good_zip, zip_source = hud_dict, source_var = zip5, source_result = geoid)
  expect_s3_class(result, "tbl_df")
  expect_true("source_geoid" %in% names(result))
  expect_equal(nrow(result), nrow(df_data_good))
  expect_type(result$source_geoid, "character")
})

test_that("return = 'all' appends full dictionary columns", {
  result <- zi_crosswalk(df_data_good, input_var = good_zip, zip_source = hud_dict,
                         source_var = zip5, source_result = geoid, return = "all")
  expect_s3_class(result, "tbl_df")
  expect_true(any(grepl("^source_", names(result))))
  expect_true(ncol(result) > ncol(df_data_good) + 1)
})

# test HUD end-to-end via custom dictionary ------------------------------------------------

test_that("zi_crosswalk with zi_prep_hud (commercial) produces expected output", {
  hud_commercial <- zi_prep_hud(zi_mo_hud, by = "commercial")
  result <- zi_crosswalk(df_data_good, input_var = good_zip,
                         zip_source = hud_commercial, source_var = zip5, source_result = geoid)
  expect_s3_class(result, "tbl_df")
  expect_true("source_geoid" %in% names(result))
  expect_equal(nrow(result), nrow(df_data_good))
})

test_that("zi_crosswalk with zi_prep_hud (total) produces expected output", {
  hud_total <- zi_prep_hud(zi_mo_hud, by = "total")
  result <- zi_crosswalk(df_data_good, input_var = good_zip,
                         zip_source = hud_total, source_var = zip5, source_result = geoid)
  expect_s3_class(result, "tbl_df")
  expect_true("source_geoid" %in% names(result))
})

test_that("zi_crosswalk with return_max = FALSE produces multiple rows for boundary ZIPs", {
  # Use a ZIP that crosses county boundaries (63005 maps to multiple GEOIDs)
  df_boundary <- data.frame(
    id = 1L,
    zip5 = "63005"
  )
  hud_all <- zi_prep_hud(zi_mo_hud, by = "residential", return_max = FALSE)
  result <- zi_crosswalk(df_boundary, input_var = zip5,
                         zip_source = hud_all, source_var = zip5, source_result = geoid)
  expect_s3_class(result, "tbl_df")
  expect_true("source_geoid" %in% names(result))
  # With return_max = FALSE, a boundary ZIP should produce multiple rows

  expect_gt(nrow(result), 1)
})

# test deprecated parameters ------------------------------------------------

test_that("deprecated input_zip produces warning and works", {
  expect_warning(
    result <- zi_crosswalk(df_data_good, input_zip = good_zip,
                           zip_source = hud_dict, source_var = zip5,
                           source_result = geoid),
    "input_zip.*deprecated.*removed in early 2027"
  )
  expect_s3_class(result, "tbl_df")
  expect_true("source_geoid" %in% names(result))
  expect_equal(nrow(result), nrow(df_data_good))
})

test_that("deprecated dict with 'SOURCE YEAR' string produces warning and works", {
  skip_if_no_integration()
  expect_warning(
    result <- zi_crosswalk(df_data_good, input_var = good_zip,
                           dict = "UDS 2020"),
    "dict.*deprecated.*removed in early 2027"
  )
  expect_s3_class(result, "tbl_df")
  expect_true("source_zcta" %in% names(result))
  expect_equal(nrow(result), nrow(df_data_good))
})

test_that("deprecated dict with data frame produces warning and works", {
  expect_warning(
    result <- zi_crosswalk(df_data_good, input_var = good_zip,
                           dict = hud_dict, source_var = zip5,
                           source_result = geoid),
    "dict.*deprecated.*removed in early 2027"
  )
  expect_s3_class(result, "tbl_df")
  expect_true("source_geoid" %in% names(result))
  expect_equal(nrow(result), nrow(df_data_good))
})
