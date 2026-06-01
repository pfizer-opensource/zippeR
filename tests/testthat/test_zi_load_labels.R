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
  expect_warning(zi_load_labels(source = "UDS", type = "zip5", include_scf = TRUE, vintage = 2022),
                 "include_scf", fixed = TRUE)
})

# test positive-path assertions ------------------------------------------------

test_that("UDS zip5 returns expected schema", {
  result <- zi_load_labels(source = "UDS", type = "zip5", vintage = 2022)
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("zip5", "label_city", "label_state") %in% names(result)))
  expect_gt(nrow(result), 30000)
  expect_type(result$zip5, "character")
  expect_true(all(nchar(result$zip5) == 5))
  expect_type(result$label_city, "character")
  expect_type(result$label_state, "character")
})

test_that("USPS zip3 returns expected schema", {
  skip_if_no_integration()
  result <- zi_load_labels(source = "USPS", type = "zip3", vintage = 202408)
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("zip3", "label_area", "label_state") %in% names(result)))
  expect_gt(nrow(result), 800)
  expect_type(result$zip3, "character")
  expect_true(all(nchar(result$zip3) == 3))
  expect_type(result$label_area, "character")
  expect_type(result$label_state, "character")
})

test_that("USPS zip3 with include_scf adds SCF columns", {
  skip_if_no_integration()
  result <- zi_load_labels(source = "USPS", type = "zip3", vintage = 202408,
                           include_scf = TRUE)
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("scf_name", "scf_state", "scf_id") %in% names(result)))
  expect_gt(nrow(result), 800)
})
