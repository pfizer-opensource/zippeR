# test zi_label ------------------------------------------------

# create test data ------------------------------------------------

df_zip5 <- data.frame(

  id = c(1:3),
  zip5 = c("63005", "63139", "63636")
)

df_zip3 <- data.frame(
  id = c(1:3),
  zip3 = c("630", "631", "636")
)

df_bad_zip <- data.frame(
  id = c(1:3),
  zip5 = c("63005", "63139", "ham")
)

# test errors ------------------------------------------------

test_that("missing parameters trigger appropriate errors", {
  expect_error(zi_label(df_zip5),
               "is required", fixed = TRUE)
})

test_that("incorrectly specified parameters trigger appropriate errors", {
  expect_error(zi_label(.data = "not_a_df", input_var = zip5),
               "must be a data frame", fixed = TRUE)
  expect_error(zi_label(.data = df_zip5, input_var = nonexistent),
               "was not found", fixed = TRUE)
  expect_error(zi_label(.data = df_bad_zip, input_var = zip5),
               "are invalid", fixed = TRUE)
  expect_error(zi_label(.data = df_zip5, input_var = zip5, label_source = "INVALID"),
               "must be", fixed = TRUE)
  expect_error(zi_label(.data = df_zip5, input_var = zip5, label_source = "UDS", type = "bad"),
               "must be", fixed = TRUE)
  expect_error(zi_label(.data = df_zip3, input_var = zip3, label_source = "UDS", type = "zip3"),
               "must be", fixed = TRUE)
  expect_error(zi_label(.data = df_zip5, input_var = zip5, label_source = "USPS", type = "zip5"),
               "must be", fixed = TRUE)
  expect_error(zi_label(.data = df_zip5, input_var = zip5, label_source = "UDS", vintage = 2000),
               "must be between", fixed = TRUE)
})

test_that("custom dictionary requires source_var", {
  custom_dict <- data.frame(zip5 = c("63005"), city = c("Chesterfield"))
  expect_error(zi_label(df_zip5, input_var = zip5, label_source = custom_dict),
               "is required", fixed = TRUE)
})

test_that("custom dictionary source_var must exist", {
  custom_dict <- data.frame(zip5 = c("63005"), city = c("Chesterfield"))
  expect_error(zi_label(df_zip5, input_var = zip5, label_source = custom_dict, source_var = bad_col),
               "was not found", fixed = TRUE)
})

# test inputs with custom dictionary ------------------------------------------------

test_that("custom dictionary workflow executes without error", {
  mo_label <- zi_mo_usps
  expect_no_error(
    zi_label(df_zip3, input_var = zip3, label_source = mo_label, source_var = zip3, type = "zip3")
  )
})

# test outputs with custom dictionary ------------------------------------------------

test_that("custom dictionary produces expected output", {
  mo_label <- zi_mo_usps
  result <- zi_label(df_zip3, input_var = zip3, label_source = mo_label, source_var = zip3, type = "zip3")
  expect_s3_class(result, "tbl_df")
  expect_true("label_area" %in% names(result))
  expect_true("label_state" %in% names(result))
  expect_equal(nrow(result), 3)
})

# test positive-path assertions for UDS/USPS sources ------------------------------------------------

test_that("UDS zip5 label lookup returns expected schema", {
  result <- zi_label(df_zip5, input_var = zip5, label_source = "UDS",
                     type = "zip5", vintage = 2022)
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("id", "zip5", "label_city", "label_state") %in% names(result)))
  expect_equal(nrow(result), 3)
  expect_type(result$label_city, "character")
  expect_type(result$label_state, "character")
})

test_that("USPS zip3 label lookup returns expected schema", {
  skip_if_no_integration()
  result <- zi_label(df_zip3, input_var = zip3, label_source = "USPS",
                     type = "zip3", vintage = 202408)
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("id", "zip3", "label_area", "label_state") %in% names(result)))
  expect_equal(nrow(result), 3)
  expect_type(result$label_area, "character")
  expect_type(result$label_state, "character")
})

test_that("USPS zip3 with include_scf returns SCF columns", {
  skip_if_no_integration()
  result <- zi_label(df_zip3, input_var = zip3, label_source = "USPS",
                     type = "zip3", vintage = 202408, include_scf = TRUE)
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("scf_name", "scf_state", "scf_id") %in% names(result)))
  expect_equal(nrow(result), 3)
})
