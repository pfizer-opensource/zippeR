# test zi_convert ------------------------------------------------

# create test data ------------------------------------------------

df_good <- data.frame(
  id = c(1:3),
  zip5 = c("63005", "63139", "63636")
)

df_bad_zip <- data.frame(
  id = c(1:3),
  zip5 = c("63005", "63139", "ham")
)

df_numeric <- data.frame(
  id = c(1:3),
  zip5 = c(63005, 63139, 63636)
)

# test errors ------------------------------------------------

test_that("missing parameters trigger appropriate errors", {
  expect_error(zi_convert(df_good),
               "is required", fixed = TRUE)
})

test_that("incorrectly specified parameters trigger appropriate errors", {
  expect_error(zi_convert(.data = "not_a_df", input_var = zip5),
               "must be a data frame", fixed = TRUE)
  expect_error(zi_convert(.data = df_good, input_var = nonexistent),
               "was not found", fixed = TRUE)
  expect_error(zi_convert(.data = df_bad_zip, input_var = zip5),
               "are invalid", fixed = TRUE)
})

# test inputs ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_no_error(zi_convert(.data = df_good, input_var = zip5))
  expect_no_error(zi_convert(.data = df_good, input_var = zip5, output_var = zip3))
})

# test outputs ------------------------------------------------

test_that("correctly specified functions produce expected classes", {
  result <- zi_convert(.data = df_good, input_var = zip5)
  expect_s3_class(result, "tbl_df")
})

test_that("conversion produces correct 3-digit values", {
  result <- zi_convert(.data = df_good, input_var = zip5)
  expect_equal(result$zip5, c("630", "631", "636"))
})

test_that("overwrite mode replaces column in place", {
  result <- zi_convert(.data = df_good, input_var = zip5)
  expect_equal(result$zip5, c("630", "631", "636"))
})

test_that("output_var creates a new column with the specified name", {
  result <- zi_convert(.data = df_good, input_var = zip5, output_var = zip3)
  expect_true("zip3" %in% names(result))
  expect_equal(result$zip3, c("630", "631", "636"))
  expect_equal(result$zip5, c("63005", "63139", "63636"))
})

test_that("output_var overwrites existing column with warning", {
  df_conflict <- data.frame(id = c(1:3), zip5 = c("63005", "63139", "63636"), zip3 = c("a", "b", "c"))
  expect_warning(zi_convert(.data = df_conflict, input_var = zip5, output_var = zip3),
                 "already exists", fixed = TRUE)
})
