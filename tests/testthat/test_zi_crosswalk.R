context("test zi_crosswalk function")

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
               "Input ZIP Code data in the 'bad_zip1' column are invalid. Please use 'zi_validate()' with the 'verbose = TRUE' option to investigate further. The 'zi_repair()' function may be used to address issues.", fixed = TRUE)
  expect_error(zi_crosswalk(df_data_bad, input_var = bad_zip2, zip_source = hud_dict, source_var = zip5, source_result = geoid),
               "Input ZIP Code data in the 'bad_zip2' column are invalid. Please use 'zi_validate()' with the 'verbose = TRUE' option to investigate further. The 'zi_repair()' function may be used to address issues.", fixed = TRUE)
})

# test errors ------------------------------------------------


# test inputs ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(zi_crosswalk(df_data_good, input_var = good_zip, zip_source = hud_dict, source_var = zip5, source_result = geoid), NA)
})

# test outputs ------------------------------------------------

