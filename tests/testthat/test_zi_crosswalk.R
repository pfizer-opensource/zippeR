context("test zi_crosswalk function")

# create test data ------------------------------------------------

incorrect_zips <- c("63088", "63108", "631399", "ham")
correct_zips <- c("63088", "63108", "63139", "63140")
correct_zcta <- c("10052", "10053", "10054", "10053")
incorrect_zcta <- c("10052", "100537", "10054", "ham")


df_data_bad <- data.frame(
  input_zip = c(1:4),
  zips = incorrect_zips
)

df_data_good <- data.frame(
  input_zip = c(1:4),
  zips = correct_zips
)

df_dict_good <- data.frame(
  ZIP = correct_zips,
  ZCTA = correct_zcta
)

df_dict_bad_zip <- data.frame(
  ZIP= incorrect_zips,
  ZCTA = correct_zcta
)

df_dict_bad_zcta <- data.frame(
  ZIP= correct_zips,
  ZCTA = incorrect_zcta
)

# test errors ------------------------------------------------

test_that("missing parameters trigger appropriate errors", {
  expect_error(zi_crosswalk(),
               "Please specify arguments.")
})

test_that("incorrectly specified parameters trigger appropriate errors", {
  expect_error(zi_crosswalk(.data = df_data_good, input_zip = zips, dict = 'UDS 2009'),
               "The 'dict' value provided is invalid. Please provide a value between 'UDS 2010' and 'UDS 2022'.")
  expect_error(zi_crosswalk(.data = df_data_good, input_zip = "ham"),
               "The given 'input_zip' column is not found in your input object.")
  expect_error(zi_crosswalk(.data = df_data_good, input_zip = zips, dict = df_dict_good, dict_zip = "ham"),
               "The given 'dict_zip' column is not found in your dictionary object.")
  expect_error(zi_crosswalk(.data = df_data_good, input_zip = zips, dict = df_dict_good, dict_zcta = "ham"),
               "The given 'dict_zcta' column is not found in your dictionary object.")
  expect_error(zi_crosswalk(.data =  df_data_bad, input_zip = input_zip),
               "Input ZIP Code data in the 'input_zip' column are invalid. Please use 'zi_validate()' with the 'verbose = TRUE' option to investigate further. The 'zi_repair()' function may be used to address issues.", fixed = TRUE)
  expect_error(zi_crosswalk(.data = df_data_good, dict = df_dict_bad_zip, input_zip = zips, dict_zip = ZIP),
               "Dictionary ZIP Code data in the ")
  expect_error(zi_crosswalk(.data =  df_data_good, input_zip = zips, dict = df_dict_bad_zcta, dict_zip = ZIP, dict_zcta = ZCTA, style = "zcta5"),
               "Dictionary ZCTA data in the ")
  expect_error(zi_crosswalk(.data =  df_data_good, input_zip = zips, dict = df_dict_bad_zcta, dict_zip = ZIP, dict_zcta = ZCTA, style = "zcta3"),
               "Dictionary ZCTA data in the ")
})

# test inputs ------------------------------------------------

# test_that("correctly specified functions execute without error", {
#  expect_error(zi_crosswalk(df_data_good, input_zip = zips, df_dict_good), NA)
#  expect_error(zi_crosswalk(df_data_good, input_zip = zips, df_dict_good, style = "zcta3"), NA)
# })

# test outputs ------------------------------------------------

test_that("correctly specified functions produce expected classes", {
  # expect_type(zi_crosswalk(df_data_good, input_zip = zips, df_dict_good), "list")
  expect_type(zi_crosswalk(df_data_good, input_zip = zips, df_dict_good, style = "zcta3"), "list")
})

# test_that("correctly specified functions produce messages", {
#  expect_message(zi_crosswalk(df_data_good, input_zip = zips, df_dict_good, style = "zcta3"), "Dictionary five-digit ZCTAs converted to three-digit ZCTAs.")
# })


result1 <- zi_crosswalk(df_data_good, input_zip = zips, df_dict_good)
result2 <- zi_crosswalk(df_data_good, input_zip = zips, df_dict_good, style = "zcta3")

test_that("correctly specified functions produce expected output", {
  expect_equivalent(result1, list(input_zip = c(1:4), zips = c("63088", "63108", "63139", "63140"), ZCTA = c("10052", "10053", "10054", "10053")))
  expect_equivalent(result2, list(input_zip = c(1:4), zips = c("63088", "63108", "63139", "63140"), ZCTA3 = c("100", "100", "100", "100")))
})
