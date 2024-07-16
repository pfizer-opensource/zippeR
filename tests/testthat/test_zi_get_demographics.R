context("test zi_get_demographics function")

# create test data ------------------------------------------------

correct_year = 2010
correct_survey = "acs5"
incorrect_year <- "ham"
incorrect_year_2 <- 2009
incorrect_survey <- c("sf1", "sf3")
incorrect_survey_2 <- c("sf2")
dec_year <- 2011

# test errors ------------------------------------------------

test_that("missing parameters trigger appropriate errors", {
  expect_error(zi_get_demographics(),
               "The 'year' value is missing. Please provide a numeric value between 2010 and 2022.")
})

test_that("incorrectly specified parameters trigger appropriate errors", {
  expect_error(zi_get_demographics(year = incorrect_year, survey = correct_survey),
               "The 'year' value provided is invalid. Please provide a numeric value between 2010 and 2022.")
  expect_error(zi_get_demographics(year = correct_year, survey = incorrect_survey),
               "One only 'survey' product may be requested at a time.")
  expect_error(zi_get_demographics(year = correct_year, survey = incorrect_survey_2),
               "The 'survey' requested is invalid. Please choose one of 'sf1', 'sf3', 'acs1', 'acs3', or 'acs5'.")
  expect_error(zi_get_demographics(survey = "sf1", year = dec_year),
               "The 'year' value provided is invalid for Decennial Census data. Only 2010 may be requested currently.")
  expect_error(zi_get_demographics(survey = "acs1", year = incorrect_year_2),
               "The 'year' value provided is invalid for 1- or 5-year American Community Survey data. Please provide a year between 2010 and 2022.")
  expect_error(zi_get_demographics(survey = "acs3", year = 2014),
               "The 'year' value provided is invalid for 3-year American Community Survey data. Please provide a year between 2010 and 2013.")
  expect_error(zi_get_demographics(year = correct_year, survey = correct_survey, output = "tidi"),
               "The 'output' requested is invalid. Please choose one of 'tidy' or 'wide'.")
  expect_error(zi_get_demographics(year = correct_year, survey = correct_survey, variables = c(medincome = "B19013_001"), table = "acs1"),
               "The 'variables' or 'table' arguments cannot be used simultaneously. Please choose one or the other.")
  expect_error(zi_get_demographics(year = correct_year, survey = "acs1", zcta = 7613),
               "ZCTA data passed to the ", fixed = TRUE)
})



# test inputs ------------------------------------------------

# # can't get this to work
# test_that("correctly specified functions execute without error", {
#   expect_error(zi_get_demographics(year = 2011, survey = "acs1", variables = c(medincome = "B01003_001")), NA)
# })

# test outputs ------------------------------------------------
