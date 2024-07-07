context("test zi_aggregate function")

# create test data ------------------------------------------------

correct_year = 2010
correct_survey = "acs5"
incorrect_year <- "ham"
incorrect_year_2 <- 2009
incorrect_survey <- c("sf1", "sf3")
incorrect_survey_2 <- c("sf2")
dec_year <- 2011

age10 <- tidycensus::get_decennial(geography = "state",
                       variables = "P013001",
                       year = 2010)

age11 <- age10 %>% dplyr::rename(estimate = value, moe = NAME) %>% dplyr::select("GEOID", "variable", "estimate", "moe")


vt <-tidycensus:: get_acs(geography = "county",
              variables = c(medincome = "B19013_001"),
              state = "VT",
              year = 2020) %>%
  dplyr:: select(-NAME)

# test errors ------------------------------------------------

test_that("missing parameters trigger appropriate errors", {
  expect_error(zi_aggregate(),
               "The 'year' value is missing. Please provide a numeric value between 2010 and 2022.")
})

test_that("incorrectly specified parameters trigger appropriate errors", {
  expect_error(zi_aggregate(year = incorrect_year, survey = correct_survey),
               "The 'year' value provided is invalid. Please provide a numeric value between 2010 and 2022.")
  expect_error(zi_aggregate(year = correct_year, survey = incorrect_survey),
               "One only 'survey' product may be requested at a time.")
  expect_error(zi_aggregate(year = correct_year, survey = incorrect_survey_2),
               "The 'survey' requested is invalid. Please choose one of 'sf1', 'sf3', 'acs1', 'acs3', or 'acs5'.")
  expect_error(zi_aggregate(survey = "sf1", year = dec_year),
               "The 'year' value provided is invalid for Decennial Census data. Only 2010 may be requested currently.")
  expect_error(zi_aggregate(survey = "acs1", year = incorrect_year_2),
               "The 'year' value provided is invalid for 1- or 5-year American Community Survey data. Please provide a year between 2010 and 2022.")
  expect_error(zi_aggregate(survey = "acs3", year = 2014),
               "The 'year' value provided is invalid for 3-year American Community Survey data. Please provide a year between 2010 and 2013.")
  expect_error(zi_aggregate(year = correct_year, survey = correct_survey, output = "tidi"),
               "The 'output' requested is invalid. Please choose one of 'tidy' or 'wide'.")
  expect_error(zi_aggregate(year = correct_year, survey = "sf1", .data = age10),
               "Input data appear to be malformed - there should be three columns for Decennial Census data: 'GEOID', 'variable', and 'value'. Note that zi_aggregate() only accepts 'tidy' data.", fixed = TRUE)
  expect_error(zi_aggregate(year = correct_year, survey = "acs1", .data = age10),
               "Input data appear to be malformed - there should be four columns for ACS data: 'GEOID', 'variable', 'estimate', and 'moe'. Note that zi_aggregate() only accepts 'tidy' data.", fixed = TRUE)
  expect_error(zi_aggregate(year = correct_year, survey = "acs1", zcta = 7613, .data = age11),
               "ZCTA data passed to the 'zcta' argument are invalid. Please use 'zi_validate()' with the 'verbose = TRUE' option to investgiate further. The 'zi_repair()' function may be used to address issues.", fixed = TRUE)
})

# test inputs ------------------------------------------------


# # giving error that object out not found
# test_that("correctly specified functions execute without error", {
#   expect_error(zi_aggregate(year= 2020, survey = correct_survey, .data = vt ), NA)
#   expect_error(zi_aggregate(year= 2020, survey = correct_survey, zcta = c("056"), .data = vt), NA)
# })

# test outputs ------------------------------------------------
