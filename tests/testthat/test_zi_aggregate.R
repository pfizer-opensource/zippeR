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
               "`year` is required", fixed = TRUE)
})

test_that("incorrectly specified parameters trigger appropriate errors", {
  expect_error(zi_aggregate(year = incorrect_year, survey = correct_survey),
               "`year` must be numeric", fixed = TRUE)
  expect_error(zi_aggregate(year = correct_year, survey = incorrect_survey),
               "`survey` must contain a single value", fixed = TRUE)
  expect_error(zi_aggregate(year = correct_year, survey = incorrect_survey_2),
               "`survey` must be one of", fixed = TRUE)
  expect_error(zi_aggregate(survey = "sf1", year = dec_year),
               "Decennial Census data", fixed = TRUE)
  expect_error(zi_aggregate(survey = "acs1", year = incorrect_year_2),
               "`year` must be between", fixed = TRUE)
  expect_error(zi_aggregate(survey = "acs3", year = 2014),
               "`year` must be between", fixed = TRUE)
  expect_error(zi_aggregate(year = correct_year, survey = correct_survey, output = "tidi"),
               "`output` must be", fixed = TRUE)
  expect_error(zi_aggregate(year = correct_year, survey = "sf1", .data = age10),
               "Input data appear to be malformed - there should be three columns", fixed = TRUE)
  expect_error(zi_aggregate(year = correct_year, survey = "acs1", .data = age10),
               "Input data appear to be malformed - there should be four columns", fixed = TRUE)
  expect_error(zi_aggregate(year = correct_year, survey = "acs1", zcta = 7613, .data = age11),
               "`zcta` contains invalid ZCTA values.", fixed = TRUE)
})

# test inputs ------------------------------------------------


# # giving error that object out not found
# test_that("correctly specified functions execute without error", {
#   expect_error(zi_aggregate(year= 2020, survey = correct_survey, .data = vt ), NA)
#   expect_error(zi_aggregate(year= 2020, survey = correct_survey, zcta = c("056"), .data = vt), NA)
# })

# test outputs ------------------------------------------------
