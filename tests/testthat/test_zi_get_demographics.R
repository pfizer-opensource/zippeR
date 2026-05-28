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
               "`year` is required", fixed = TRUE)
})

test_that("incorrectly specified parameters trigger appropriate errors", {
  expect_error(zi_get_demographics(year = incorrect_year, survey = correct_survey),
               "`year` must be numeric", fixed = TRUE)
  expect_error(zi_get_demographics(year = correct_year, survey = incorrect_survey),
               "`survey` must contain a single value", fixed = TRUE)
  expect_error(zi_get_demographics(year = correct_year, survey = incorrect_survey_2),
               "`survey` must be one of", fixed = TRUE)
  expect_error(zi_get_demographics(survey = "sf1", year = dec_year),
               "Decennial Census data", fixed = TRUE)
  expect_error(zi_get_demographics(survey = "acs1", year = incorrect_year_2),
               "`year` must be between", fixed = TRUE)
  expect_error(zi_get_demographics(survey = "acs3", year = 2014),
               "`year` must be between", fixed = TRUE)
  expect_error(zi_get_demographics(year = correct_year, survey = correct_survey, output = "tidi"),
               "`output` must be", fixed = TRUE)
  expect_error(zi_get_demographics(year = correct_year, survey = correct_survey, variables = c(medincome = "B19013_001"), table = "acs1"),
               "`variables` and `table` cannot be used together", fixed = TRUE)
  expect_error(zi_get_demographics(year = correct_year, survey = "acs1", zcta = 7613),
               "`zcta` contains invalid ZCTA values.", fixed = TRUE)
})



# test inputs ------------------------------------------------

test_that("correctly specified functions execute without error", {
  skip_on_cran()
  skip_if_offline()
  skip_if(nchar(Sys.getenv("CENSUS_API_KEY")) == 0, "Census API key not available")
  expect_no_error(zi_get_demographics(year = 2012, survey = "acs5", variables = c(pop = "B01003_001")))
})
