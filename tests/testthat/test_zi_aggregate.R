# fixture data (no API calls) ------------------------------------------------

correct_year <- 2010
correct_survey <- "acs5"
incorrect_year <- "ham"
incorrect_year_2 <- 2009
incorrect_survey <- c("sf1", "sf3")
incorrect_survey_2 <- c("sf2")
dec_year <- 2011

# Decennial-style fixture with extra NAME column (4 columns) — triggers the
# "three columns" validation error for sf1 since sf1 expects exactly 3
age10 <- tibble::tibble(
  GEOID = rep(c("01", "02", "04"), each = 1),
  NAME = c("Alabama", "Alaska", "Arizona"),
  variable = rep("P013001", 3),
  value = c(36.8, 33.8, 35.9)
)

# ACS-style fixture (4 columns: GEOID, variable, estimate, moe)
age11 <- tibble::tibble(
  GEOID = rep(c("01", "02", "04"), each = 1),
  variable = rep("P013001", 3),
  estimate = c(36.8, 33.8, 35.9),
  moe = c(0.1, 0.1, 0.1)
)

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
# age10 has 4 cols but wrong names for ACS (expects GEOID, variable, estimate, moe)
  expect_error(zi_aggregate(year = correct_year, survey = "acs1", .data = age10),
               "Input data appear to be malformed - there should be four columns", fixed = TRUE)
  expect_error(zi_aggregate(year = correct_year, survey = "acs1", zcta = 7613, .data = age11),
               "`zcta` contains invalid ZCTA values.", fixed = TRUE)
})

# test outputs (using package sample data) -----------------------------------

test_that("zi_aggregate produces correct tidy output with extensive variable", {
  result <- zi_aggregate(zi_mo_pop, year = 2020, extensive = "B01003_001",
                         survey = "acs5", zcta = c("630", "631"))
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("ZCTA3", "variable", "estimate", "moe") %in% names(result)))
  expect_true(all(result$ZCTA3 %in% c("630", "631")))
  expect_true("B01003_001" %in% result$variable)
})

test_that("zi_aggregate produces correct wide output", {
  result <- zi_aggregate(zi_mo_pop, year = 2020,
                         extensive = "B01003_001",
                         survey = "acs5", zcta = c("630", "631"), output = "wide")
  expect_s3_class(result, "tbl_df")
  expect_true("ZCTA3" %in% names(result))
  expect_false("variable" %in% names(result))
})

# integration tests (require Census API key) ---------------------------------

test_that("zi_aggregate works with live Census data", {
  skip_on_cran()
  skip_if(Sys.getenv("CENSUS_API_KEY") == "",
          "Census API key not available")

  vt <- tidycensus::get_acs(
    geography = "county",
    variables = c(medincome = "B19013_001"),
    state = "VT",
    year = 2020
  ) |> dplyr::select(-NAME)

  expect_error(
    zi_aggregate(year = 2020, survey = correct_survey,
                 extensive = "medincome", .data = vt),
    NA
  )
})
