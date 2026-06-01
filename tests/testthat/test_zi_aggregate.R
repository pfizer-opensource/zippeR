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

# test internal helpers: decennial extensive ----------------------------------

# Decennial fixture: 6 ZCTAs across 2 ZCTA3s, 2 variables (one extensive, one intensive)
dec_fixture <- tibble::tibble(
  ZCTA3 = rep(c("630", "630", "630", "631", "631", "631"), 2),
  GEOID = rep(c("63001", "63002", "63003", "63101", "63102", "63103"), 2),
  variable = rep(c("P001001", "P013001"), each = 6),
  value = c(
    # P001001 (total pop - extensive): 630 -> 100+200+300=600; 631 -> 150+250+350=750
    100, 200, 300, 150, 250, 350,
    # P013001 (median age - intensive): individual values
    35.2, 38.1, 42.0, 29.5, 33.7, 40.1
  )
)

# Pre-computed weights for decennial (population proportions within ZCTA3)
dec_weights <- tibble::tibble(
  ZCTA3 = c("630", "630", "630", "631", "631", "631"),
  GEOID = c("63001", "63002", "63003", "63101", "63102", "63103"),
  weight = c(100 / 600, 200 / 600, 300 / 600, 150 / 750, 250 / 750, 350 / 750)
)

test_that("zi_census_extensive sums values by ZCTA3 and variable", {
  input <- dplyr::filter(dec_fixture, variable == "P001001")
  result <- zi_census_extensive(input)

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("ZCTA3", "variable", "value") %in% names(result)))

  r630 <- dplyr::filter(result, ZCTA3 == "630")
  r631 <- dplyr::filter(result, ZCTA3 == "631")

  expect_equal(r630$value, 600)
  expect_equal(r631$value, 750)
})

test_that("zi_census_intensive computes weighted mean correctly", {
  input <- dplyr::filter(dec_fixture, variable == "P013001")
  result <- zi_census_intensive(input, weights = dec_weights, method = "mean")

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("ZCTA3", "variable", "value") %in% names(result)))

  # expected weighted mean for 630: (35.2*100 + 38.1*200 + 42.0*300) / 600
  expected_630 <- stats::weighted.mean(c(35.2, 38.1, 42.0), c(100 / 600, 200 / 600, 300 / 600))
  r630 <- dplyr::filter(result, ZCTA3 == "630")
  expect_equal(r630$value, expected_630, tolerance = 1e-6)

  # expected weighted mean for 631: (29.5*150 + 33.7*250 + 40.1*350) / 750
  expected_631 <- stats::weighted.mean(c(29.5, 33.7, 40.1), c(150 / 750, 250 / 750, 350 / 750))
  r631 <- dplyr::filter(result, ZCTA3 == "631")
  expect_equal(r631$value, expected_631, tolerance = 1e-6)
})

test_that("zi_census_intensive computes weighted median correctly", {
  input <- dplyr::filter(dec_fixture, variable == "P013001")
  result <- zi_census_intensive(input, weights = dec_weights, method = "median")

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("ZCTA3", "variable", "value") %in% names(result)))

  # weighted median for 630:
  # sorted x = c(35.2, 38.1, 42.0), cumsum(w)/sum(w) = c(1/6, 1/2, 1)
  # first index where cumsum >= 0.5 is index 2 → value 38.1
  expected_630 <- 38.1
  r630 <- dplyr::filter(result, ZCTA3 == "630")
  expect_equal(r630$value, expected_630, tolerance = 1e-6)
})

# test internal helpers: ACS extensive ----------------------------------------

# ACS fixture: 6 ZCTAs across 2 ZCTA3s, 2 variables
acs_fixture <- tibble::tibble(
  ZCTA3 = rep(c("630", "630", "630", "631", "631", "631"), 2),
  GEOID = rep(c("63001", "63002", "63003", "63101", "63102", "63103"), 2),
  variable = rep(c("B01003_001", "B19013_001"), each = 6),
  estimate = c(
    # B01003_001 (total pop): 630 -> 1000+2000+3000=6000; 631 -> 1500+2500+3500=7500
    1000, 2000, 3000, 1500, 2500, 3500,
    # B19013_001 (median income): individual values
    55000, 62000, 48000, 71000, 58000, 45000
  ),
  moe = c(
    # B01003_001 MOEs
    100, 150, 200, 120, 180, 220,
    # B19013_001 MOEs
    5000, 6000, 4500, 7000, 5500, 4000
  )
)

# Pre-computed ACS weights
acs_weights <- tibble::tibble(
  ZCTA3 = c("630", "630", "630", "631", "631", "631"),
  GEOID = c("63001", "63002", "63003", "63101", "63102", "63103"),
  weight = c(1000 / 6000, 2000 / 6000, 3000 / 6000, 1500 / 7500, 2500 / 7500, 3500 / 7500)
)

test_that("zi_acs_extensive sums estimates and propagates MOE correctly", {
  input <- dplyr::filter(acs_fixture, variable == "B01003_001")
  result <- zi_acs_extensive(input)

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("ZCTA3", "variable", "estimate", "moe") %in% names(result)))

  r630 <- dplyr::filter(result, ZCTA3 == "630")
  expect_equal(r630$estimate, 6000)
  # MOE = sqrt(100^2 + 150^2 + 200^2)
  expect_equal(r630$moe, sqrt(100^2 + 150^2 + 200^2), tolerance = 1e-6)

  r631 <- dplyr::filter(result, ZCTA3 == "631")
  expect_equal(r631$estimate, 7500)
  expect_equal(r631$moe, sqrt(120^2 + 180^2 + 220^2), tolerance = 1e-6)
})

test_that("zi_acs_intensive computes weighted mean for estimate and moe", {
  input <- dplyr::filter(acs_fixture, variable == "B19013_001")
  result <- zi_acs_intensive(input, weights = acs_weights, method = "mean")

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("ZCTA3", "variable", "estimate", "moe") %in% names(result)))

  # expected weighted mean estimate for 630
  w630 <- c(1000 / 6000, 2000 / 6000, 3000 / 6000)
  expected_est_630 <- stats::weighted.mean(c(55000, 62000, 48000), w630)
  expected_moe_630 <- stats::weighted.mean(c(5000, 6000, 4500), w630)

  r630 <- dplyr::filter(result, ZCTA3 == "630")
  expect_equal(r630$estimate, expected_est_630, tolerance = 1e-6)
  expect_equal(r630$moe, expected_moe_630, tolerance = 1e-6)
})

test_that("zi_acs_intensive computes weighted median for estimate and moe", {
  input <- dplyr::filter(acs_fixture, variable == "B19013_001")
  result <- zi_acs_intensive(input, weights = acs_weights, method = "median")

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("ZCTA3", "variable", "estimate", "moe") %in% names(result)))

  # expected weighted median estimate for 630:
  # sorted est = c(48000, 55000, 62000), sorted w = c(3/6, 1/6, 2/6),
  # cumsum/sum = c(0.5, 0.667, 1.0) → first >= 0.5 is index 1 → 48000
  # sorted moe = c(4500, 5000, 6000), same w sort → first >= 0.5 is index 1 → 4500
  expected_est_630 <- 48000
  expected_moe_630 <- 4500

  r630 <- dplyr::filter(result, ZCTA3 == "630")
  expect_equal(r630$estimate, expected_est_630, tolerance = 1e-6)
  expect_equal(r630$moe, expected_moe_630, tolerance = 1e-6)
})

# test zi_aggregate: decennial path ------------------------------------------

test_that("zi_aggregate works with decennial extensive data", {
  # Decennial fixture: 3 columns (GEOID, variable, value)
  dec_input <- tibble::tibble(
    GEOID = c("63001", "63002", "63003", "63101", "63102", "63103"),
    variable = rep("P001001", 6),
    value = c(100, 200, 300, 150, 250, 350)
  )

  result <- zi_aggregate(dec_input, year = 2010, extensive = "P001001",
                         survey = "sf1", zcta = c("630", "631"))

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("ZCTA3", "variable", "value") %in% names(result)))
  expect_equal(nrow(result), 2)

  r630 <- dplyr::filter(result, ZCTA3 == "630")
  expect_equal(r630$value, 600)

  r631 <- dplyr::filter(result, ZCTA3 == "631")
  expect_equal(r631$value, 750)
})

test_that("zi_aggregate decennial wide output pivots correctly", {
  dec_input <- tibble::tibble(
    GEOID = rep(c("63001", "63002", "63101", "63102"), 2),
    variable = rep(c("P001001", "P002001"), each = 4),
    value = c(100, 200, 150, 250, 50, 80, 70, 90)
  )

  result <- zi_aggregate(dec_input, year = 2010, extensive = c("P001001", "P002001"),
                         survey = "sf1", output = "wide")

  expect_s3_class(result, "tbl_df")
  expect_true("ZCTA3" %in% names(result))
  expect_true("P001001" %in% names(result))
  expect_true("P002001" %in% names(result))
  expect_false("variable" %in% names(result))
})

# test zi_aggregate: ACS mixed extensive + intensive (requires API) -----------

test_that("zi_aggregate works with mixed extensive + intensive variables", {
  skip_if_no_integration()
  skip_if(Sys.getenv("CENSUS_API_KEY") == "",
          "Census API key not available")

  result <- zi_aggregate(zi_mo_pop, year = 2020,
                         extensive = "B01003_001",
                         intensive = "B19013_001",
                         survey = "acs5", zcta = c("630", "631"))

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("ZCTA3", "variable", "estimate", "moe") %in% names(result)))
  expect_true("B01003_001" %in% result$variable)
  expect_true("B19013_001" %in% result$variable)
  expect_true(all(result$ZCTA3 %in% c("630", "631")))
})

test_that("zi_aggregate works with intensive only and method mean", {
  skip_if_no_integration()
  skip_if(Sys.getenv("CENSUS_API_KEY") == "",
          "Census API key not available")

  result <- zi_aggregate(zi_mo_pop, year = 2020,
                         intensive = "B19013_001",
                         intensive_method = "mean",
                         survey = "acs5", zcta = c("630", "631"))

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("ZCTA3", "variable", "estimate", "moe") %in% names(result)))
  expect_true("B19013_001" %in% result$variable)
})

test_that("zi_aggregate works with intensive only and method median", {
  skip_if_no_integration()
  skip_if(Sys.getenv("CENSUS_API_KEY") == "",
          "Census API key not available")

  result <- zi_aggregate(zi_mo_pop, year = 2020,
                         intensive = "B19013_001",
                         intensive_method = "median",
                         survey = "acs5", zcta = c("630", "631"))

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("ZCTA3", "variable", "estimate", "moe") %in% names(result)))
  expect_true("B19013_001" %in% result$variable)
})

test_that("zi_aggregate mixed output in wide format has correct columns", {
  skip_if_no_integration()
  skip_if(Sys.getenv("CENSUS_API_KEY") == "",
          "Census API key not available")

  result <- zi_aggregate(zi_mo_pop, year = 2020,
                         extensive = "B01003_001",
                         intensive = "B19013_001",
                         survey = "acs5", zcta = c("630", "631"),
                         output = "wide")

  expect_s3_class(result, "tbl_df")
  expect_true("ZCTA3" %in% names(result))
  expect_false("variable" %in% names(result))
  # wide ACS format: variable + E/M suffix
  expect_true(any(grepl("B01003_001", names(result))))
  expect_true(any(grepl("B19013_001", names(result))))
})

# integration tests (require Census API key) ---------------------------------

test_that("zi_aggregate works with live Census data", {
  skip_if_no_integration()
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
