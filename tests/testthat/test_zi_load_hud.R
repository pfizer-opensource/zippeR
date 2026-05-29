# test zi_load_hud() internal validation ------------------------------------------------

# test API key validation ------------------------------------------------

test_that("missing API key triggers appropriate error", {
  withr::with_envvar(c("hud_key" = ""), {
    expect_error(
      zippeR:::zi_load_hud(year = 2023, qtr = 1, target = "COUNTY", queries = "MO", key = NULL),
      "valid HUD API key", fixed = TRUE
    )
  })
})

# test year/quarter restrictions for state abbreviations ------------------------------------------------

test_that("state abbreviation queries before 2021 trigger appropriate error", {
  withr::with_envvar(c("hud_key" = "fake_key_for_testing"), {
    expect_error(
      zippeR:::zi_load_hud(year = 2020, qtr = 4, target = "COUNTY", queries = "MO", key = "fake_key"),
      "only available from the first quarter", fixed = TRUE
    )
    expect_error(
      zippeR:::zi_load_hud(year = 2019, qtr = 1, target = "TRACT", queries = "ALL", key = "fake_key"),
      "only available from the first quarter", fixed = TRUE
    )
  })
})

# test CBSADIV availability restrictions ------------------------------------------------

test_that("CBSADIV before Q4 2017 triggers appropriate error", {
  expect_error(
    zippeR:::zi_load_hud(year = 2016, qtr = 4, target = "CBSADIV", queries = "63139", key = "fake_key"),
    "CBSADIV", fixed = TRUE
  )
  expect_error(
    zippeR:::zi_load_hud(year = 2017, qtr = 3, target = "CBSADIV", queries = "63139", key = "fake_key"),
    "CBSADIV", fixed = TRUE
  )
})

# test COUNTYSUB availability restrictions ------------------------------------------------

test_that("COUNTYSUB before Q2 2018 triggers appropriate error", {
  expect_error(
    zippeR:::zi_load_hud(year = 2017, qtr = 4, target = "COUNTYSUB", queries = "63139", key = "fake_key"),
    "COUNTYSUB", fixed = TRUE
  )
  expect_error(
    zippeR:::zi_load_hud(year = 2018, qtr = 1, target = "COUNTYSUB", queries = "63139", key = "fake_key"),
    "COUNTYSUB", fixed = TRUE
  )
})

# test query format validation ------------------------------------------------

test_that("invalid query format triggers appropriate error", {
  expect_error(
    zippeR:::zi_load_hud(year = 2023, qtr = 1, target = "COUNTY", queries = "INVALID", key = "fake_key"),
    "must be a state abbreviation", fixed = TRUE
  )
  expect_error(
    zippeR:::zi_load_hud(year = 2023, qtr = 1, target = "TRACT", queries = "ABC", key = "fake_key"),
    "must be a state abbreviation", fixed = TRUE
  )
})
