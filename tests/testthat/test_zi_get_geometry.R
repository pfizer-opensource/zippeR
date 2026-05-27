context("test zi_get_geometry function")

# create test data ------------------------------------------------

chr_year <- "2010"
incorrect_year <- 2009
correct_year <- 2011

# test errors ------------------------------------------------

test_that("incorrectly specified parameters trigger appropriate errors", {
  expect_error(zi_get_geometry(year = chr_year, method = 'centroid'),
               "`year` must be numeric", fixed = TRUE)
  expect_error(zi_get_geometry(year = incorrect_year, method = 'centroid'),
               "`year` must be between", fixed = TRUE)
  expect_error(zi_get_geometry(year = correct_year, style = "zcta", method = 'centroid'),
               "`style` must be", fixed = TRUE)
  expect_error(zi_get_geometry(year = correct_year, return = "ham", method = 'centroid'),
               "`return` must be", fixed = TRUE)
  expect_warning(try(zi_get_geometry(year = correct_year, return = "full", style = "zcta3", method = 'centroid', shift_geo = 3), silent = TRUE),
               "`return` cannot be", fixed = TRUE)
  expect_warning(try(zi_get_geometry(year = correct_year, style = "zcta3", cb = TRUE, method = "intersect", shift_geo = 3), silent = TRUE),
               "`cb` does not apply", fixed = TRUE)
  expect_error(zi_get_geometry(year = correct_year, shift_geo = 3, method = "intersect"),
               "`shift_geo` must be", fixed = TRUE)
  expect_error(zi_get_geometry(year = correct_year, shift_geo = TRUE, state = 'WA', method = "intersect"),
               "`shift_geo` can only be used", fixed = TRUE)
  expect_error(zi_get_geometry(year = correct_year, state = c("AS", "GU"), method = 'centroid'),
               "Territories must be supplied with `territory`", fixed = TRUE)
  expect_error(zi_get_geometry(year = correct_year, county = "TARRANT", method = "intersect"),
               "`state` is required when `county` is supplied", fixed = TRUE)
  expect_error(zi_get_geometry(year = correct_year, state = 'WA'),
               "`method` is required", fixed = TRUE)
  expect_error(zi_get_geometry(year = correct_year, method = 'ham'),
               "`method` must be", fixed = TRUE)
  expect_error(zi_get_geometry(year = correct_year, method = "intersect", territory = c("GI")),
               "`territory` contains an invalid value", fixed = TRUE)
  expect_error(zi_get_geometry(year = correct_year, method = "centroid", starts_with = 63),
               "`starts_with` must be a character vector of two-digit values.", fixed = TRUE)
  expect_error(zi_get_geometry(year = correct_year, method = "intersect", includes = 10603),
               "`includes` contains invalid ZCTA values.", fixed = TRUE)
  expect_error(zi_get_geometry(year = correct_year, method = "centroid", excludes = "ham"),
               "`excludes` contains invalid ZCTA values.", fixed = TRUE)
})


# test successful execution ------------------------------------------------

test_that("incorrectly specified parameters trigger appropriate errors", {
  skip_on_cran()
  expect_error(zi_get_geometry(year = 2020, method = "centroid"), NA)
})
