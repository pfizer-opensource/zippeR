context("test zi_get_geometry function")

# create test data ------------------------------------------------

chr_year <- "2010"
incorrect_year <- 2009
correct_year <- 2011

# test errors ------------------------------------------------

test_that("incorrectly specified parameters trigger appropriate errors", {
  expect_error(zi_get_geometry(year = chr_year, method = 'centroid'),
               "The 'year' value provided is invalid. Please provide a numeric value between years 2010 and 2023.")
  expect_error(zi_get_geometry(year = incorrect_year, method = 'centroid'),
               "The 'year' value provided is invalid. Please provide a year between 2010 and 2023.")
  expect_error(zi_get_geometry(year = correct_year, style = "zcta", method = 'centroid'),
               "The 'style' value provided is invalid. Please select either 'zcta5' or 'zcta3'.")
  expect_error(zi_get_geometry(year = correct_year, return = "ham", method = 'centroid'),
               "The 'return' value provided is invalid. Please select either 'id' or 'full'.")
  expect_warning(zi_get_geometry(year = correct_year, return = "full", style = "zcta3", method = 'centroid'),
               "The 'full' option for 'return' is not available for 'zcta3' data. Please use 'id' instead.")
  expect_warning(zi_get_geometry(year = correct_year, style = "zcta3", cb = TRUE, method = "intersect"),
               "The 'cb' argument does not apply to 'zcta3' data.")
  expect_error(zi_get_geometry(year = correct_year, shift_geo = 3, method = "intersect"),
               "The 'shift_geo' value provided is invalid. Please select either 'TRUE' or 'FALSE'.")
  expect_error(zi_get_geometry(year = correct_year, shift_geo = TRUE, state = 'WA', method = "intersect"),
               "The 'shift_geo' functionality can only be used when you are returning data for all states.")
  expect_error(zi_get_geometry(year = correct_year, state = c("AS", "GU"), method = 'centroid'),
               "Please specify territories using the 'territory' argument instead. ")
  expect_error(zi_get_geometry(year = correct_year, county = "TARRANT", method = "intersect"),
               "Please provide at least one state abbreviation or FIPS code for the 'state' argument that corresponds to data passed to the 'county' argument.")
  expect_error(zi_get_geometry(year = correct_year, state = 'WA'),
               "Please select a valid method for returning ZCTA values. Your choices are 'centroid' and 'intersect'. See documentation for details.")
  expect_error(zi_get_geometry(year = correct_year, method = 'ham'),
               "The two valid methods for returning ZCTA values are 'centroid' and 'intersect'. See documentation for details.")
  expect_error(zi_get_geometry(year = correct_year, method = "intersect", territory = c("GI")),
               "An abbreviation given for the 'territory' argument is invalid. ")
  expect_error(zi_get_geometry(year = correct_year, method = "centroid", starts_with = 63),
               "ZCTA data passed to the 'starts_with' argument are invalid. Please use a character vector with only two-digit values.")
  expect_error(zi_get_geometry(year = correct_year, method = "intersect", includes = 10603),
               "ZCTA data passed to the 'includes' argument are invalid. ")
  expect_error(zi_get_geometry(year = correct_year, method = "centroid", excludes = "ham"),
               "ZCTA data passed to the 'excludes' argument are invalid. ")
})


# test successful execution ------------------------------------------------

test_that("incorrectly specified parameters trigger appropriate errors", {
  skip_on_cran()
  expect_error(zi_get_geometry(year = correct_year, method = "centroid"), NA)
})
