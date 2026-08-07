# create test data ------------------------------------------------

chr_year <- "2010"
incorrect_year <- 2009
out_of_range_year <- 2025
correct_year <- 2011

# test errors ------------------------------------------------

test_that("incorrectly specified parameters trigger appropriate errors", {
  expect_error(zi_get_geometry(year = chr_year, method = "centroid"),
               "`year` must be numeric", fixed = TRUE)
  expect_error(zi_get_geometry(year = incorrect_year, method = "centroid"),
               "`year` must be between", fixed = TRUE)
  expect_error(zi_get_geometry(year = out_of_range_year, method = "centroid"),
               "`year` must be between", fixed = TRUE)
  expect_error(zi_get_geometry(year = correct_year, style = "zcta", method = "centroid"),
               "`style` must be", fixed = TRUE)
  expect_error(zi_get_geometry(year = correct_year, return = "ham", method = "centroid"),
               "`return` must be", fixed = TRUE)
  expect_warning(try(zi_get_geometry(year = correct_year, return = "full", style = "zcta3", method = "centroid", shift_geo = 3), silent = TRUE),
               "`return` cannot be", fixed = TRUE)
  expect_warning(try(zi_get_geometry(year = correct_year, style = "zcta3", cb = TRUE, method = "intersect", shift_geo = 3), silent = TRUE),
               "`cb` does not apply", fixed = TRUE)
  expect_error(zi_get_geometry(year = correct_year, shift_geo = 3, method = "intersect"),
               "`shift_geo` must be", fixed = TRUE)
  expect_error(zi_get_geometry(year = correct_year, shift_geo = TRUE, state = "WA", method = "intersect"),
               "`shift_geo` can only be used", fixed = TRUE)
  expect_error(zi_get_geometry(year = correct_year, state = c("AS", "GU"), method = "centroid"),
               "Territories must be supplied with `territory`", fixed = TRUE)
  expect_error(zi_get_geometry(year = correct_year, county = "TARRANT", method = "intersect"),
               "`state` is required when `county` is supplied", fixed = TRUE)
  expect_error(zi_get_geometry(year = correct_year, state = "WA"),
               "`method` is required", fixed = TRUE)
  expect_error(zi_get_geometry(year = correct_year, method = "ham"),
               "`method` must be", fixed = TRUE)
  expect_error(zi_get_geometry(year = correct_year, method = "intersect", territory = c("GI")),
               "`territory` contains an invalid value", fixed = TRUE)
  expect_error(zi_get_geometry(year = correct_year, method = "centroid", starts_with = 63),
               "`starts_with` must be a character vector of two-digit values.", fixed = TRUE)
  expect_error(zi_get_geometry(year = correct_year, method = "centroid", starts_with = "A1"),
               "`starts_with` must be a character vector of two-digit values.", fixed = TRUE)
  expect_error(zi_get_geometry(year = correct_year, class = "dataframe", method = "centroid"),
               "`class` must be", fixed = TRUE)
  expect_error(zi_get_geometry(year = correct_year, method = "intersect", includes = 10603),
               "`includes` contains invalid ZCTA values.", fixed = TRUE)
  expect_error(zi_get_geometry(year = correct_year, method = "centroid", excludes = "ham"),
               "`excludes` contains invalid ZCTA values.", fixed = TRUE)
})


# test successful execution ------------------------------------------------

test_that("zcta5 centroid returns expected schema", {
  skip_if_no_integration()
  result <- zi_get_geometry(year = 2020, style = "zcta5", method = "centroid")
  expect_s3_class(result, "sf")
  expect_true("GEOID20" %in% names(result) || "GEOID" %in% names(result))
  expect_true("geometry" %in% names(result))
  expect_gt(nrow(result), 30000)
})

test_that("zcta5 with state filter returns subset", {
  skip_if_no_integration()
  result <- zi_get_geometry(year = 2020, style = "zcta5", state = "MO",
                            method = "intersect")
  expect_s3_class(result, "sf")
  expect_true("GEOID20" %in% names(result) || "GEOID" %in% names(result))
  expect_gt(nrow(result), 500)
  expect_lt(nrow(result), 2000)
})

test_that("zcta5 with starts_with filters correctly", {
  skip_if_no_integration()
  result <- zi_get_geometry(year = 2020, style = "zcta5", method = "centroid",
                            starts_with = "63")
  expect_s3_class(result, "sf")
  geoid_col <- if ("GEOID20" %in% names(result)) "GEOID20" else "GEOID"
  expect_true(all(substr(result[[geoid_col]], 1, 2) == "63"))
  expect_gt(nrow(result), 0)
})

test_that("zcta5 return='full' includes all TIGER columns", {
  skip_if_no_integration()
  result <- zi_get_geometry(year = 2020, style = "zcta5", method = "centroid",
                            return = "full")
  expect_s3_class(result, "sf")
  expect_gt(ncol(result), 3)
})

test_that("zcta3 returns three-digit geometries", {
  skip_if_no_integration()
  result <- zi_get_geometry(year = 2020, style = "zcta3", method = "centroid",
                            shift_geo = TRUE)
  expect_s3_class(result, "sf")
  expect_true("GEOID" %in% names(result) || "ZCTA3" %in% names(result))
  geoid_col <- intersect(names(result), c("GEOID", "ZCTA3"))[1]
  expect_true(all(nchar(result[[geoid_col]]) == 3))
  expect_gt(nrow(result), 800)
})

test_that("zcta5 includes/excludes filters work", {
  skip_if_no_integration()
  result <- zi_get_geometry(year = 2020, style = "zcta5", method = "centroid",
                            includes = c("63005", "63139"))
  expect_s3_class(result, "sf")
  geoid_col <- if ("GEOID20" %in% names(result)) "GEOID20" else "GEOID"
  expect_true("63005" %in% result[[geoid_col]])
  expect_true("63139" %in% result[[geoid_col]])
})

# test 2022/2023 year regression ------------------------------------------------

test_that("2022 and 2023 continue to return valid zcta5 geometry with a state filter", {
  skip_if_no_integration()
  result_2022 <- zi_get_geometry(year = 2022, style = "zcta5", state = "MO",
                                 method = "intersect", class = "tibble")
  expect_gt(nrow(result_2022), 500)

  result_2023 <- zi_get_geometry(year = 2023, style = "zcta5", state = "MO",
                                 method = "intersect", class = "tibble")
  expect_gt(nrow(result_2023), 500)
})

# test 2024 support -------------------------------------------------

test_that("2024 nationwide request succeeds without a state/county filter", {
  skip_if_no_integration()
  result_2024 <- zi_get_geometry(year = 2024, style = "zcta5", cb = FALSE,
                                 class = "tibble")
  expect_gt(nrow(result_2024), 30000)
})

test_that("2024 state-scoped request succeeds", {
  skip_if_no_integration()
  result_2024 <- zi_get_geometry(year = 2024, style = "zcta5", state = "MO",
                                 method = "intersect", class = "tibble")
  expect_gt(nrow(result_2024), 500)
})
