# test zi_prep_hud ------------------------------------------------

# create test data ------------------------------------------------

mo_xwalk <- zi_mo_hud

# test errors ------------------------------------------------

test_that("missing parameters trigger appropriate errors", {
  expect_error(zi_prep_hud(mo_xwalk),
               "is required", fixed = TRUE)
})

test_that("incorrectly specified parameters trigger appropriate errors", {
  expect_error(zi_prep_hud(mo_xwalk, by = "invalid"),
               "must be", fixed = TRUE)
  expect_error(zi_prep_hud(mo_xwalk, by = "residential", return_max = "yes"),
               "must be", fixed = TRUE)
  expect_error(zi_prep_hud(data.frame(a = 1), by = "residential"),
               "missing required columns", fixed = TRUE)
})

# test inputs ------------------------------------------------

test_that("correctly specified functions execute without error", {
  skip_on_cran()
  expect_no_error(zi_prep_hud(mo_xwalk, by = "residential"))
  expect_no_error(zi_prep_hud(mo_xwalk, by = "commercial"))
  expect_no_error(zi_prep_hud(mo_xwalk, by = "total"))
  expect_no_error(zi_prep_hud(mo_xwalk, by = "residential", return_max = FALSE))
})

# test outputs ------------------------------------------------

test_that("correctly specified functions produce expected classes", {
  skip_on_cran()
  result <- zi_prep_hud(mo_xwalk, by = "residential")
  expect_s3_class(result, "tbl_df")
  expect_true("zip5" %in% names(result))
  expect_true("geoid" %in% names(result))
  expect_true("state" %in% names(result))
  expect_true("ratio" %in% names(result))
})

test_that("return_max = TRUE returns one row per zip5-state combination", {
  skip_on_cran()
  result <- zi_prep_hud(mo_xwalk, by = "residential", return_max = TRUE)
  counts <- table(paste(result$zip5, result$state))
  expect_true(all(counts == 1))
})

test_that("return_max = FALSE includes max column", {
  skip_on_cran()
  result <- zi_prep_hud(mo_xwalk, by = "residential", return_max = FALSE)
  expect_true("max" %in% names(result))
})
