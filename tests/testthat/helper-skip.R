# Skip helper for integration tests that require network access.
# These tests are skipped by default in CI (fast feedback loop) and run
# on a weekly schedule or when RUN_INTEGRATION_TESTS=true is set.

skip_if_no_integration <- function() {

  # Always run integration tests interactively (developer machine)
  if (interactive()) return(invisible(TRUE))

  # In non-interactive contexts (CI), require explicit opt-in
  if (!identical(Sys.getenv("RUN_INTEGRATION_TESTS"), "true")) {
    testthat::skip("Integration tests skipped (set RUN_INTEGRATION_TESTS=true to run)")
  }

  # Still respect offline environments
  testthat::skip_if_offline()
}
