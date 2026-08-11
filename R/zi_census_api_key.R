#' Set a Census API Key
#'
#' @description This function stores a Census API key for use with
#'     \code{zi_get_demographics()} and \code{zi_aggregate()}. It is a thin,
#'     \code{zi_}-prefixed wrapper around \code{tidycensus::census_api_key()}
#'     so that package setup can be completed entirely through
#'     \code{zippeR}'s own function namespace.
#'
#' @param key A character scalar containing a Census API key, which can be
#'     obtained at \url{https://api.census.gov/data/key_signup.html}.
#' @param overwrite A logical scalar; if \code{TRUE}, overwrite an existing
#'     Census API key stored in \code{.Renviron}. Default is \code{FALSE}.
#' @param install A logical scalar; if \code{TRUE}, install the key in
#'     \code{.Renviron} for use in future sessions. Default is \code{FALSE}.
#'
#' @return The Census API key, invisibly.
#'
#' @examplesIf interactive()
#'   zi_census_api_key(key = "111111abc", install = TRUE)
#'
#' @export
zi_census_api_key <- function(key, overwrite = FALSE, install = FALSE){

  # check inputs
  if (missing(key)){
    cli::cli_abort("{.arg key} is required. Please provide a Census API key.")
  }

  if (length(key) != 1){
    cli::cli_abort("{.arg key} must be a single value.")
  }

  if (!is.character(key)){
    cli::cli_abort(c(
      "{.arg key} must be a character scalar.",
      "i" = "You provided a value of class {.cls {class(key)}}."
    ))
  }

  if (is.na(key)){
    cli::cli_abort("{.arg key} cannot be {.val NA}. Please provide a Census API key.")
  }

  if (key == ""){
    cli::cli_abort("{.arg key} cannot be an empty string. Please provide a Census API key.")
  }

  if (grepl("['\"\r\n]", key)){
    cli::cli_abort(c(
      "{.arg key} contains an invalid character.",
      "i" = "Census API keys do not contain quotes or line breaks; check that you copied the key correctly."
    ))
  }

  if (length(overwrite) != 1){
    cli::cli_abort("{.arg overwrite} must be a single value.")
  }

  if (!is.logical(overwrite) || is.na(overwrite)){
    cli::cli_abort(c(
      "{.arg overwrite} must be {.val TRUE} or {.val FALSE}.",
      "i" = "You provided {.val {overwrite}}."
    ))
  }

  if (length(install) != 1){
    cli::cli_abort("{.arg install} must be a single value.")
  }

  if (!is.logical(install) || is.na(install)){
    cli::cli_abort(c(
      "{.arg install} must be {.val TRUE} or {.val FALSE}.",
      "i" = "You provided {.val {install}}."
    ))
  }

  # delegate to tidycensus for its side effect (session env var / .Renviron write)
  tidycensus::census_api_key(key = key, overwrite = overwrite, install = install)

  return(invisible(key))

}
