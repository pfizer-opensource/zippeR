#' Load Crosswalk Files
#'
#' @description Spatial data on USPS ZIP Codes are not published by the U.S.
#'     Postal Service or the U.S. Census Bureau. Instead, ZIP Codes can be
#'     converted to a variety of Census Bureau geographies using crosswalk files.
#'     This function reads in ZIP Code to ZIP Code Tabulation Area (ZCTA) crosswalk
#'     files from the former UDS Mapper project, which was sunset by the American
#'     Academy of Family Physicians in early 2024. It also provides access to the
#'     U.S. Department of Housing and Urban Development's ZIP Code crosswalk files,
#'     which provide similar functionality for converting ZIP Codes to a variety
#'     of geographies including counties.
#'
#'
#' @param zip_source Required character scalar; specifies the source of ZIP Code
#'     crosswalk data. This can be one of either \code{"UDS"} (default) or
#'     \code{"HUD"}.
#' @param year Required four-digit numeric scalar for year; varies based on source.
#'     For \code{"UDS"}, years 2009 through 2022 are available. For \code{"HUD"},
#'     years 2010 through 2024 are available.
#' @param qtr Numeric scalar, required when \code{zip_code} is \code{"HUD"}.
#'     Integer value between 1 and 4, representing the quarter of the year.
#' @param target Character scalar, required when \code{zip_code} is \code{"HUD"}.
#'     Can be one of \code{"TRACT"}, \code{"COUNTY"}, \code{"CBSA"},
#'     \code{"CBSADIV"}, \code{"CD"}, and \code{"COUNTYSUB"}.
#' @param query Scalar or vector, required when \code{zip_code} is \code{"HUD"}.
#'     This can be a five-digit numeric or character ZIP Code, a vector of
#'     ZIP Codes, a two-letter character state abbreviation, or \code{"all"}.
#' @param key Optional when \code{zip_code} is \code{"HUD"}. This should be a
#'     character string containing your HUD API key. Alternatively, it can be
#'     stored in your \code{.RProfile} as \code{hud_key}.
#'
#' @return A tibble containing the crosswalk file.
#'
#' @examplesIf interactive()
#'  # former UDS mapper crosswalks
#'  zi_load_crosswalk(zip_source = "UDS", year = 2020)
#'
#' @examplesIf nzchar(Sys.getenv("hud_key"))
#'  # HUD crosswalks
#'  ## ZIP Code to CBSA crosswalk for all ZIP Codes
#'  zi_load_crosswalk(zip_source = "HUD", year = 2023, qtr = 1, target = "CBSA",
#'      query = "all", key = Sys.getenv("hud_key"))
#'
#'  ## ZIP Code to County crosswalk for all ZIP Codes in Missouri
#'  zi_load_crosswalk(zip_source = "HUD", year = 2023, qtr = 1, target = "COUNTY",
#'      query = "MO", key = Sys.getenv("hud_key"))
#'
#'  ## ZIP Code to Tract crosswalk for ZIP Code 63139 in St. Louis City
#'  zi_load_crosswalk(zip_source = "HUD", year = 2023, qtr = 1, target = "TRACT",
#'      query = 63139, key = Sys.getenv("hud_key"))
#'
#' @export
zi_load_crosswalk <- function(zip_source = "UDS", year, qtr = NULL, target = NULL,
                              query = NULL, key = NULL){

  # check inputs
  if (!(zip_source %in% c("UDS", "HUD"))){
    cli::cli_abort(c(
      "{.arg zip_source} must be {.val UDS} or {.val HUD}.",
      "i" = "You provided {.val {zip_source}}."
    ))
  }

  if (!is.numeric(year)){
    cli::cli_abort(c(
      "{.arg year} must be numeric.",
      "i" = "You provided {.val {year}}."
    ))
  }

  if (zip_source == "UDS" & !(year %in% c(2009:2022))){
    cli::cli_abort(c(
      "{.arg year} must be between {.val 2009} and {.val 2022} when {.arg zip_source} is {.val UDS}.",
      "i" = "You provided {.val {year}}."
    ))
  }

  if (zip_source == "HUD"){

    if (!(year %in% c(2010:2024))){
      cli::cli_abort(c(
        "{.arg year} must be between {.val 2010} and {.val 2024} when {.arg zip_source} is {.val HUD}.",
        "i" = "You provided {.val {year}}."
      ))
    }

    if (!(qtr %in% c(1:4))){
      cli::cli_abort(c(
        "{.arg qtr} must be between {.val 1} and {.val 4} when {.arg zip_source} is {.val HUD}.",
        "i" = "You provided {.val {qtr}}."
      ))
    }

    if (!(target %in% c("TRACT", "COUNTY", "CBSA", "CBSADIV", "CD", "COUNTYSUB"))){
      cli::cli_abort(c(
        "{.arg target} is invalid when {.arg zip_source} is {.val HUD}.",
        "i" = "Use one of {.val TRACT}, {.val COUNTY}, {.val CBSA}, {.val CBSADIV}, {.val CD}, or {.val COUNTYSUB}."
      ))
    }

    if (is.null(query)){
      cli::cli_abort("{.arg query} is required when {.arg zip_source} is {.val HUD}.")
    }
  }

  # select zip
  if (zip_source == "HUD"){
    out <- zi_load_hud(year = year, qtr = qtr, target = target, queries = toupper(query),
                key = key)
  } else if (zip_source == "UDS"){
    out <- zi_load_uds(year = year)
  }

  # return output
  return(out)

}

# Load HUD and UDS Crosswalk Files
#
# @description These two helper functions load either the UDS crosswalk files
# or the HUD API crosswalk data based on the zip_source specified with
# zi_load_crosswalk.
#
# @param year A four-digit numeric scalar for year. \code{zippeR} currently
#     supports data for from 2010 to 2020.
#
# @param qtr Numbers 1-4 to select a quarter.
#
# @param target A string to select the crosswalk type. Options are 'TRACT',
# 'COUNTY', 'CBSA', 'CBSADIV', 'CD", and 'COUNTYSUB'.
#
# @param query This can be a five-digit numeric for zip code, a two-
# letter capitalized string state abbreviation, or 'ALL'.
#
# @return A tibble containing either the UDS Mapper crosswalk file for a given
# year the HUD API data for a particular year, quarter, target, and location.
#

zi_load_uds <- function(year) {
  # Load the bundled crosswalk and filter to the requested year
  crosswalk_path <- system.file("extdata", "uds_crosswalk.rds", package = "zippeR")

  if (crosswalk_path == "") {
    cli::cli_abort(c(
      "x" = "Bundled UDS crosswalk data not found in the package installation.",
      "i" = "Re-installing {.pkg zippeR} should resolve this."
    ))
  }

  all_data <- readRDS(crosswalk_path)
  out <- dplyr::filter(all_data, .data$year == .env$year)
  out$year <- NULL

  # check validation
  valid_zip <- zi_validate(out$zip)

  if (!valid_zip) {
    cli::cli_warn(c(
      "The {.arg zip} column failed initial validation.",
      "i" = "Inspect it closely and address issues found with {.fn zi_validate} before using it."
    ))
  }

  valid_zcta <- zi_validate(out$zcta)

  if (!valid_zcta) {
    cli::cli_warn(c(
      "The {.arg ZCTA} column failed initial validation.",
      "i" = "Inspect it closely and address issues found with {.fn zi_validate} before using it."
    ))
  }

  names(out) <- toupper(names(out))
  return(out)
}

zi_load_hud <- function(year, qtr, target, queries, key = NULL){

  if (is.null(key)){
    key <- Sys.getenv("hud_key")
  }

  if (key == ""){
    cli::cli_abort("Please provide a valid HUD API key.")
  }

  url <- "https://www.huduser.gov/hudapi/public/usps"

  # Loop over queries using base-R lapply + rbind
  result <- dplyr::as_tibble(do.call(rbind, lapply(queries, function(query) {

    if (year <= 2020 & query %in% c(datasets::state.abb, "VI", "PR", "ALL")){
      cli::cli_abort(c(
        "Two-letter state abbreviations and {.val ALL} are only available from the first quarter of {.val 2021} onward.",
        "i" = "You requested {.val {query}} for {.val {year}} Q{qtr}."
      ))
    }

    if (target == "CBSADIV" & year <= 2016 | target == "CBSADIV" & year == 2017 & qtr < 4){
      cli::cli_abort(c(
        "{.val CBSADIV} data is only available from the fourth quarter of {.val 2017} onward.",
        "i" = "You requested {.val {year}} Q{qtr}."
      ))
    }

    if (target == "COUNTYSUB" & year < 2018 | target == "COUNTYSUB" & year == 2018 & qtr < 2){
      cli::cli_abort(c(
        "{.val COUNTYSUB} data is only available from the second quarter of {.val 2018} onward.",
        "i" = "You requested {.val {year}} Q{qtr}."
      ))
    }

    if (!(query %in% c(datasets::state.abb, "VI", "PR", "ALL")) & !is.numeric(query) && nchar(as.character(query)) != 5){
      cli::cli_abort(c(
        "{.arg query} must be a state abbreviation, {.val ALL}, or a five-digit ZIP Code.",
        "i" = "You provided {.val {query}}."
      ))
    }

    url <- "https://www.huduser.gov/hudapi/public/usps"

    # create request
    if (target == "TRACT"){
      type <- "?type=1&query="
    } else if (target == "COUNTY"){
      type <- "?type=2&query="
    } else if (target == "CBSA"){
      type <- "?type=3&query="
    } else if (target == "CBSADIV"){
      type <- "?type=4&query="
    } else if (target == "CD"){
      type <- "?type=5&query="
    } else if (target == "COUNTYSUB"){
      type <- "?type=11&query="
    }

    # get data and format
    request <- tryCatch(
      httr2::request(paste0(url, type, query, "&year=", year, "&quarter=", qtr)) |>
        httr2::req_headers(Authorization = paste("Bearer", key)) |>
        httr2::req_perform(),
      httr2_http_error = function(e) {
        cli::cli_abort(c(
          "x" = "HUD API request failed with status {.val {httr2::resp_status(e$resp)}}.",
          "i" = "Check your API key and query parameters (year={year}, qtr={qtr}, query={query})."
        ))
      }
    )

    content <- httr2::resp_body_string(request)
    json <- jsonlite::fromJSON(content)
    list <- lapply(json, "[[", 5)

    # create output
    out <- as.data.frame(list)
    colnames(out) <- sub("data.", "", colnames(out))
    names(out) <- toupper(names(out))
    out <- dplyr::as_tibble(out)

    #return output
    return(out)

  })))

}
