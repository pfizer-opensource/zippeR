#' Download Demographic Data for Five-digit ZCTAs
#'
#' @description This function returns demographic data for five-digit ZIP Code
#'     Tabulation Areas (ZCTAs), which are rough approximations of many (but not
#'     all) USPS ZIP codes.
#'
#'
#' @param year A four-digit numeric scalar for year. \code{zippeR} currently
#'     supports data for from 2010 to 2022. Different \code{survey} products
#'     are available for different years. See the \code{survey} parameter
#'     for more details
#' @param variables A character scalar or vector of variable IDs.
#' @param table A character scalar of a table ID (only one table may be
#'     requested per call).
#' @param survey A character scalar representing the Census product. It can
#'     be either a Decennial Census product (either \code{"sf1"} or \code{"sf3"})
#'     or an American Community Survey product (either \code{"acs1"},
#'     \code{"acs3"}, or \code{"acs5"}). For Decennial Census calls, only the 2010
#'     Census is available. In addition, if a variable cannot be found in \code{"sf1"},
#'     the function will look in \code{"sf3"}. Also note that \code{"acs3"} was
#'     discontinued after 2013.
#' @param output A character scalar; one of \code{"tidy"} (long output) or
#'     \code{"wide"} depending on the type of data format you want. If you are
#'     planning to pass these data to \code{zi_aggregate()}, you must choose
#'     \code{"tidy"}. If you are leaving these data as five-digit ZCTAs and are
#'     planning to join them with geometric data, \code{"wide"} is the
#'     strongly encouraged format.
#' @param zcta An optional vector of ZCTAs that demographic data are requested
#'     for. If this is \code{NULL}, data will be returned for all ZCTAs. If a
#'     vector is supplied, only data for those requested ZCTAs will be returned.
#'     The vector can be created with \code{zi_get_geometry()} and should only
#'     contain five-digit ZCTAs.
#' @param key A Census API key, which can be obtained at
#'     \url{https://api.census.gov/data/key_signup.html}. This can be omitted if
#'     \code{tidycensus::census_api_key()} has been used to write your key to
#'     your \code{.Renviron} file. You can check whether an API key has been
#'     written to \code{.Renviron} by using \code{Sys.getenv("CENSUS_API_KEY")}.
#'
#' @return A tibble containing all demographic data requested in either
#'     \code{"tidy"} or \code{"wide"} format, or \code{NULL} if the Census
#'     Bureau API call fails.
#'
#' @examplesIf interactive()
#'   # download all ZCTAs
#'   zi_get_demographics(year = 2012, variables = "B01003_001", survey = "acs5")
#'
#'   # limit output to subset of ZCTAs
#'   ## download all ZCTAs in Missouri, intersects method
#'   mo20 <- zi_get_geometry(year = 2020, state = "MO", method = "intersect")
#'
#'   ## download demographic data
#'   zi_get_demographics(year = 2012, variables = "B01003_001", survey = "acs5",
#'       zcta = mo20$GEOID)
#'
#' @export
zi_get_demographics <- function(year, variables = NULL,
                                table = NULL, survey, output = "tidy",
                                zcta = NULL, key = NULL){

  # check inputs
  if (missing(year)){
    cli::cli_abort("{.arg year} is required. Please provide a numeric value between {.val 2010} and {.val 2022}.")
  }

  if (!is.numeric(year)){
    cli::cli_abort(c(
      "{.arg year} must be numeric.",
      "i" = "You provided {.val {year}}."
    ))
  }

  if (length(survey) > 1){
    cli::cli_abort(c(
      "{.arg survey} must contain a single value.",
      "i" = "You provided {.val {survey}}."
    ))
  }

  if (!(survey %in% c("sf1", "sf3", "acs1", "acs3", "acs5"))){
    cli::cli_abort(c(
      "{.arg survey} must be one of {.val sf1}, {.val sf3}, {.val acs1}, {.val acs3}, or {.val acs5}.",
      "i" = "You provided {.val {survey}}."
    ))
  }

  if (survey %in% c("sf1", "sf3") & year != 2010){
    cli::cli_abort(c(
      "{.arg year} must be {.val 2010} for Decennial Census data.",
      "i" = "You requested {.val {survey}} for {.val {year}}."
    ))
  }

  if (survey %in% c("acs1", "acs5") & !(year %in% c(2010:2022))){
    cli::cli_abort(c(
      "{.arg year} must be between {.val 2010} and {.val 2022} for {.arg survey} values {.val acs1} and {.val acs5}.",
      "i" = "You requested {.val {survey}} for {.val {year}}."
    ))
  }

  if (survey == "acs3" & !(year %in% c(2010:2013))){
    cli::cli_abort(c(
      "{.arg year} must be between {.val 2010} and {.val 2013} when {.arg survey} is {.val acs3}.",
      "i" = "You provided {.val {year}}."
    ))
  }

  if (!is.null(variables) & !is.null(table)){
    cli::cli_abort("{.arg variables} and {.arg table} cannot be used together.")
  }

  if (!(output %in% c("tidy", "wide"))){
    cli::cli_abort(c(
      "{.arg output} must be {.val tidy} or {.val wide}.",
      "i" = "You provided {.val {output}}."
    ))
  }

  if (!is.null(zcta)){
    valid <- zi_validate(zcta)

    if (!valid){
      cli::cli_abort(c(
        "{.arg zcta} contains invalid ZCTA values.",
        "i" = "Use {.fn zi_validate} with {.code verbose = TRUE} to investigate further."
      ))
    }
  }

  if (is.null(variables) & is.null(table)){
    cli::cli_abort("Either {.arg variables} or {.arg table} must be provided.")
  }

  # call underlying tidycensus data
  if (survey %in% c("sf1", "sf3")){

    ## call get_decennial
    out <- zi_get_decennial(geography = "zcta", variables = variables,
                            table = table, year = year, output = output,
                            survey = survey, key = key)

  } else if (survey %in% c("acs1", "acs3", "acs5")){

    ## call get_acs
    out <- zi_get_acs(geography = "zcta", variables = variables,
                      table = table, year = year, output = output,
                      survey = survey, key = key)

    ## prep data
    if (!is.null(out)){
      out <- dplyr::mutate(out, GEOID = sub("^\\S+ ", "", NAME))
    }

  }

  # tidy if data are returned
  if (!is.null(out)){
    ## remove additional cols and re-arrange
    out <- dplyr::select(out, -NAME)
    out <- dplyr::arrange(out, GEOID)

    ## optionally subset
    if (!is.null(zcta)){
      out <- dplyr::filter(out, GEOID %in% zcta)
    }
  }

  # return output
  return(out)

}
