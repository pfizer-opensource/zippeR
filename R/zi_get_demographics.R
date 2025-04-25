#' Download Demographic Data for Five-digit ZCTAs
#'
#' @description This function returns demographic data for five-digit ZIP Code
#'     Tabulation Areas (ZCTAs), which are rough approximations of many (but not
#'     all) USPS ZIP codes.
#'
#' @usage zi_get_demographics(year, variables = NULL, table = NULL,
#'     survey, output = "tidy", zcta = NULL, key = NULL)
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
#'     \code{"tidy"} or \code{"wide"} format.
#'
#' @examples
#' \donttest{
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
#' }
#'
#' @export
zi_get_demographics <- function(year, variables = NULL,
                                table = NULL, survey, output = "tidy",
                                zcta = NULL, key = NULL){

  # check inputs
  if (missing(year) == TRUE){
    stop("The 'year' value is missing. Please provide a numeric value between 2010 and 2022.")
  }

  if (is.numeric(year) == FALSE){
    stop("The 'year' value provided is invalid. Please provide a numeric value between 2010 and 2022.")
  }

  if (length(survey) > 1){
    stop("One only 'survey' product may be requested at a time.")
  }

  if (survey %in% c("sf1", "sf3", "acs1", "acs3", "acs5") == FALSE){
    stop("The 'survey' requested is invalid. Please choose one of 'sf1', 'sf3', 'acs1', 'acs3', or 'acs5'.")
  }

  if (survey %in% c("sf1", "sf3") == TRUE & year != 2010){
    stop("The 'year' value provided is invalid for Decennial Census data. Only 2010 may be requested currently.")
  }

  if (survey %in% c("acs1", "acs5") == TRUE & year %in% c(2010:2022) == FALSE){
    stop("The 'year' value provided is invalid for 1- or 5-year American Community Survey data. Please provide a year between 2010 and 2022.")
  }

  if (survey == "acs3" & year %in% c(2010:2013) == FALSE){
    stop("The 'year' value provided is invalid for 3-year American Community Survey data. Please provide a year between 2010 and 2013.")
  }

  if (is.null(variables) == FALSE & is.null(table) == FALSE){
    stop("The 'variables' or 'table' arguments cannot be used simultaneously. Please choose one or the other.")
  }

  if (output %in% c("tidy", "wide") == FALSE){
    stop("The 'output' requested is invalid. Please choose one of 'tidy' or 'wide'.")
  }

  if (is.null(zcta) == FALSE){
    valid <- zi_validate(zcta)

    if (valid == FALSE){
      stop("ZCTA data passed to the 'zcta' argument are invalid. Please use 'zi_validate()' with the 'verbose = TRUE' option to investgiate further. The 'zi_repair()' function may be used to address isses.")
    }
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
      out <- dplyr::mutate(out, GEOID = stringr::word(NAME, 2))
    }

  }

  # tidy if data are returned
  if(!is.null(out)){
    ## remove additional cols and re-arrange
    out <- dplyr::select(out, -NAME)
    out <- dplyr::arrange(out, GEOID)

    ## optionally subset
    if (is.null(zcta) == FALSE){
      out <- dplyr::filter(out, GEOID %in% zcta == TRUE)
    }
  }

  # return output
  return(out)

}
