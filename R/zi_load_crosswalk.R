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
#' @usage zi_load_crosswalk(zip_source = "UDS", year, qtr = NULL, target = NULL,
#'     query = NULL, key = NULL)
#'
#' @param zip_source Required character scalar; specifies the source of ZIP Code
#'     crosswalk data. This can be one of either \code{"UDS"} (default) or
#'     \code{"HUD"}.
#' @param year Required four-digit numeric scalar for year; varies based on source.
#'     For \code{"UDS"}, years 2009 through 2023 are available. For \code{"HUD"},
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
#'     stored in your \code{.RProfile} as \code{HUD_API_KEY}.
#'
#' @return A tibble containing the crosswalk file.
#'
#' @examples
#' \dontrun{
#'   ## access to former UDS mapper crosswalk
#'   zi_load_crosswalk(zip_source = "UDS", year = 2020)
#'
#'   ## access to HUD ZIP Code to Tract crosswalk for ZIP Code 27703
#'   zi_load_crosswalk(zip_source = "HUD", year = 2023, qtr = 1, target = "TRACT",
#'       query = 27703)
#'
#'   ## access to HUD ZIP Code to County crosswalk for all ZIP Codes in North Carolina
#'   zi_load_crosswalk(zip_source = "HUD", year = 2023, qtr = 1, target = "COUNTY",
#'       query = "NC")
#'
#'   ## access to HUD ZIP Code to CBSA crosswalk for all ZIP Codes
#'   zi_load_crosswalk(zip_source = "HUD", year = 2023, qtr = 1, target = "CBSA",
#'       query = "all")
#' }
#'
#' @export
zi_load_crosswalk <- function(zip_source = "UDS", year, qtr = NULL, target = NULL,
                              query = NULL, key = NULL){

  # check inputs
  if (zip_source %in% c("UDS", "HUD") == FALSE){
    stop("The 'zip_source' value provided is invalid. Please input either 'UDS' or 'HUD'.")
  }

  if (is.numeric(year) == FALSE){
    stop("The 'year' value provided is invalid. Please provide a numeric value for the requested year.")
  }

  if (zip_source == "UDS" & year %in% c(2009:2023) == FALSE){
    stop("The 'year' value provided is invalid for UDS data. Please provide a year between 2009 and 2023.")
  }

  if (zip_source == "HUD"){

    if (year %in% c(2010:2024) == FALSE){
      stop("The 'year' value provided is invalid for HUD data. Please provide a year between 2010 and 2024.")
    }

    if (qtr %in% c(1:4) == FALSE){
      stop("The 'qtr' value is required when 'zip_source' is 'HUD'. Please provide a value between 1 and 4.")
    }

    if (target %in% c("TRACT", "COUNTY", "CBSA", "CBSADIV", "CD", "COUNTYSUB") == FALSE){
      stop("The 'target' value is required when 'zip_source' is 'HUD'. Please provide a valid target value (see help file).")
    }

    if (is.null(query) == TRUE){
      stop("The 'query' value is required when 'zip_source' is 'HUD'. Please provide a valid query value (see help file).")
    }
  }

  # select zip
  if (zip_source == 'HUD'){
    out <- zi_load_hud(year = year, qtr = qtr, target = target, queries = toupper(query),
                key = key)
  } else if (zip_source == 'UDS'){
    out <- zi_load_uds(year = year)
  }

  # return output
  return(out)

}
