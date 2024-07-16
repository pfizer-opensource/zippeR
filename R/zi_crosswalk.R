#' Crosswalk ZIP Codes with UDS, HUD, or a Custom Dictionary
#'
#' @description This function compares input data containing ZIP Codes with
#'     a crosswalk file that will append ZCTAs. This is an important step because
#'     not all ZIP Codes have the same five digits as their enclosing ZCTA.
#'
#' @usage zi_crosswalk(.data, input_var, zip_source = "UDS", source_var,
#'     source_result, year = NULL, qtr = NULL, target = NULL, query = NULL,
#'     by = NULL, return_max = NULL, key = NULL, return = "id")
#'
#' @param .data An "input object" that is data.frame or tibble that contains
#'     ZIP Codes to be crosswalked.
#' @param input_var The column in the input data that contains five-digit ZIP
#'     Codes. If the input is numeric, it will be transformed to character data
#'     and leading zeros will be added.
#' @param zip_source Required character scalar or data frame; specifies the
#'     source of ZIP Code crosswalk data. This can be one of either \code{"UDS"}
#'     (default) or \code{"HUD"}, or a data frame containing a custom dictionary.
#' @param source_var Character scalar, required when \code{zip_source} is a
#'     data frame containing a custom dictionary; specifies the column name in
#'     the dictionary object that contains ZIP Codes.
#' @param source_result Character scalar, required when \code{zip_source} is a
#'     data frame containing a custom dictionary; specifies the column name in
#'     the dictionary object that contains ZCTAs, GEOIDs, or other values.
#' @param year Optional four-digit numeric scalar for year; varies based on source.
#'     For \code{"UDS"}, years 2009 through 2023 are available. For \code{"HUD"},
#'     years 2010 through 2024 are available. Does not need to be specified when
#'     a custom dictionary is used.
#' @param qtr Numeric scalar, required when \code{zip_code} is \code{"HUD"}.
#'     Integer value between 1 and 4, representing the quarter of the year.
#' @param target Character scalar, required when \code{zip_code} is \code{"HUD"}.
#'     Can be one of \code{"TRACT"}, \code{"COUNTY"}, \code{"CBSA"},
#'     \code{"CBSADIV"}, \code{"CD"}, and \code{"COUNTYSUB"}.
#' @param query Scalar or vector, required when \code{zip_code} is \code{"HUD"}.
#'     This can be a five-digit numeric or character ZIP Code, a vector of
#'     ZIP Codes, a two-letter character state abbreviation, or \code{"all"}.
#' @param by Character scalar, required when \code{zip_code} is \code{"HUD"};
#'     the column name to use for identifying the best match for a given ZIP Code.
#'     This could be either \code{"residential"}, \code{"commercial"}, or \code{"total"}.
#' @param return_max Logical scalar, required when \code{zip_code} is \code{"HUD"};
#'     if \code{TRUE} (default), only the geography with the highest proportion
#'     of the ZIP Code type will be returned. If the ZIP Code straddles two states,
#'     two records will be returned. If \code{FALSE}, all records for the ZIP Code
#'     will be returned. Where a tie exists (i.e. two geographies each contain half
#'     of all addresses), the county with the lowest \code{GEOID} value will be
#'     returned.
#' @param key Optional when \code{zip_code} is \code{"HUD"}. This should be a
#'     character string containing your HUD API key. Alternatively, it can be
#'     stored in your \code{.RProfile} as \code{hud_key}.
#' @param return Character scalar, specifies the type of output to return. Can be
#'     one of \code{"id"} (default), which appends only the crosswalked value,
#'     or \code{"all"}, which returns the entire crosswalk file appended to
#'     the source data.
#'
#' @return A \code{tibble} with crosswalk values (or optionally, the full
#'     crosswalk file) appended based on the \code{return} argument.
#'
#' @examples
#' # create sample data
#' df <- data.frame(id = c(1:3), zip5 = c("63005", "63139", "63636"))
#'
#' # UDS crosswalk
#' \donttest{
#'   zi_crosswalk(df, input_var = zip5, zip_source = "UDS", year = 2022)
#' }
#'
#' # HUD crosswalk
#' \donttest{
#'   zi_crosswalk(df, input_var = zip5, zip_source = "HUD", year = 2023,
#'     qtr = 1, target = "COUNTY", query = "MO", by = "residential",
#'     return_max = TRUE)
#' }
#'
#' # custom dictionary
#' ## load sample crosswalk data to simulate custom dictionary
#' mo_xwalk <- zi_mo_hud
#'
#' # prep crosswalk
#' # when a ZIP Code crosses county boundaries, the portion with the largest
#' # number of residential addresses will be returned
#' mo_xwalk <- zi_prep_hud(mo_xwalk, by = "residential", return_max = TRUE)
#'
#' ## crosswalk
#' zi_crosswalk(df, input_var = zip5, zip_source = mo_xwalk, source_var = zip5,
#'   source_result = geoid)
#'
#' @export
zi_crosswalk <- function(.data, input_var, zip_source = "UDS", source_var,
                         source_result, year = NULL, qtr = NULL,
                         target = NULL, query = NULL, by = NULL, return_max = NULL,
                         key = NULL, return = "id"){

  # check inputs
  ## determine workflow
  if (inherits(zip_source, what = "data.frame")){
    workflow <- "custom"
  } else if (zip_source %in% c("UDS", "HUD")){
    workflow <- "api"
  } else {
    stop("The 'zip_source' argument is invalid. Please provide either 'UDS', 'HUD', or a custom dictionary.")
  }

  ## checks regardless of workflow
  if (!inherits(.data, what = "data.frame")){
    stop("The '.data' object provided is not a data frame.")
  }

  if (missing(input_var)){
    stop("The 'input_var' argument is missing. Please provide the column name in '.data' that contains ZIP Code values.")
  }

  input_varQN <- as.character(substitute(input_var))

  if (input_varQN %in% names(.data) == FALSE){
    stop("The given 'input_var' column is not found in your input object.")
  }

  valid <- zi_validate(x = .data[[input_varQN]])

  if (valid == FALSE){
    stop(paste0("Input ZIP Code data in the '", input_varQN, "' column are invalid. Please use 'zi_validate()' with the 'verbose = TRUE' option to investigate further. The 'zi_repair()' function may be used to address issues."))
  }

  if (!return %in% c("id", "all")){
    stop("The 'return' value provided is invalid. Please input either 'id' or 'all'.")
  }

  ## checks dependent on workflow
  if (workflow == "custom"){

    if (missing(source_var)){
      stop("The 'source_var' argument is missing. Please provide the column name in the dictionary object that contains ZIP Code values.")
    }

    source_varQN <- as.character(substitute(source_var))

    if (source_varQN %in% names(zip_source) == FALSE){
      stop("The given 'source_var' column is not found in your dictionary object.")
    }

    valid <- zi_validate(x = zip_source[[source_varQN]], style = "zcta5")

    if (valid == FALSE){
      stop(paste0("Dictionary ZCTA data in the '", source_varQN, "' column are invalid. Please use 'zi_validate()' with the 'verbose = TRUE' option to investigate further. The 'zi_repair()' function may be used to address issues."))
    }

    if (missing(source_result)){
      stop("The 'source_result' argument is missing. Please provide the column name in the dictionary object that contains the crosswalk result values.")
    }

    source_resultQN <- as.character(substitute(source_result))

    if (source_resultQN %in% names(zip_source) == FALSE){
      stop("The given 'source_result' column is not found in your dictionary object.")
    }

  } else if (workflow == "api"){

    if (is.numeric(year) == FALSE){
      stop("The 'year' value provided is invalid. Please provide a numeric value for the requested year.")
    }

    if (zip_source == "UDS" & year %in% c(2009:2022) == FALSE){
      stop("The 'year' value provided is invalid for UDS data. Please provide a year between 2009 and 2022.")
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

      if (is.null(by)){
        stop("A valid value for 'by' value is required. Please input either 'residential', 'commercial', or 'total'.")
      }

      if (by %in% c("residential", "commercial", "total") == FALSE){
        stop("The 'by' value provided is invalid. Please input either 'residential', 'commercial', or 'total'.")
      }

      if (!is.logical(return_max)){
        stop("A logical value must be provided for the 'return_max' argument.")
      }
    }
  }

  # prepare dictionary data
  if (workflow == "api"){

    if (zip_source == "HUD"){

      dict <- zi_load_hud(year = year, qtr = qtr, target = target, queries = toupper(query),
                         key = key)
      dict <- zi_prep_hud(dict, by = by, return_max = return_max)

      if (return == "id"){
        dict <- dplyr::select(dict, zip5, source_geoid = geoid)
      } else if (return == "all"){
        dict <- dplyr::select(dict, zip5, geoid, dplyr::everything())
      }

      source_resultQN <- "geoid"

    } else if (zip_source == "UDS"){

      dict <- zi_load_uds(year = year)

      if (return == "id"){
        dict <- dplyr::select(dict, zip5 = ZIP, source_zcta = ZCTA)
      } else if (return == "all"){
        dict <- dplyr::select(dict, zip5 = ZIP, zcta = ZCTA, dplyr::everything())
      }

      source_resultQN <- "zcta"

    }

    dict <- dplyr::rename_with(dict, .fn = tolower)

    source_varQN <- "zip5"

  } else if (workflow == "custom"){
    dict <- zip_source

    if (return == "id"){
      dict <- dplyr::select(dict, source_varQN, source_resultQN)

      source_new_result <- paste0("source_", source_resultQN)
      names(dict)[names(dict) == source_resultQN] <- source_new_result

    } else if (return == "all"){
      dict <- dplyr::select(dict, source_varQN, source_resultQN, dplyr::everything())
    }
  }

  if (return == "all"){
    dict_names <- names(dict)[names(dict) != source_varQN]
    dict <- dplyr::rename_with(dict, .fn = ~paste0("source_", .x), .cols = dict_names)
  }

  # create output
  ## check for naming conflicts
  dict_names <- names(dict)[names(dict) != source_varQN]

  if (any(dict_names %in% names(.data))){
    warning("The column names in the dictionary object conflict with column names in the input data. Please inspect output carefully.")
  }

  ## join with input data
  out <- merge(x = .data, y = dict, by.x = input_varQN, by.y = source_varQN, all.x = TRUE, all.y = FALSE)

  ## create tibble
  out <- tibble::as_tibble(out)

  # return output
  return(out)

}
