#' Crosswalk ZIP Codes with UDS, HUD, or a Custom Dictionary
#'
#' @description This function compares input data containing ZIP Codes with
#'     a crosswalk file that will append ZCTAs. This is an important step because
#'     not all ZIP Codes have the same five digits as their enclosing ZCTA.
#'
#'
#' @param .data An "input object" that is data.frame or tibble that contains
#'     ZIP Codes to be crosswalked.
#' @param input_var The column in the input data that contains five-digit ZIP
#'     Codes, specified as a bare (unquoted) column name. Input must be character
#'     data with proper leading zeros; use \code{\link{zi_repair}} to fix
#'     numeric inputs first.
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
#'     For \code{"UDS"}, years 2009 through 2022 are available. For \code{"HUD"},
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
#' @param input_zip \strong{[Deprecated]} Use \code{input_var}
#'     instead. Will be removed in early 2027.
#' @param dict \strong{[Deprecated]} Use \code{zip_source} and
#'     \code{year} instead. Will be removed in early 2027.
#'
#' @return A \code{tibble} with crosswalk values (or optionally, the full
#'     crosswalk file) appended based on the \code{return} argument.
#'
#' @examples
#' # create sample data
#' df <- data.frame(id = c(1:3), zip5 = c("63005", "63139", "63636"))
#'
#' @examplesIf interactive()
#' # UDS crosswalk
#' zi_crosswalk(df, input_var = zip5, zip_source = "UDS", year = 2022)
#'
#' @examplesIf nzchar(Sys.getenv("hud_key"))
#' # HUD crosswalk
#' zi_crosswalk(df, input_var = zip5, zip_source = "HUD", year = 2023,
#'   qtr = 1, target = "COUNTY", query = "MO", by = "residential",
#'   return_max = TRUE, key = Sys.getenv("hud_key"))
#'
#' @examples
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
                         key = NULL, return = "id",
                         input_zip, dict = NULL){

  # handle deprecated arguments
  if (!is.null(dict)){
    cli::cli_warn(c(
      "{.arg dict} is deprecated and will be removed in early 2027.",
      "i" = "Use {.arg zip_source} and {.arg year} instead."
    ))
    if (inherits(dict, "data.frame")){
      zip_source <- dict
    } else if (is.character(dict)){
      parts <- strsplit(dict, " ")[[1]]
      if (length(parts) == 2){
        zip_source <- parts[1]
        if (is.null(year)) year <- as.numeric(parts[2])
      } else {
        zip_source <- dict
      }
    }
  }

  if (!missing(input_zip)){
    cli::cli_warn(c(
      "{.arg input_zip} is deprecated and will be removed in early 2027.",
      "i" = "Use {.arg input_var} instead."
    ))
  }

  # check inputs
  ## determine workflow
  if (inherits(zip_source, what = "data.frame")){
    workflow <- "custom"
  } else if (zip_source %in% c("UDS", "HUD")){
    workflow <- "api"
  } else {
    cli::cli_abort(c(
      "{.arg zip_source} must be {.val UDS}, {.val HUD}, or a data frame.",
      "i" = "You provided {.val {zip_source}}."
    ))
  }

  ## checks regardless of workflow
  if (!inherits(.data, what = "data.frame")){
    cli::cli_abort("{.arg .data} must be a data frame.")
  }

  # resolve input_var (handle deprecated input_zip)
  if (!missing(input_zip)){
    input_varQN <- as.character(substitute(input_zip))
  } else if (missing(input_var)){
    cli::cli_abort("{.arg input_var} is required. Provide the column in {.arg .data} that contains ZIP Code values.")
  } else {
    input_varQN <- as.character(substitute(input_var))
  }

  if (!(input_varQN %in% names(.data))){
    cli::cli_abort(c(
      "{.arg input_var} was not found in {.arg .data}.",
      "i" = "You provided {.val {input_varQN}}."
    ))
  }

  valid <- zi_validate(x = .data[[input_varQN]])

  if (!valid){
    cli::cli_abort(c(
      "Input ZIP Code data in {.arg {input_varQN}} are invalid.",
      "i" = "Use {.fn zi_validate} with {.code verbose = TRUE} to investigate further."
    ))
  }

  if (!return %in% c("id", "all")){
    cli::cli_abort(c(
      "{.arg return} must be {.val id} or {.val all}.",
      "i" = "You provided {.val {return}}."
    ))
  }

  ## checks dependent on workflow
  if (workflow == "custom"){

    if (missing(source_var)){
      cli::cli_abort("{.arg source_var} is required. Provide the column in {.arg zip_source} that contains ZIP Code values.")
    }

    source_varQN <- as.character(substitute(source_var))

    if (!(source_varQN %in% names(zip_source))){
      cli::cli_abort(c(
        "{.arg source_var} was not found in {.arg zip_source}.",
        "i" = "You provided {.val {source_varQN}}."
      ))
    }

    valid <- zi_validate(x = zip_source[[source_varQN]], style = "zcta5")

    if (!valid){
      cli::cli_abort(c(
        "Dictionary ZCTA data in {.arg {source_varQN}} are invalid.",
        "i" = "Use {.fn zi_validate} with {.code verbose = TRUE} to investigate further."
      ))
    }

    if (missing(source_result)){
      cli::cli_abort("{.arg source_result} is required. Provide the result column in {.arg zip_source}.")
    }

    source_resultQN <- as.character(substitute(source_result))

    if (!(source_resultQN %in% names(zip_source))){
      cli::cli_abort(c(
        "{.arg source_result} was not found in {.arg zip_source}.",
        "i" = "You provided {.val {source_resultQN}}."
      ))
    }

  } else if (workflow == "api"){

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

      if (is.null(by)){
        cli::cli_abort("{.arg by} is required. Please provide {.val residential}, {.val commercial}, or {.val total}.")
      }

      if (!(by %in% c("residential", "commercial", "total"))){
        cli::cli_abort(c(
          "{.arg by} must be {.val residential}, {.val commercial}, or {.val total}.",
          "i" = "You provided {.val {by}}."
        ))
      }

      if (!is.logical(return_max)){
        cli::cli_abort(c(
          "{.arg return_max} must be {.val TRUE} or {.val FALSE}.",
          "i" = "You provided {.val {return_max}}."
        ))
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
      dict <- dplyr::select(dict, dplyr::all_of(source_varQN), dplyr::all_of(source_resultQN))

      source_new_result <- paste0("source_", source_resultQN)
      names(dict)[names(dict) == source_resultQN] <- source_new_result

    } else if (return == "all"){
      dict <- dplyr::select(dict, dplyr::all_of(source_varQN), dplyr::all_of(source_resultQN), dplyr::everything())
    }
  }

  if (return == "all"){
    dict_names <- names(dict)[names(dict) != source_varQN]
    dict <- dplyr::rename_with(dict, .fn = ~paste0("source_", .x), .cols = dplyr::all_of(dict_names))
  }

  # create output
  ## check for naming conflicts
  dict_names <- names(dict)[names(dict) != source_varQN]

  if (any(dict_names %in% names(.data))){
    cli::cli_warn("Column names in {.arg zip_source} conflict with columns in {.arg .data}. Inspect the output carefully.")
  }

  ## join with input data
  join_by <- stats::setNames(source_varQN, input_varQN)
  out <- dplyr::left_join(.data, dict, by = join_by)

  ## create tibble
  out <- tibble::as_tibble(out)

  # return output
  return(out)

}
