#' Label ZIP Codes with Contextual Data
#'
#' @description This function appends information about the city (for five-digit
#'    ZIP Codes) or area (for three-digit ZIP Codes) to a data frame containing
#'    these values. State is returned for both types of ZIP Codes. The function
#'    also optionally returns data on Sectional Center Facilities (SCFs) for
#'    three-digit ZIP Codes.
#'
#'
#' @param .data An "input object" that is data.frame or tibble that contains
#'     ZIP Codes to be crosswalked.
#' @param input_var The column in the input data that contains five-digit ZIP
#'     Codes, specified as a bare (unquoted) column name. Input must be character
#'     data with proper leading zeros; use \code{\link{zi_repair}} to fix
#'     numeric inputs first.
#' @param label_source Required character scalar or data frame; specifies the
#'     source of the label data. This could be either \code{'UDS'} (default) or
#'     \code{'USPS'}, or a data frame containing a custom dictionary.
#' @param source_var Character scalar, required when \code{label_source} is a
#'     data frame containing a custom dictionary; specifies the column name in
#'     the dictionary object that contains ZIP Codes.
#' @param type Character scalar, required when \code{label_source} is either
#'     \code{label_source} is \code{'UDS'} or \code{'USPS'}; one of either
#'     \code{'zip3'} or \code{'zip5'}. The  \code{'zip3'} type is only available
#'     from the \code{'USPS'} source, while the \code{'zip5'} type is available
#'     from \code{'UDS'}.
#' @param include_scf A logical scalar required when \code{label_source = 'USPS'} and
#'     \code{type = 'zip3'}; specifying whether to include the SCF (Sectional
#'     Center Facility) ID in the output. The default is \code{FALSE}.
#' @param vintage Character or numeric scalar, required when \code{label_source}
#'     is either \code{label_source} is \code{'UDS'} or \code{'USPS'}; specifying
#'     the date for \code{label_source = 'USPS'} or the year of the data for
#'     \code{label_source = 'UDS'}. The \code{zip_load_labels_list()} function
#'     can be used to see available date values for \code{label_source = 'USPS'}.
#'
#' @details Labels are approximations of the actual location of a ZIP Code. For
#'     five-digit ZIP Codes, the city and state may or may not correspond to
#'     an individuals' mailing address city (since multiple cities may be
#'     accepted as valid by USPS for a particular ZIP Code) or state (since ZIP
#'     Codes may cross state lines).
#'
#'     For three-digit ZIP Codes, the area and state may or may not correspond to
#'     an individuals' mailing address state (since SCFs cover multiple states).
#'     For example, the three digit ZIP Code \code{010} covers Western Massachusetts
#'     in practice, but is assigned to the state of Connecticut.
#'
#' @return A tibble containing the original data with additional columns from the
#'     selected label data set appended.
#'
#' @examples
#' # create sample data
#' df <- data.frame(
#'   id = c(1:3),
#'   zip5 = c("63005", "63139", "63636"),
#'   zip3 = c("630", "631", "636")
#' )
#'
#' @examplesIf interactive()
#' # UDS crosswalk
#' zi_label(df, input_var = zip5, label_source = "UDS", vintage = 2022)
#'
#' # USPS crosswalk
#' zi_label(df, input_var = zip3, label_source = "USPS", type = "zip3",
#'   vintage = 202408)
#'
#' @examples
#' # custom dictionary
#' ## load sample ZIP3 label data to simulate custom dictionary
#' mo_label <- zi_mo_usps
#'
#' ## label
#' zi_label(df, input_var = zip3, label_source = mo_label, source_var = zip3,
#'   type = "zip3")
#'
#' @export
zi_label <- function(.data, input_var, label_source = "UDS", source_var,
                     type = "zip5", include_scf = FALSE, vintage = 2022){

  # check inputs
  ## determine workflow
  if (inherits(label_source, what = "data.frame")){
    workflow <- "custom"
  } else if (label_source %in% c("UDS", "USPS")){
    workflow <- "api"
  } else {
    cli::cli_abort(c(
      "{.arg label_source} must be {.val UDS}, {.val USPS}, or a data frame.",
      "i" = "You provided {.val {label_source}}."
    ))
  }

  ## checks regardless of workflow
  if (!inherits(.data, what = "data.frame")){
    cli::cli_abort("{.arg .data} must be a data frame.")
  }

  if (missing(input_var)){
    cli::cli_abort("{.arg input_var} is required. Provide the column in {.arg .data} that contains ZIP Code values.")
  }

  input_varQN <- as.character(substitute(input_var))

  if (!(input_varQN %in% names(.data))){
    cli::cli_abort(c(
      "{.arg input_var} was not found in {.arg .data}.",
      "i" = "You provided {.val {input_varQN}}."
    ))
  }

  if (type == "zip5"){
    type_zcta <- "zcta5"
  } else if (type == "zip3"){
    type_zcta <- "zcta3"
  } else {
    cli::cli_abort(c(
      "{.arg type} must be {.val zip5} or {.val zip3}.",
      "i" = "You provided {.val {type}}."
    ))
  }

  valid <- zi_validate(x = .data[[input_varQN]], style = type_zcta)

  if (!valid){
    cli::cli_abort(c(
      "Input ZIP Code data in {.arg {input_varQN}} are invalid.",
      "i" = "Use {.fn zi_validate} with {.code verbose = TRUE} to investigate further."
    ))
  }

  ## checks dependent on workflow
  if (workflow == "custom"){

    if (missing(source_var)){
      cli::cli_abort("{.arg source_var} is required. Provide the column in {.arg label_source} that contains ZIP Code values.")
    }

    source_varQN <- as.character(substitute(source_var))

    if (!(source_varQN %in% names(label_source))){
      cli::cli_abort(c(
        "{.arg source_var} was not found in {.arg label_source}.",
        "i" = "You provided {.val {source_varQN}}."
      ))
    }

    valid <- zi_validate(x = label_source[[source_varQN]], style = type_zcta)

    if (!valid){
      cli::cli_abort(c(
        "Dictionary ZCTA data in {.arg {source_varQN}} are invalid.",
        "i" = "Use {.fn zi_validate} with {.code verbose = TRUE} to investigate further."
      ))
    }

  } else if (workflow == "api"){

    if (label_source == "UDS"){

      if (type == "zip3"){
        cli::cli_abort("{.arg type} must be {.val zip5} when {.arg label_source} is {.val UDS}.")
      }

      if (!is.numeric(vintage)){
        vintage_num <- as.numeric(vintage)
      } else {
        vintage_num <- vintage
      }

      if (!vintage_num %in% c(2009:2022)){
        cli::cli_abort(c(
          "{.arg vintage} must be between {.val 2009} and {.val 2022} when {.arg label_source} is {.val UDS}.",
          "i" = "You provided {.val {vintage}}."
        ))
      }

      if (include_scf){
        cli::cli_warn(c(
          "{.arg include_scf} only affects {.val zip3} labels.",
          "i" = "{.arg type} is {.val {type}}."
        ))
      }

    } else if (label_source == "USPS"){

      if (type == "zip5"){
        cli::cli_abort("{.arg type} must be {.val zip3} when {.arg label_source} is {.val USPS}.")
      }

      if (is.numeric(vintage)){
        vintage_chr <- as.character(vintage)
      } else {
        vintage_chr <- vintage
      }

      labels_list <- zi_load_labels_list(type = "zip3")

      result <- subset(labels_list, date == vintage_chr)

      if (nrow(result) != 1){
        cli::cli_abort(c(
          "{.arg vintage} is not available.",
          "i" = "Use {.fn zi_load_labels_list} to see available vintages."
        ))
      }

    }

  }

  # load label data
  if (workflow == "api"){
    if (label_source == "UDS"){

      dict <- zi_load_labels_uds(year = vintage_num)
      source_varQN <- "zip5"

    } else if (label_source == "USPS") {

      dict <- zi_load_labels_usps(
        type = type,
        include_scf = include_scf,
        vintage = vintage_chr
      )

      source_varQN <- type

    }
  } else if (workflow == "custom"){

    dict <- label_source

  }

  # label data
  ## check for naming conflicts
  dict_names <- names(dict)[names(dict) != source_varQN]

  if (any(dict_names %in% names(.data))){
    cli::cli_warn("Column names in {.arg label_source} conflict with columns in {.arg .data}. Inspect the output carefully.")
  }

  ## join with input data
  join_by <- stats::setNames(source_varQN, input_varQN)
  out <- dplyr::left_join(.data, dict, by = join_by)

  ## create tibble
  out <- tibble::as_tibble(out)

  # return output
  return(out)

}
