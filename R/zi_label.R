#' Label ZIP Codes with Contextual Data
#'
#' @description This function appends information about the city (for five-digit
#'    ZIP Codes) or area (for three-digit ZIP Codes) to a data frame containing
#'    these values. State is returned for both types of ZIP Codes. The function
#'    also optionally returns data on Sectional Center Facilities (SCFs) for
#'    three-digit ZIP Codes.
#'
#' @usage zi_label(.data, input_var, label_source = "UDS", source_var,
#'     type = "zip5", include_scf = FALSE, vintage = 2022)
#'
#' @param .data An "input object" that is data.frame or tibble that contains
#'     ZIP Codes to be crosswalked.
#' @param input_var The column in the input data that contains five-digit ZIP
#'     Codes. If the input is numeric, it will be transformed to character data
#'     and leading zeros will be added.
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
#' # UDS crosswalk
#' \donttest{
#'   zi_label(df, input_var = zip5, label_source = "UDS", vintage = 2022)
#' }
#'
#' # USPS crosswalk
#' \donttest{
#'   zi_label(df, input_var = zip3, label_source = "USPS", type = "zip3",
#'     vintage = 202408)
#' }
#'
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
    stop("The 'label_source' argument is invalid. Please provide either 'UDS', 'USPS', or a custom dictionary.")
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

  if (type == "zip5"){
    type_zcta <- "zcta5"
  } else if (type == "zip3"){
    type_zcta <- "zcta3"
  } else {
    stop("The 'type' argument must be either 'zip5' or 'zip3'.")
  }

  valid <- zi_validate(x = .data[[input_varQN]], style = type_zcta)

  if (valid == FALSE){
    stop(paste0("Input ZIP Code data in the '", input_varQN, "' column are invalid. Please use 'zi_validate()' with the 'verbose = TRUE' option to investigate further. The 'zi_repair()' function may be used to address issues."))
  }

  ## checks dependent on workflow
  if (workflow == "custom"){

    if (missing(source_var)){
      stop("The 'source_var' argument is missing. Please provide the column name in the dictionary object that contains ZIP Code values.")
    }

    source_varQN <- as.character(substitute(source_var))

    if (source_varQN %in% names(label_source) == FALSE){
      stop("The given 'source_var' column is not found in your dictionary object.")
    }

    valid <- zi_validate(x = label_source[[source_varQN]], style = type_zcta)

    if (valid == FALSE){
      stop(paste0("Dictionary ZCTA data in the '", source_varQN, "' column are invalid. Please use 'zi_validate()' with the 'verbose = TRUE' option to investigate further. The 'zi_repair()' function may be used to address issues."))
    }

  } else if (workflow == "api"){

    if (label_source == "UDS"){

      if (type == "zip3"){
        stop("The 'UDS' source only provides ZIP5 data, replace 'type' with 'zip5'.")
      }

      if (is.numeric(vintage) == FALSE){
        vintage_num <- as.numeric(vintage)
      } else {
        vintage_num <- vintage
      }

      if (!vintage_num %in% c(2009:2022)){
        stop("The 'UDS' source only provides ZIP5 data between 2009 and 2022.")
      }

      if (include_scf == TRUE){
        warning("The 'include_scf' argument only modifies the output of 'zip3' labels.")
      }

    } else if (label_source == "USPS"){

      if (type == "zip5"){
        stop("The 'USPS' source only provides ZIP3 data, replace 'type' with 'zip3'.")
      }

      if (is.numeric(vintage) == TRUE){
        vintage_chr <- as.character(vintage)
      } else {
        vintage_chr <- vintage
      }

      labels_list <- zi_load_labels_list(type = "zip3")

      result <- subset(labels_list, date == vintage_chr)

      if (nrow(result) != 1){
        stop("The requested vintage is not available. Use 'zi_load_labels_list()' to see available vintages.")
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
    warning("The column names in the dictionary object conflict with column names in the input data. Please inspect output carefully.")
  }

  ## join with input data
  out <- merge(x = .data, y = dict, by.x = input_varQN, by.y = source_varQN, all.x = TRUE, all.y = FALSE)

  ## create tibble
  out <- tibble::as_tibble(out)

  # return output
  return(out)

}
