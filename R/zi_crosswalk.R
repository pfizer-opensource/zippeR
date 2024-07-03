#' Crosswalk ZIP Codes with ZCTA Dictionary
#'
#' @description This function compares input data containing ZIP Codes with
#'     a crosswalk file that will append ZCTAs. This is an important step because
#'     not all ZIP Codes have the same five digits as their enclosing ZCTA.
#'
#' @param .data An "input object" that is data.frame or tibble that contains
#'     ZIP Codes to be crosswalked.
#' @param input_zip The column in the input data that contains five-digit ZIP
#'     Codes. If the input is numeric, it will be transformed to character data
#'     and leading zeros will be added.
#' @param dict A "dictionary object" that contains ZIP Code to ZCTA crosswalks.
#'     This can either be an object in the user's global environment or,
#'     alternatively, a string that corresponds to the vintage of UDS Mapper crosswalk
#'     files to be used. Valid strings are formatted with \code{"UDS"} followed by
#'     a year between 2010 and 2021 so that a complete input looks like
#'     \code{dict = "UDS 2020"}. See \code{zi_load_crosswalk()} for additional
#'     information and to preview these objects.
#' @param dict_zip The name of the column in the "dictionary object" that contains
#'     ZIP Code data. This is \code{"ZIP"} by default, but can be changed optionally
#'     to another name.
#' @param dict_zcta The name of the column in the "dictionary object" that contains
#'     ZCTA data. This is \code{"ZCTA"} by default, but can be changed optionally
#'     to another name. This could be either a three or five digit ZCTA based
#'     on the \code{style} argument.
#' @param style A character scalar - either \code{"zcta5"} or \code{"zcta3"}.
#'     This applies only to the \code{dict_zcta} argument.
#' @param zip_source A character scalar that specifies the source of ZIP Code crosswalk
#'     data. This can be one of either \code{"UDS"} or \code{"HUD"}. The default is
#'     \code{"UDS"}.
#'
#' @return A \code{tibble} with ZCTAs appended.
#'
#' @export
zi_crosswalk <- function(.data, input_zip, dict = "UDS 2021", dict_zip = "ZIP",
                         dict_zcta = "ZCTA", style = "zcta5", zip_source = "UDS"){

  # check inputs
  if (missing(.data) == TRUE & missing(input_zip) == TRUE){
    stop("Please specify arguments.")
  }

  # optionally pull UDC crosswalk
  if (inherits(dict, what = "data.frame") == FALSE){

    ## pull year
    yr <- as.numeric(stringr::word(dict, 2))

    ## validate year
    if (yr %in% c(2010:2022) == FALSE){
      stop("The 'dict' value provided is invalid. Please provide a value between 'UDS 2010' and 'UDS 2022'.")
    }

    ## load dictionary object
    dict <- zi_load_crosswalk(year = yr, zip_source = zip_source)

  }

  # nse
  in_zipQN <- as.character(substitute(input_zip))
  dict_zipQN <- as.character(substitute(dict_zip))
  dict_zctaQN <- as.character(substitute(dict_zcta))

  # verify columns
  if (in_zipQN %in% names(.data) == FALSE){
    stop("The given 'input_zip' column is not found in your input object.")
  }

  if (dict_zipQN %in% names(dict) == FALSE){
    stop("The given 'dict_zip' column is not found in your dictionary object.")
  }

  if (dict_zctaQN %in% names(dict) == FALSE){
    stop("The given 'dict_zcta' column is not found in your dictionary object.")
  }

  # verify formatting for ZIPs
  valid <- zi_validate(x = .data[[in_zipQN]])

  if (valid == FALSE){
    stop(paste0("Input ZIP Code data in the '", in_zipQN, "' column are invalid. Please use 'zi_validate()' with the 'verbose = TRUE' option to investigate further. The 'zi_repair()' function may be used to address issues."))
  }

  valid <- zi_validate(x = dict[[dict_zipQN]])

  if (valid == FALSE){
    stop(paste0("Dictionary ZIP Code data in the '", dict_zipQN, "' column are invalid. Please use 'zi_validate()' with the 'verbose = TRUE' option to investigate further. The 'zi_repair()' function may be used to address issues."))
  }

  valid <- zi_validate(x = dict[[dict_zctaQN]], style = style)

  if (valid == FALSE & style == "zcta5"){
    stop(paste0("Dictionary ZCTA data in the '", dict_zctaQN, "' column are invalid. Please use 'zi_validate()' with the 'verbose = TRUE' option to investigate further. The 'zi_repair()' function may be used to address issues."))
  }

  if (valid == FALSE & style == "zcta3"){

    dict[[dict_zctaQN]] <- substr(dict[[dict_zctaQN]], 1, 3)

    valid <- zi_validate(x = dict[[dict_zctaQN]], style = style)

    if (valid == FALSE){
      stop(paste0("Dictionary ZCTA data in the '", dict_zctaQN, "' column are invalid. Please use 'zi_validate()' with the 'verbose = TRUE' option to investigate further. The 'zi_repair()' function may be used to address issues."))
    } else if (valid == TRUE){
      message("Dictionary five-digit ZCTAs converted to three-digit ZCTAs.")
      dict <- dplyr::rename(dict, "ZCTA3" = dplyr::all_of(dict_zctaQN))
      dict_zctaQN <- "ZCTA3"
    }

  }

  # subset dictionary
  dict <- dplyr::select(dict, dplyr::any_of(c(dict_zipQN, dict_zctaQN)))

  # join with input data
  out <- dplyr::left_join(.data, dict, by = stats::setNames(dict_zipQN, in_zipQN))

  # create tibble
  out <- tibble::as_tibble(out)

  # return output
  return(out)

}
