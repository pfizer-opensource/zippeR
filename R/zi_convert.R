#' Convert Five-digit ZIP Codes to Three-digit ZIP Codes
#'
#' @description This function converts five-digit ZIP Codes to three-digit ZIP
#'     Codes. The first three digits of a ZIP Code are known as the ZIP3 Code,
#'     and corresponds to the sectional center facility (SCF) that processes mail
#'     for a region.
#'
#' @usage zi_convert(.data, input_var, output_var)
#'
#' @param .data A data frame containing a column of five-digit ZIP Codes.
#' @param input_var A character scalar specifying the column name with the five-digit
#'    ZIP Codes in the data frame.
#' @param output_var Optional; A character scalar specifying the column name to
#'    store the three-digit ZIP Codes in the data frame.
#'
#' @return A tibble containing the original data frame with a new column of
#'   three-digit ZIP Codes.
#'
#' @examples
#' # add new column
#' ## create sample data
#' df <- data.frame(id = c(1:3), zip5 = c("63005", "63139", "63636"))
#'
#' ## convert ZIP Codes to ZIP3, creating a new column
#' zi_convert(.data = df, input_var = zip5, output_var = zip3)
#'
#' # overwrite existing column
#' ## create sample data
#' df <- data.frame(id = c(1:3), zip = c("63005", "63139", "63636"))
#'
#' ## convert ZIP Codes to ZIP3, creating a new column
#' zi_convert(.data = df, input_var = zip)
#'
#' @export
zi_convert <- function(.data, input_var, output_var){

  # check inputs
  if (!inherits(.data, what = "data.frame")){
    stop("The '.data' object provided is not a data frame.")
  }

  if (missing(input_var)){
    stop("A value for 'input_var' is required.")
  }

  input_varQN <- as.character(substitute(input_var))

  if (input_varQN %in% names(.data) == FALSE){
    stop("The given 'input_var' column is not found in your data object.")
  }

  valid <- zi_validate(x = .data[[input_varQN]])

  if (valid == FALSE){
    stop(paste0("Input ZIP Code data in the '", input_varQN, "' column are invalid. Please use 'zi_validate()' with the 'verbose = TRUE' option to investigate further. The 'zi_repair()' function may be used to address issues."))
  }

  if (!missing(output_var)){
    output_varQN <- as.character(substitute(input_var))

    if (output_varQN %in% names(.data) == TRUE){
      warning(paste0("The given 'output_var' column, '", output_varQN , "', was found in your data object, and the column was overwritten."))
    }
  } else {
    output_varQN <- input_varQN
  }

  # convert ZIP Codes to ZIP3
  .data[[output_varQN]] <- substr(.data[[input_varQN]], 1, 3)

  # create output
  out <- tibble::as_tibble(.data)

  # return output
  return(out)

}
