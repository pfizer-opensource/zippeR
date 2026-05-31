#' Convert Five-digit ZIP Codes to Three-digit ZIP Codes
#'
#' @description This function converts five-digit ZIP Codes to three-digit ZIP
#'     Codes. The first three digits of a ZIP Code are known as the ZIP3 Code,
#'     and corresponds to the sectional center facility (SCF) that processes mail
#'     for a region.
#'
#'
#' @param .data A data frame containing a column of five-digit ZIP Codes.
#' @param input_var The column in the data frame containing five-digit ZIP Codes,
#'    specified as a bare (unquoted) column name (uses non-standard evaluation).
#' @param output_var Optional; a bare (unquoted) column name to store the
#'    three-digit ZIP Codes. If omitted, the input column is overwritten.
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
    cli::cli_abort("{.arg .data} must be a data frame.")
  }

  if (missing(input_var)){
    cli::cli_abort("{.arg input_var} is required.")
  }

  input_varQN <- as.character(substitute(input_var))

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

  if (!missing(output_var)){
    output_varQN <- as.character(substitute(output_var))

    if (output_varQN %in% names(.data)){
      cli::cli_warn(c(
        "{.arg output_var} already exists and was overwritten.",
        "i" = "The existing column was {.arg {output_varQN}}."
      ))
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
