#' Validate ZIP Code or ZCTA Vector
#'
#' @description This function validates vectors of ZIP Code or ZCTA values. It
#'     is used internally throughout \code{zippeR} for data validation, but
#'     is exported to facilitate troubleshooting.
#'
#' @param x A vector containing ZIP or ZCTA values to be validated.
#' @param style A character scalar - either \code{"zcta5"} (default) or
#'     \code{"zcta3"}.
#' @param verbose A logical scalar; if \code{FALSE} (default), an overall
#'     evaluation will be returned. If \code{TRUE}, a \code{tibble} object
#'     listing validation criteria and results will be returned.
#'
#' @details The \code{zi_validate()} function checks for four conditions:
#' \itemize{
#'  \item{Is the input vector character data? This is important because of USPS's
#'      use of leading zeros in ZIP codes and ZCTAs.}
#'  \item{Are all values five characters (if \code{style = "zcta5"}, the default),
#'      or three characters (if \code{style = "zcta3"})?}
#'  \item{Are any input values over five characters (if \code{style = "zcta5"},
#'      the default), or three characters (if \code{style = "zcta3"})?}
#'  \item{Do any input values have non-numeric characters?}
#' }
#'
#' The questions provide a basis for repairing issues identified with
#' \code{zi_repair()}.
#'
#' @return Either a logical value (if \code{verbose = FALSE}) or a \code{tibble}
#'     containing validation criteria and results.
#'
#' @examples
#' # sample five-digit ZIPs
#' zips <- c("63088", "63108", "63139")
#'
#' # successful validation
#' zi_validate(zips)
#'
#' # sample five-digit ZIPs in data frame
#' zips <- data.frame(id = c(1:3), ZIP = c("63139", "63108", "00501"), stringsAsFactors = FALSE)
#'
#' # successful validation
#' zi_validate(zips$ZIP)
#'
#' # sample five-digit ZIPs with character
#' zips <- c("63088", "63108", "zip")
#'
#' # failed validation
#' zi_validate(zips)
#' zi_validate(zips, verbose = TRUE)
#'
#' @export
zi_validate <- function(x, style = "zcta5", verbose = FALSE){

  # check inputs
  if (missing(x)){
    cli::cli_abort("Please provide a vector of data for validation.")
  }

  if (is.data.frame(x)){
    cli::cli_abort("Please provide a vector of data, instead of a data frame, for validation.")
  }

  if (length(style) != 1){
    cli::cli_abort("{.arg style} must be a single value.")
  }

  if (!(style %in% c("zcta5", "zcta3"))){
    cli::cli_abort(c(
      "{.arg style} must be {.val zcta5} or {.val zcta3}.",
      "i" = "You provided {.val {style}}."
    ))
  }

  if (length(verbose) != 1){
    cli::cli_abort("{.arg verbose} must be a single value.")
  }

  if (!is.logical(verbose)){
    cli::cli_abort(c(
      "{.arg verbose} must be {.val TRUE} or {.val FALSE}.",
      "i" = "You provided {.val {verbose}}."
    ))
  }

  # ensure character
  if (!is.character(x)){
    chr_out <- FALSE
  } else {
    chr_out <- TRUE
  }

  # ensure length and padding
  chr_len <- unique(nchar(x))
  chr_len <- chr_len[!is.na(chr_len)]

  ## inputs are too long
  if (style == "zcta5"){

    if (max(chr_len, na.rm = TRUE) > 5){
      len_out2 <- FALSE
    } else {
      len_out2 <- TRUE
    }

  } else if (style == "zcta3"){

    if (max(chr_len, na.rm = TRUE) > 3){
      len_out2 <- FALSE
    } else {
      len_out2 <- TRUE
    }

  }

  ## inputs are inconsistent
  if (length(chr_len) == 1){

    if (style == "zcta5"){

      if (chr_len != 5){
        len_out3 <- FALSE
      } else {
        len_out3 <- TRUE
      }

    } else if (style == "zcta3"){

      if (chr_len != 3){
        len_out3 <- FALSE
      } else {
        len_out3 <- TRUE
      }

    }

  } else {
    len_out3 <- FALSE
  }

  # check for non-numeric output
  nona <- x[!is.na(x)]
  nonum <- suppressWarnings(which(is.na(as.numeric(nona))))

  if (length(nonum) > 0){
    num_out <- FALSE
  } else {
    num_out <- TRUE
  }

  # create output
  if (!verbose){
    out <- all(c(chr_out, len_out2, len_out3, num_out))
  } else if (verbose){

    if (style == "zcta5"){
      length_prompt <- "No input values are over 5 characters long?"
      style_prompt <- "All input values have 5 characters?"
    } else if (style == "zcta3"){
      length_prompt <- "No input values are over 3 characters long?"
      style_prompt <- "All input values have 3 characters?"
    }

    out <- tibble::as_tibble(data.frame(
      condition = c("Input is a character vector?",
                    style_prompt,
                    length_prompt,
                    "All input values are numeric?"),
      result = c(chr_out, len_out3, len_out2, num_out)
    ))

  }

  # return output
  return(out)

}

#' Repair ZIP Code or ZCTA Vector
#'
#' @description This function repairs two of the four conditions identified
#'     in the validation checks with \code{zi_validate()}. For the other two
#'     conditions, values are converted \code{NA}. See Details below for the
#'     specific changes made.
#'
#' @param x A vector containing ZIP or ZCTA values to be repaired.
#' @param style A character scalar - either \code{"zcta5"} or \code{"zcta3"}.
#'
#' @details The \code{zi_repair()} function addresses four conditions:
#' \itemize{
#'  \item{If the input vector is numeric, it will be converted to character data.}
#'  \item{If there are values less than five characters (if \code{style = "zcta5"},
#'      the default), or three characters (if \code{style = "zcta3"}), they will
#'      be padded with leading zeros.}
#'  \item{If there are input values over five characters (if \code{style = "zcta5"},
#'      the default), or three characters (if \code{style = "zcta3"}), they will
#'      be converted to \code{NA}.}
#'  \item{If there are input values that have non-numeric characters, they will
#'      be converted to \code{NA}.}
#' }
#'
#' Since two of the four steps will result in \code{NA} values, it is strongly
#' recommended to attempt to manually fix these issues first.
#'
#' @return A repaired vector of ZIP or ZCTA values.
#'
#' @examples
#' # sample five-digit ZIPs with character
#' zips <- c("63088", "63108", "zip")
#'
#' # failed validation
#' zi_validate(zips)
#'
#' # repair
#' zips <- zi_repair(zips)
#'
#' # successful validation
#' zi_validate(zips)
#'
#' @export
zi_repair <- function(x, style = "zcta5"){

  # check inputs
  if (missing(x)){
    cli::cli_abort("Please provide a vector of data for validation.")
  }

  if (is.data.frame(x)){
    cli::cli_abort("Please provide a vector of data, instead of a data frame, for validation.")
  }

  if (!(style %in% c("zcta5", "zcta3"))){
    cli::cli_abort(c(
      "{.arg style} must be {.val zcta5} or {.val zcta3}.",
      "i" = "You provided {.val {style}}."
    ))
  }

  # run validation
  valid <- zi_validate(x, style = style, verbose = TRUE)

  if (!all(valid$result)){

    # ensure character
    if (!valid$result[1]){
      x <- as.character(x)
    }

    # identify issue where length is too long
    if (!valid$result[3]){

      if (style == "zcta5"){
        x <- ifelse(nchar(x) > 5, NA, x)
      } else if (style == "zcta3"){
        x <- ifelse(nchar(x) > 3, NA, x)
      }

    }

    # convert characters to NA (only for values that aren't purely digits)
    if (!valid$result[4]){
      x <- ifelse(grepl("^[0-9]+$", x), x, NA_character_)
    }

    # ensure padding
    if (!valid$result[2]){

      if (style == "zcta5"){
        x <- ifelse(!is.na(x), formatC(as.integer(x), width = 5, flag = "0"), NA_character_)
      } else if (style == "zcta3"){
        x <- ifelse(!is.na(x), formatC(as.integer(x), width = 3, flag = "0"), NA_character_)
      }

    }

    # returning warning
    if (!valid$result[3] | !valid$result[4]){
      cli::cli_warn("NAs introduced by coercion.")
    }

  } else {
    cli::cli_inform("This is a valid vector of ZIP or ZCTA codes; nothing to repair.")
  }

  # return output
  return(x)

}
