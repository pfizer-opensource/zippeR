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
  if (missing(x) == TRUE){
    stop("Please provide a vector of data for validation.")
  }

  if (is.data.frame(x) == TRUE){
    stop("Please provide a vector of data, instead of a data frame, for validation.")
  }

  if (style %in% c("zcta5", "zcta3") == FALSE){
    stop("The 'style' value provided is invalid. Please select either 'zcta5' or 'zcta3'.")
  }

  if (is.logical(verbose) == FALSE){
    stop("The 'verbose' value provided is invalid. Please select either 'TRUE' or 'FALSE'.")
  }

  # ensure character
  if (is.character(x) == FALSE){
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
  if (verbose == FALSE){
    out <- all(c(chr_out, len_out2, len_out3, num_out))
  } else if (verbose == TRUE){

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
#'     conditions, values are conveted \code{NA}. See Details below for the
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
  if (missing(x) == TRUE){
    stop("Please provide a vector of data for validation.")
  }

  if (is.data.frame(x) == TRUE){
    stop("Please provide a vector of data, instead of a data frame, for validation.")
  }

  if (style %in% c("zcta5", "zcta3") == FALSE){
    stop("The 'style' value provided is invalid. Please select either 'zcta5' or 'zcta3'.")
  }

  # run validation
  valid <- zi_validate(x, style = style, verbose = TRUE)

  if (all(valid$result) == FALSE){

    # ensure character
    if (valid$result[1] == FALSE){
      x <- as.character(x)
    }

    # identify issue where length is too long
    if (valid$result[3] == FALSE){

      if (style == "zcta5"){
        x <- ifelse(nchar(x) > 5, NA, x)
      } else if (style == "zcta3"){
        x <- ifelse(nchar(x) > 3, NA, x)
      }

    }

    # convert characters to NA
    if (valid$result[4] == FALSE){
      x <- as.character(suppressWarnings(as.numeric(x)))
    }

    # ensure padding
    if (valid$result[2] == FALSE){

      if (style == "zcta5"){
        x <- stringr::str_pad(x, 5, pad = "0")
      } else if (style == "zcta3"){
        x <- stringr::str_pad(x, 3, pad = "0")
      }

    }

    # returning warning
    if (valid$result[3] == FALSE | valid$result[4] == FALSE){
      warning("NAs introduced by coercion")
    }

  } else {
    message("This is a valid vector of ZIP or ZCTA codes - nothing to repair!")
  }

  # return output
  return(x)

}
