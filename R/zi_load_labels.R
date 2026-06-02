#' Load Label Data
#'
#' @description This function loads a specific label data set that can be used to
#'     label five or three-digit ZIP codes in a data frame.
#'
#'
#' @param source A required character scalar; specifies the source of the label
#'     data. The only supported sources are \code{'UDS'} (default) and
#'     \code{'USPS'}.
#' @param type A required character scalar; one of either  \code{'zip3'} or
#'     \code{'zip5'}. The  \code{'zip3'} type is only available from the \code{'USPS'}
#'     source, while the \code{'zip5'} type is available from \code{'UDS'}.
#' @param include_scf A logical scalar required when \code{source = 'USPS'} and
#'     \code{type = 'zip3'}; specifying whether to include the SCF (Sectional
#'     Center Facility) ID in the output. The default is \code{FALSE}.
#' @param vintage A required character or numeric scalar; specifying the date
#'     for \code{source = 'USPS'} or the year of the data for \code{source = 'UDS'}.
#'     The \code{zip_load_labels_list()} function can be used to see available
#'     date values for \code{source = 'USPS'}.
#'
#' @return A tibble with the specified label data for either five or three-digit
#'     ZIP Codes.
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
#' @examplesIf interactive()
#'   # zip5 labels via UDS
#'   zi_load_labels(source = "UDS", type = "zip5", vintage = 2022)
#'
#'   # zip3 labels via USPS
#'   zi_load_labels(source = "USPS", type = "zip3", vintage = 202408)
#'
#' @export
zi_load_labels <- function(source = "UDS", type = "zip5", include_scf = FALSE,
                           vintage = 2022){

  # check inputs
  if (!source %in% c("USPS", "UDS")){
    cli::cli_abort(c(
      "{.arg source} must be {.val USPS} or {.val UDS}.",
      "i" = "You provided {.val {source}}."
    ))
  }

  if (!type %in% c("zip3", "zip5")){
    cli::cli_abort(c(
      "{.arg type} must be {.val zip3} or {.val zip5}.",
      "i" = "You provided {.val {type}}."
    ))
  }

  if (source == "UDS"){

    if (type == "zip3"){
      cli::cli_abort("{.arg type} must be {.val zip5} when {.arg source} is {.val UDS}.")
    }

    if (!is.numeric(vintage)){
      vintage_num <- as.numeric(vintage)
    } else {
      vintage_num <- vintage
    }

    if (!vintage_num %in% c(2009:2022)){
      cli::cli_abort(c(
        "{.arg vintage} must be between {.val 2009} and {.val 2022} when {.arg source} is {.val UDS}.",
        "i" = "You provided {.val {vintage}}."
      ))
    }

    if (include_scf){
      cli::cli_warn(c(
        "{.arg include_scf} only affects {.val zip3} labels.",
        "i" = "{.arg type} is {.val {type}}."
      ))
    }

  } else if (source == "USPS"){

    if (type == "zip5"){
      cli::cli_abort("{.arg type} must be {.val zip3} when {.arg source} is {.val USPS}.")
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

  # load data
  if (source == "UDS"){
    out <- zi_load_labels_uds(year = vintage_num)
  } else if (source == "USPS") {
    out <- zi_load_labels_usps(
      type = type,
      include_scf = include_scf,
      vintage = vintage_chr
    )
  }

  # return output
  return(out)

}

# sub functions for zi_load_labels

zi_load_labels_uds <- function(year){

  # load UDS crosswalk
  out <- zi_load_uds(year = year)

  # format output
  out <- dplyr::select(out, zip5 = ZIP, label_city = PO_NAME, label_state = STATE,
                       label_type = ZIP_TYPE)

  # return output
  return(out)

}

zi_load_labels_usps <- function(type, include_scf, vintage){

  # load USPS crosswalk
  if (type == "zip3"){

    ## read from GitHub
    out <- tryCatch(
      utils::read.csv(
        paste0("https://raw.githubusercontent.com/chris-prener/usps-zip-ref/main/data/zip3_", vintage, ".csv"),
        colClasses = c(zip3 = "character", scf_id = "character"),
        stringsAsFactors = FALSE
      ),
      error = function(e) {
        cli::cli_abort(c(
          "x" = "Failed to download USPS label data from GitHub.",
          "i" = "Original error: {conditionMessage(e)}"
        ))
      }
    )
    out <- tibble::as_tibble(out)

    ## rename cols
    out <- dplyr::rename(out, label_area = destination_area, label_state = destination_state)

    ## optionally remove scf cols
    if (!include_scf){
      out <- subset(out, select = -c(scf_id, scf_name, scf_state))
    }

    ## format output
    out <- tibble::as_tibble(out)
  }

  # return output
  return(out)

}
