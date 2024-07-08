#' Load List of Available Label Datasets
#'
#' @description This function loads a list of available label data sets that can
#'    be used to label ZIP Codes. Currently, only three-digit ZIP Codes are
#'    supported.
#'
#' @param type A character scalar specifying the type of label data to load. The
#'   only supported type is  \code{'zip3'} (three-digit ZIP Codes).
#'
#' @return A tibble containing date values that can be used with \code{zi_load_labels}.
#'
#' @examples
#' \donttest{
#'   zi_load_labels_list(type = "zip3")
#' }
#'
#' @export
zi_load_labels_list <- function(type = "zip3"){

  # check inputs
  if (!type %in% c("zip3")){
    stop("The only 'type' currently supported is 'zip3'.")
  }

  # create output
  ## load list of available label data sets
  labels_list <- read.csv(file = "https://raw.githubusercontent.com/chris-prener/usps-zip-ref/main/data/meta.csv")

  ## subset
  out <- subset(labels_list, type == type)

  ## convert to tibble
  out <- tibble::as_tibble(out)

  # return output
  return(out)

}

#' Load Label Data
#'
#' @description This function loads a specific label data set that can be used to
#'     label three-digit ZIP codes in a data frame.
#'
#' @param source A required character scalar; specifying the source of the label
#'     data. The only supported sources are \code{'UDS'} (default) and
#'     \code{'USPS'}.
#' @param type A required character scalar; one of either  \code{'zip3'} or
#'     \code{'zip5'}. The  \code{'zip3'} type is only available from the 'USPS'
#'     source, while the \code{'zip5'} type is available from \code{'UDS'}.
#' @param vintage A required character or numeric scalar; specifying the date
#'     for \code{source = 'USPS'} or the year of the data for \code{source = 'UDS'}.
#'     The \code{zip_load_labels_list()} function can be used to see available
#'     date values for \code{source = 'USPS'}.
#'
#' @examples
#' \donttest{
#'   # zip5 labels via UDS
#'   zi_load_labels(source = "UDS", type = "zip5", vintage = 2022)
#'
#'   # zip3 labels via USPS
#'   zi_load_labels(source = "USPS", type = "zip3", vintage = 202408)
#' }
#'
#' @export
zi_load_labels <- function(source = "UDS", type = "zip5", vintage = 2022){

  # check inputs
  if (!source %in% c("USPS", "UDS")){
    stop("The only 'source' values currently supported are 'USPS' and 'UDS'.")
  }

  if (!type %in% c("zip3", "zip5")){
    stop("The 'type' must be one of 'zip3' or 'zip5'.")
  }

  if (source == "UDS"){

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

  } else if (source == "USPS"){

    if (type == "zip5"){
      stop("The 'USPS' source only provides ZIP3 data, replace 'type' with 'zip3'.")
    }

    if (is.numeric(vintage) == TRUE){
      vintage_chr <- as.character(vintage)
    } else {
      vintage_chr <- vintage
    }

    labels_list <- zi_load_labels_list(type = "zip3")

    result <- subset(labels_list, vintage == vintage_chr)

    if (nrow(result) != 1){
      stop("The requested vintage is not available. Use 'zi_load_labels_list()' to see available vintages.")
    }

  }

  # load data
  if (source == "UDS"){
    out <- zi_load_uds(year = vintage_num)
    out <- dplyr::select(out, zip5 = ZIP, label_city = PO_NAME, label_state = STATE,
                         label_type = ZIP_TYPE)
  } else if (source == "USPS" & type == "zip3") {
    out <- readr::read_csv(
      paste0("https://raw.githubusercontent.com/chris-prener/usps-zip-ref/main/data/zip3_", vintage_chr, ".csv"),
      col_types = readr::cols(
        zip3 = readr::col_character(),
        scf_id = readr::col_character()
      )
    )

    out <- dplyr::rename(out, label_area = destination_area, label_state = destination_state)

    out <- tibble::as_tibble(out)
  }

  # return output
  return(out)

}
