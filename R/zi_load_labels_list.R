#' Load List of Available Label Data Sets
#'
#' @description This function loads a list of available label data sets that can
#'    be used to label ZIP Codes. Currently, only three-digit ZIP Codes are
#'    supported.
#'
#' @usage zi_load_labels_list(type = "zip3")
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
  labels_list <- utils::read.csv(file = "https://raw.githubusercontent.com/chris-prener/usps-zip-ref/main/data/meta.csv")

  ## subset
  out <- subset(labels_list, type == type)

  ## convert to tibble
  out <- tibble::as_tibble(out)

  # return output
  return(out)

}
