#' Missouri USPS Three-digit ZIP Code Labels, August 2024
#'
#' @description A tibble containing the USPS Three-digit ZIP Code labels for
#'    August 2024.
#'
#' @docType data
#'
#' @usage data(zi_mo_usps)
#'
#' @format A data frame with 37 rows and 3 variables:
#' \describe{
#'   \item{zip3}{three-digit United States Postal Service ZIP Code}
#'   \item{label_area}{area associated with the three-digit ZIP Code}
#'   \item{label_state}{state associated with the three-digit ZIP Code}
#'   }
#'
#' @details The data included in \code{zi_mo_usps} can be replicated with the
#'   following code: \code{zi_load_labels(type = "zip3", source = "USPS",
#'   vintage = 202408)}. After downloading the data, subset to
#'   \code{label_state == "MO"}.
#'
#' @source U.S. Postal Service Facility Access and Shipment Tracking (FAST) Database
#'
#' @examples
#' utils::str(zi_mo_usps)
#' utils::head(zi_mo_usps)
#'
"zi_mo_usps"
