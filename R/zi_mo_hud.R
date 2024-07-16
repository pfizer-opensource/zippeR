#' Missouri HUD ZIP Code to County Crosswalk, 2023
#'
#' @description A tibble containing the HUD ZIP Code to County Crosswalk file
#'     for Missouri's ZIP Codes in 2023's first quarter.
#'
#' @docType data
#'
#' @usage data(zi_mo_hud)
#'
#' @format A data frame with 1749 rows and 8 variables:
#' \describe{
#'   \item{ZIP}{five-digit United States Postal Service ZIP Code}
#'   \item{GEOID}{five-digit county FIPS code}
#'   \item{RES_RATIO}{for ZIP Codes that cross county boundaries, the proportion
#'       of the ZIP Code's residential customers in the given county}
#'   \item{BUS_RATIO}{for ZIP Codes that cross county boundaries, the proportion
#'       of the ZIP Code's commercial customers in the given county}
#'   \item{OTH_RATIO}{for ZIP Codes that cross county boundaries, the proportion
#'       of the ZIP Code's other customers in the given county}
#'   \item{TOT_RATIO}{for ZIP Codes that cross county boundaries, the proportion
#'       of the ZIP Code's total customers in the given county}
#'   \item{CITY}{United States Postal Service city name}
#'   \item{STATE}{United States Postal Service state abbreviation}
#'   }
#'
#' @details The data included in \code{zi_mo_hud} can be replicated with the
#'   following code: \code{zi_load_crosswalk(zip_source = "HUD", year = 2023,
#'   qtr = 1, target = "COUNTY", query = "MO")}. This assumes your HUD API key
#'   is stored in your \code{.Rprofile} file as \code{hud_key}.
#'
#' @source U.S. Department of Housing and Urban Development's ZIP Code crosswalk
#'     files
#'
#' @examples
#' utils::str(zi_mo_hud)
#' utils::head(zi_mo_hud)
#'
"zi_mo_hud"
