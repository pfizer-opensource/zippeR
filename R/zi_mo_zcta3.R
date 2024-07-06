#' Missouri Three-digit ZCTAs, 2022
#'
#' @description A simple features data set containing the geometric data
#'     for Missouri's three-digit ZIP Code Tabulation Areas (ZCTAs) for 2022,
#'     derived from the U.S. Census Bureau's 2022 TIGER/Line shapefiles.
#'
#' @docType data
#'
#' @usage data(zi_mo_zcta3)
#'
#' @format A data frame with 31 rows and 2 variables:
#' \describe{
#'   \item{ZCTA3}{three-digit ZCTA value}
#'   \item{geometry}{simple features geometry}
#'   }
#'
#' @source U.S. Census Bureau's TIGER/Line database
#'
#' @examples
#' utils::str(zi_mo_zcta3)
#' utils::head(zi_mo_zcta3)
#'
"zi_mo_zcta3"
