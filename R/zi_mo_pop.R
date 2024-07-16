#' Total Population and Median Household Income, Missouri ZCTAs 2022
#'
#' @description A tibble containing the total population
#'     and median household income estimates from the 2018-2022 5-year
#'     U.S. Census Bureau American Communiy Survey estimates for Missouri
#'     five-digit ZIP Code Tabulation Areas (ZCTAs).
#'
#' @docType data
#'
#' @usage data(zi_mo_pop)
#'
#' @format A data frame with 2664 rows and 4 variables:
#' \describe{
#'   \item{GEOID}{full GEOID string}
#'   \item{variable}{variable, either \code{B01003_001} (total population) or
#'         \code{B19013_001} (median household income)}
#'   \item{estimate}{value for associated \code{variable}}
#'   \item{moe}{margin of error for associated \code{variable}}
#'   }
#'
#' @details The data included in \code{zi_mo_pop} can be replicated with the
#'   following code: \code{zi_get_demographics(year = 2022,
#'   variables = c("B01003_001", "B19013_001"), survey = "acs5")}.
#'
#' @source U.S. Census Bureau American Community Survey
#'
#' @examples
#' utils::str(zi_mo_pop)
#' utils::head(zi_mo_pop)
#'
"zi_mo_pop"
