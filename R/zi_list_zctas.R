#' List ZCTA GEOIDs for States
#'
#' @description This function returns a vector of GEOIDs that represent ZCTAs
#'     in and around states, depending on the method selected. The two methods
#'     included described in Details below.
#'
#' @usage zi_list_zctas(year, state, method)
#'
#' @param year A four-digit numeric scalar for year. \code{zippeR} currently
#'     supports data between 2010 and 2021.
#' @param state A scalar or vector with state abbreviations (e.x. \code{"MO"})
#'     or FIPS codes (e.x. \code{29}).
#' @param method A character scalar - either \code{"intersect"} or \code{"centroid"}.
#'     See Details below.
#'
#' @details Since ZCTAs cross state lines, two methods are used to create these
#'     vectors. The \code{"intersect"} method will return ZCTAs that border
#'     the state selected. In most cases, this will result in more ZCTAs
#'     being returned than are actually within the states(s) named in the
#'     \code{state} argument. Conversely, the \code{"centroid"} method will
#'     return only ZCTAs whose centroids (geographical centers) lie within the
#'     states named. In most cases, this will return fewer ZCTAs than
#'     actually lie within the state selected. Users will need to review
#'     their data carefully and, when using other \code{zipperR} functions,
#'     will likely need to use the \code{include} and \code{exclude} arguments
#'     to finalize the geographies returned.
#'
#' @return A vector of GEOIDs representing ZCTAs in and around the state selected.
#'
#' @examples
#' # Missouri ZCTAs, intersect method
#' ## return list
#' mo_zctas <- zi_list_zctas(year = 2021, state = "MO", method = "intersect")
#'
#' ## preview ZCTAs
#' mo_zctas[1:10]
#'
#' # Missouri ZCTAs, centroid method
#' ## return list
#' mo_zctas <- zi_list_zctas(year = 2021, state = "MO", method = "centroid")
#'
#' ## preview ZCTAs
#' mo_zctas[1:10]
#'
#' @export
zi_list_zctas <- function(year, state, method){

  # check inputs
  if (missing(year) == TRUE){
    stop("The 'year' value is missing. Please provide a numeric value between 2010 and 2023.")
  }

  if (is.numeric(year) == FALSE){
    stop("The 'year' value provided is invalid. Please provide a numeric value between years 2010 and 2023.")
  }

  if (year %in% c(2010:2023) == FALSE){
    stop("The 'year' value provided is invalid. Please provide a numeric value between years 2010 and 2023.")
  }

  if (missing(state) == TRUE){
    stop("Please provide a vector of valid state abbreviations for the 'state' argument.")
  }

  if (missing(method) == TRUE){
    stop("Please select a valid method for returning ZCTA values. Your choices are 'centroid' and 'intersect'. See documentation for details.")
  }

  if (method %in% c("centroid", "intersect") == FALSE){
    stop("The two valid methods for returning ZCTA values are 'centroid' and 'intersect'. See documentation for details.")
  }

  # rename args
  statez <- state
  yearz <- year

  # validate
  ## validate state (using tigris workflow)
  statez <- unlist(sapply(statez, validate_state, USE.NAMES=FALSE))

  # subset based on method
  if (method == "centroid"){
    sub <- dplyr::filter(reference_centroids, fips %in% statez == TRUE & year == yearz)
  } else if (method == "intersect"){
    sub <- dplyr::filter(reference_intersects, fips %in% statez == TRUE & year == yearz)
  }

  sub <- sub$obj

  # pull ZCTAs to create output
  if (method == "centroid"){
    out <- changes_centroids[sub]
  } else if (method == "intersect"){
    out <- changes_intersects[sub]
  }

  out <- sort(unique(unlist(out)))

  ## return output
  return(out)

}
