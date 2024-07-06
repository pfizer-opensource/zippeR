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
#' ## Missouri ZCTAs, Intersect Method
#' ### Return List
#' mo_zctas <- zi_list_zctas(year = 2021, state = "MO", method = "intersect")
#'
#' ### Print Total Number of ZCTAs Returned
#' length(mo_zctas)
#'
#' ### Preview ZCTAs
#' mo_zctas[1:10]
#'
#' ## Missouri ZCTAs, Centroid Method
#' ### Return List
#' mo_zctas <- zi_list_zctas(year = 2021, state = "MO", method = "centroid")
#'
#' ### Print Total Number of ZCTAs Returned
#' length(mo_zctas)
#'
#' ### Preview ZCTAs
#' mo_zctas[1:10]
#'
#' @export
zi_list_zctas <- function(year, state, method){

  #check inputs non-missing
  if (missing(state) == TRUE & missing(year) == TRUE & missing(method) == TRUE){
    stop("Please provide a vector of data for validation.")
  }

  # global variables
  fips = NULL

  # rename args
  statez <- state
  yearz <- year

  # validate
  ## validate state (using tigris workflow)
  statez <- unlist(sapply(statez, validate_state, USE.NAMES=FALSE))

  if (is.numeric(year) == FALSE){
    stop("The 'year' value provided is invalid. Please provide a numeric value between years 2010 and 2023.")
  }

  if (year %in% c(2010:2023) == FALSE){
    stop("The 'year' value provided is invalid. Please provide a numeric value between years 2010 and 2023.")
  }

  if (method %in% c("centroid", "intersect") == FALSE){
    stop("The two valid methods for returning ZCTA values are 'centroid' and 'intersect'. See documentation for details.")
  }

  ## subset based on method
  if (method == "centroid"){
    sub <- dplyr::filter(reference_centroids, fips %in% statez == TRUE & year == yearz)
  } else if (method == "intersect"){
    sub <- dplyr::filter(reference_intersects, fips %in% statez == TRUE & year == yearz)
  }

  sub <- sub$obj

  ## pull ZCTAs
  if (method == "centroid"){
    out <- changes_centroids[sub]
  } else if (method == "intersect"){
    out <- changes_intersects[sub]
  }

  out <- sort(unique(unlist(out)))

  ## return output
  return(out)

}
