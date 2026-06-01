#' List ZCTA GEOIDs for States
#'
#' @description This function returns a vector of GEOIDs that represent ZCTAs
#'     in and around states, depending on the method selected. The two methods
#'     included described in Details below.
#'
#'
#' @param year A four-digit numeric scalar for year. \code{zippeR} currently
#'     supports data between 2010 and 2024.
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
#' @examplesIf interactive()
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
  if (missing(year)){
    cli::cli_abort("{.arg year} is required. Please provide a numeric value between {.val 2010} and {.val 2024}.")
  }

  if (!is.numeric(year)){
    cli::cli_abort(c(
      "{.arg year} must be numeric.",
      "i" = "You provided {.val {year}}."
    ))
  }

  if (!(year %in% c(2010:2024))){
    cli::cli_abort(c(
      "{.arg year} must be between {.val 2010} and {.val 2024}.",
      "i" = "You provided {.val {year}}."
    ))
  }

  if (year == 2024){
    cli::cli_abort(c(
      "{.arg year} {.val 2024} is not yet available for {.fn zi_list_zctas}.",
      "i" = "Use {.val 2023} or earlier. Support for {.val 2024} will be added in a future release."
    ))
  }

  if (missing(state)){
    cli::cli_abort("{.arg state} is required.")
  }

  if (missing(method)){
    cli::cli_abort("{.arg method} is required. Choose {.val centroid} or {.val intersect}.")
  }

  if (!(method %in% c("centroid", "intersect"))){
    cli::cli_abort(c(
      "{.arg method} must be {.val centroid} or {.val intersect}.",
      "i" = "You provided {.val {method}}."
    ))
  }

  # rename args
  statez <- state
  yearz <- year

  # validate
  ## validate state (using tigris workflow)
  statez <- unlist(sapply(statez, validate_state, USE.NAMES = FALSE))

  if (is.null(statez) || length(statez) == 0){
    cli::cli_abort(c(
      "No valid states found in {.arg state}.",
      "i" = "Provide valid state abbreviations or FIPS codes."
    ))
  }

  # subset based on method
  if (method == "centroid"){
    sub <- dplyr::filter(reference_centroids, fips %in% statez & year == yearz)
  } else if (method == "intersect"){
    sub <- dplyr::filter(reference_intersects, fips %in% statez & year == yearz)
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
