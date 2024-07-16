#' Convert HUD Crosswalk Data to Finalized Crosswalk
#'
#' @description The output from \code{zi_load_crosswalk()} for HUD data requires
#'     additional processing to be used in the \code{zi_crosswalk()} function.
#'     This function prepares the HUD data for use in joins.
#'
#' @usage zi_prep_hud(.data, by, return_max = TRUE)
#'
#' @param .data The output from \code{zi_load_crosswalk()} with HUD data.
#' @param by Character scalar; the column name to use for identifying the best
#'     match for a given ZIP Code. This could be either \code{"residential"},
#'     \code{"commercial"}, or \code{"total"}.
#' @param return_max Logical scalar; if \code{TRUE} (default), only the county
#'     with the highest proportion of the ZIP Code type will be returned. If the
#'     ZIP Code straddles two states, two records will be returned. If \code{FALSE},
#'     all records for the ZIP Code will be returned. Where a tie exists (i.e.
#'     two counties each contain half of all addresses), the county with the lowest
#'     \code{GEOID} value will be returned.
#'
#' @return A tibble that has been further prepared for use as a crosswalk.
#'
#' @examples
#' # load sample crosswalk data
#' mo_xwalk <- zi_mo_hud
#'
#'   # the above data can be replicated with the following code:
#'   # zi_load_crosswalk(zip_source = "HUD", year = 2023, qtr = 1,
#'   #   target = "COUNTY", query = "MO")
#'
#' # prep crosswalk
#' # when a ZIP Code crosses county boundaries, the portion with the largest
#' # number of residential addresses will be returned
#' zi_prep_hud(mo_xwalk, by = "residential", return_max = TRUE)
#'
#' @export
zi_prep_hud <- function(.data, by, return_max = TRUE){

  # check input data
  if (missing(by) == TRUE){
    stop("A value for 'by' value is missing and is required. Please input either 'residential', 'commercial', or 'total'.")
  }

  if (by %in% c("residential", "commercial", "total") == FALSE){
    stop("The 'by' value provided is invalid. Please input either 'residential', 'commercial', or 'total'.")
  }

  if (is.logical(return_max) == FALSE){
    stop("A logical value must be provided for the 'return_max' argument.")
  }

  ## tidy
  hud <- dplyr::rename_with(.data, tolower)

  if (by == "residential"){
    hud <- dplyr::select(hud, zip5 = zip, geoid, state, ratio = res_ratio)
  } else if (by == "commercial"){
    hud <- dplyr::select(hud, zip5 = zip, geoid, state, ratio = bus_ratio)
  } else if (by == "total"){
    hud <- dplyr::select(hud, zip5 = zip, geoid, state, ratio = tot_ratio)
  }

  # convert state_fips
  state_df <- tigris::states(year = 2022, cb = TRUE, resolution = "20m")
  state_df <- sf::st_set_geometry(state_df, value = NULL)
  state_df <- dplyr::select(state_df, state = STUSPS, state_fips = STATEFP)
  state_df <- dplyr::filter(state_df, as.numeric(state_fips) < 60)

  out <- dplyr::left_join(hud, state_df, by = "state")
  out <- dplyr::select(out, zip5, geoid, state, state_fips, ratio)

  # identify max
  out <- dplyr::arrange(out, zip5, state, geoid)
  out <- dplyr::group_by(out, zip5, state)

  if (return_max == TRUE){
    out <- dplyr::arrange(out, geoid)
    out <- dplyr::filter(out, ratio == max(ratio, na.rm = TRUE))
    out <- dplyr::slice(out, 1)
  } else if (return_max == FALSE){
    out <- dplyr::mutate(out,
                         max = ifelse(ratio == max(ratio, na.rm = TRUE),
                                      TRUE, FALSE)
    )
  }

  out <- dplyr::ungroup(out)

  # subset on max and prep output
  out <- dplyr::filter(out, is.na(state) == FALSE)

  # return output
  return(out)

}
