#' Convert HUD Crosswalk Data to Finalized Crosswalk
#'
#' @description The output from \code{zi_load_crosswalk()} for HUD data requires
#'     additional processing to be used in the \code{zi_crosswalk()} function.
#'     This function prepares the HUD data for use in joins.
#'
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
  if (missing(by)){
    cli::cli_abort("{.arg by} is required. Please provide {.val residential}, {.val commercial}, or {.val total}.")
  }

  if (!(by %in% c("residential", "commercial", "total"))){
    cli::cli_abort(c(
      "{.arg by} must be {.val residential}, {.val commercial}, or {.val total}.",
      "i" = "You provided {.val {by}}."
    ))
  }

  if (!is.logical(return_max)){
    cli::cli_abort(c(
      "{.arg return_max} must be {.val TRUE} or {.val FALSE}.",
      "i" = "You provided {.val {return_max}}."
    ))
  }

  # validate .data schema
  required_cols <- c("zip", "geoid", "state", "res_ratio", "bus_ratio", "tot_ratio")
  data_cols <- tolower(names(.data))
  missing_cols <- setdiff(required_cols, data_cols)
  if (length(missing_cols) > 0){
    cli::cli_abort(c(
      "{.arg .data} is missing required columns: {.val {missing_cols}}.",
      "i" = "Expected HUD crosswalk output from {.fn zi_load_crosswalk}."
    ))
  }

  ## tidy
  hud <- dplyr::rename_with(.data, tolower)

  if (by == "residential"){
    hud <- stats::setNames(hud[, c("zip", "geoid", "state", "res_ratio")],
                     c("zip5", "geoid", "state", "ratio"))
  } else if (by == "commercial"){
    hud <- stats::setNames(hud[, c("zip", "geoid", "state", "bus_ratio")],
                     c("zip5", "geoid", "state", "ratio"))
  } else if (by == "total"){
    hud <- stats::setNames(hud[, c("zip", "geoid", "state", "tot_ratio")],
                     c("zip5", "geoid", "state", "ratio"))
  }

  # convert state_fips using static lookup (avoids network download)
  state_df <- states_lookup[as.numeric(states_lookup$fips) < 60, c("abb", "fips")]
  names(state_df) <- c("state", "state_fips")
  state_df$state <- toupper(state_df$state)

  out <- left_join_base(hud, state_df, by = "state")
  out <- out[, c("zip5", "geoid", "state", "state_fips", "ratio")]

  # identify max
  out <- out[order(out$zip5, out$state, out$geoid), ]
  out <- dplyr::group_by(out, zip5, state)

  # ave() groups by combining vectors via split(); unlike dplyr::group_by(),
  # split() drops NA keys into per-row singleton groups instead of grouping
  # them together. Coercing to factors with exclude = NULL preserves NA as a
  # real grouping level, matching dplyr's grouped max() semantics exactly.
  # These must be (re)computed against out's row order at the point ave()
  # is called, since return_max reorders out by geoid beforehand.

  if (return_max){
    out <- out[order(out$geoid), ]
    zip5_grp <- factor(out$zip5, exclude = NULL)
    state_grp <- factor(out$state, exclude = NULL)
    grp_max <- stats::ave(out$ratio, zip5_grp, state_grp,
                    FUN = function(x) max(x, na.rm = TRUE))
    out <- out[!is.na(out$ratio) & out$ratio == grp_max, ]
    out <- dplyr::slice(out, 1)
  } else if (!return_max){
    zip5_grp <- factor(out$zip5, exclude = NULL)
    state_grp <- factor(out$state, exclude = NULL)
    grp_max <- stats::ave(out$ratio, zip5_grp, state_grp,
                    FUN = function(x) max(x, na.rm = TRUE))
    out$max <- ifelse(out$ratio == grp_max, TRUE, FALSE)
  }

  out <- dplyr::ungroup(out)

  # subset on max and prep output
  out <- out[!is.na(out$state), ]

  # return output
  return(out)

}
