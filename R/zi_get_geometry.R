#' Download and Optionally Geoprocess ZCTAs
#'
#' @description This function returns geometric data for ZIP Code Tabulation
#'     Areas (ZCTAs), which are rough approximations of many (but not all)
#'     USPS ZIP codes. Downloading and processing these data will be heavily
#'     affected by your internet connection, your choice for the \code{cb}
#'     argument, and the processing power of your computer (if you select
#'     specific counties).
#'
#'
#' @param year A four-digit numeric scalar for year. \code{zippeR} currently
#'     supports data between 2010 and 2024
#' @param style A character scalar - either \code{"zcta5"} or \code{"zcta3"}.
#'     See Details below.
#' @param return A character scalar; if \code{"id"} (default), only the five-digit
#'     number of each ZCTA (or three-digit if \code{style = "zcta3"}) is returned.
#'     This is the only valid option for  \code{style = "zcta3"}. For
#'     \code{style = "zcta5"}, if \code{return = "full"}, all TIGER/Line columns
#'     are returned.
#' @param class A character scalar; if \code{"sf"} (default), a \code{sf} object
#'     suitable for mapping will be returned. If \code{"tibble"}, an object
#'     that omits the geometric data will be returned instead.
#' @param state A character scalar or vector with character state abbreviations
#'     (e.x. \code{"MO"}) or numeric FIPS codes (e.x. \code{29}). ZCTAs that
#'     are within the given states (determined based on a combination of
#'     \code{year} and \code{method}) will be returned. See Details below for
#'     more information. This argument is optional unless a argument is also
#'     specified for \code{county}.
#' @param county A character scalar or vector with character GEOIDs (e.x.
#'     \code{"29510"}). ZCTAs that are within the given states (determined based
#'     on a combination of \code{year} and \code{method}) will be returned. See
#'     Details below for more information. This argument is optional.
#' @param territory A character scalar or vector with character territory abbreviations
#'     (e.x. \code{"PR"}) or numeric FIPS codes (e.x. \code{72}). ZCTAs that are
#'     within the given territories will be returned. By default, all territories
#'     are excluded. The five territory abbreviations are: \code{"AS"} (American
#'     Samoa), \code{"GU"} (Guam), \code{"MP"} (Northern Mariana Islands),
#'     \code{"PR"} (Puerto Rico), and \code{"VI"} (U.S. Virgin Islands).
#' @param cb A logical scalar; if \code{FALSE}, the most detailed TIGER/Line
#'     data will be used for \code{style = "zcta5"}. If \code{TRUE}, a
#'     generalized (1:500k) version of the data will be used. The generalized
#'     data will download significantly faster, though they show less detail.
#'     According to the \code{tigris::zctas()} documentation, the download size
#'     if \code{TRUE} is ~65MB while it is ~500MB if \code{cb = FALSE}.
#'
#'     This argument does not apply to \code{style = "zcta3"}, which only returns
#'     generalized data. It also does not apply if \code{class = "tibble"}.
#' @param starts_with A character scalar or vector containing the first two
#'     digits of a GEOID or ZCTA3 value to return. It defaults to \code{NULL},
#'     which will return all ZCTAs in the US. For example, supplying the argument
#'     \code{starts_with = c("63", "64")} will return only those ZCTAs or ZCTA3s
#'     that begin with 63 or 64. If you supply a state or a county, that will limit
#'     the data this argument is applied to, potentially leading to missed ZCTAs.
#' @param includes A character scalar or vector containing GEOID's or ZCTA3 values
#'     to include when finalizing output. This may be necessary depending on what
#'     is identified with the \code{method} argument.
#' @param excludes A character scalar or vector containing GEOID's or ZCTA3 values
#'     to exclude when finalizing output. This may be necessary depending on what
#'     is identified with the \code{method} argument.
#' @param method A character scalar - either \code{"intersect"} or \code{"centroid"}.
#'     See Details below.
#' @param shift_geo A logical scalar; if \code{TRUE}, Alaska, Hawaii, and Puerto Rico
#'     will be re-positioned so that the lie to the southwest of the continental
#'     United States. This defaults to \code{FALSE}, and can only be used when
#'     states are not listed for the \code{state} argument. It does not apply
#'     if \code{class = "tibble"}.
#'
#' @details This function contains options for both the type of ZCTA and,
#'     optionally, for how state and county data are identified. For type,
#'     either five-digit or three-digit ZCTA geometries are available. The
#'     three-digit ZCTAs were created by geoprocessing the five-digit boundaries
#'     for each year, and then applying a modest amount of simplification
#'     (with \code{sf::st_simplify()}) to reduce file size. The source files
#'     are available on GitHub at \url{https://github.com/chris-prener/zcta3}.
#'
#'     Since ZCTAs cross state lines, two methods are used to create these
#'     geometry data for years 2012 and beyond for states and all years for counties.
#'     The \code{"intersect"} method  will return ZCTAs that border the states or
#'     counties selected. In most  cases, this will result in more ZCTAs being
#'     returned than are actually within the states or counties selected.
#'     Conversely, the \code{"centroid"} method will return only ZCTAs whose
#'     centroids (geographical centers) lie within the states or counties named.
#'     In most cases, this will return fewer ZCTAs than actually lie within the
#'     states or counties selected. Users will need to review their data carefully
#'     and will likely need to use the \code{include} and \code{exclude} arguments
#'     to finalize the geographies returned.
#'
#'     For state-level data in 2010 and 2011, the Census Bureau published individual
#'     state files that will be utilized automatically by \code{zippeR}. If
#'     county-level data are requested for these years, the state-specific file
#'     will be used as a base before identifying ZCTAs within counties using
#'     either the \code{"intersect"} or \code{"centroid"} method described above.
#'
#' @return A \code{sf} object (or \code{tibble} if \code{class = "tibble"})
#'     with ZCTAs matching the parameters specified above: either a nationwide
#'     file, a specific state or states, or a specific county or counties.
#'     Returns \code{NULL} if the Census Bureau download fails or if state
#'     validation yields no matching ZCTAs.
#'
#' @examplesIf interactive()
#'   # five-digit ZCTAs
#'   ## download all ZCTAs for 2020 including territories
#'   zi_get_geometry(year = 2020, territory = c("AS", "GU", "MP", "PR", "VI"),
#'       shift_geo = TRUE)
#'
#'   ## download all ZCTAs for 2020 excluding territories
#'   zi_get_geometry(year = 2020, shift_geo = TRUE)
#'
#'   ## download all ZCTAs in a selection of states, intersects method
#'   zi_get_geometry(year = 2020, state = c("IA", "IL", "MO"), method = "intersect")
#'
#'   ## download all ZCTAs in a single county - St. Louis City, MO
#'   zi_get_geometry(year = 2020, state = "MO", county = "29510",
#'       method = "intersect")
#'
#'   # three-digit ZCTAs
#'   ## download all ZCTAs for 2018 including territories
#'   zi_get_geometry(year = 2018, territory = c("AS", "GU", "MP", "PR", "VI"),
#'       shift_geo = TRUE)
#'
#' @export
zi_get_geometry <- function(year, style = "zcta5", return = "id", class = "sf",
                            state = NULL, county = NULL, territory = NULL,
                            cb = FALSE, starts_with = NULL, includes = NULL,
                            excludes = NULL, method = NULL, shift_geo = FALSE){

  # check inputs
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

  if (!(style %in% c("zcta5", "zcta3"))){
    cli::cli_abort(c(
      "{.arg style} must be {.val zcta5} or {.val zcta3}.",
      "i" = "You provided {.val {style}}."
    ))
  }

  if (!(class %in% c("sf", "tibble"))){
    cli::cli_abort(c(
      "{.arg class} must be {.val sf} or {.val tibble}.",
      "i" = "You provided {.val {class}}."
    ))
  }

  if (!(return %in% c("id", "full"))){
    cli::cli_abort(c(
      "{.arg return} must be {.val id} or {.val full}.",
      "i" = "You provided {.val {return}}."
    ))
  }

  if (style == "zcta3" & return == "full"){
    cli::cli_warn(c(
      "{.arg return} cannot be {.val full} when {.arg style} is {.val zcta3}.",
      "i" = "Use {.val id} instead."
    ))
  }

  if (style == "zcta3" & cb){
    cli::cli_warn(c(
      "{.arg cb} does not apply when {.arg style} is {.val zcta3}.",
      "i" = "You provided {.val {cb}}."
    ))
  }

  if (!is.logical(shift_geo)){
    cli::cli_abort(c(
      "{.arg shift_geo} must be {.val TRUE} or {.val FALSE}.",
      "i" = "You provided {.val {shift_geo}}."
    ))
  }

  if (shift_geo & !is.null(state)){
    cli::cli_abort("{.arg shift_geo} can only be used when returning data for all states.")
  }

  if (any(state %in% c("AS", "GU", "MP", "PR", "VI"))){
    cli::cli_abort(c(
      "Territories must be supplied with {.arg territory}, not {.arg state}.",
      "i" = "Valid territories are {.val AS}, {.val GU}, {.val MP}, {.val PR}, and {.val VI}, or their equivalent FIPS codes."
    ))
  }

  if (!is.null(state)){
    state <- unlist(sapply(state, validate_state, USE.NAMES = FALSE))
  }

  if (!is.null(county) & is.null(state)){
    cli::cli_abort("{.arg state} is required when {.arg county} is supplied.")
  }

  if (!is.null(state) & missing(method)){
    cli::cli_abort("{.arg method} is required. Choose {.val centroid} or {.val intersect}.")
    }

  if (!is.null(method)){
    if (!(method %in% c("centroid", "intersect"))){
      cli::cli_abort(c(
        "{.arg method} must be {.val centroid} or {.val intersect}.",
        "i" = "You provided {.val {method}}."
      ))
    }
  }

  ## validate counties
  if (!is.null(territory)){
    ## normalize FIPS codes to abbreviations
    territory_fips <- c("60" = "AS", "66" = "GU", "69" = "MP", "72" = "PR", "78" = "VI")
    territory <- sapply(territory, function(t) {
      t_str <- as.character(t)
      if (t_str %in% names(territory_fips)) territory_fips[[t_str]] else t
    }, USE.NAMES = FALSE)

    if (!all(territory %in% c("AS", "GU", "MP", "PR", "VI"))){
      cli::cli_abort(c(
        "{.arg territory} contains an invalid value.",
        "i" = "Use one or more of {.val AS}, {.val GU}, {.val MP}, {.val PR}, or {.val VI}, or their equivalent FIPS codes."
      ))
    }
  }

  if (!is.null(starts_with)){
    valid <- zi_validate_starts(starts_with)

    if (!valid){
      cli::cli_abort("{.arg starts_with} must be a character vector of two-digit values.")
    }
  }

  if (!is.null(includes)){
    valid <- zi_validate(includes, style = style)

    if (!valid){
      cli::cli_abort(c(
        "{.arg includes} contains invalid ZCTA values.",
        "i" = "Use {.fn zi_validate} with {.code verbose = TRUE} to investigate further."
      ))
    }
  }

  if (!is.null(excludes)){
    valid <- zi_validate(excludes, style = style)

    if (!valid){
      cli::cli_abort(c(
        "{.arg excludes} contains invalid ZCTA values.",
        "i" = "Use {.fn zi_validate} with {.code verbose = TRUE} to investigate further."
      ))
    }
  }

  # check year
  if (year == 2011){
    cli::cli_inform(c(
      "i" = "{.arg year} {.val 2011} is not available; using {.val 2010} data instead.",
      "i" = "The Census Bureau did not publish separate 2011 ZCTA boundaries."
    ))
    year <- 2010
  }

  # call sub functions
  if (style == "zcta5"){

    out <- zi_get_zcta5(year = year, return = return, state = state,
                        county = county, territory = territory, cb = cb,
                        starts_with = starts_with,
                        includes = includes, excludes = excludes,
                        method = method)

  } else if (style == "zcta3"){

    out <- zi_get_zcta3(year = year, state = state,
                        county = county, territory = territory, cb = cb,
                        starts_with = starts_with,
                        includes = includes, excludes = excludes,
                        method = method)

  }

  # finalize output
  if (!is.null(out)){
    if (class == "sf" & shift_geo){

      ## shift geometry
      out <- tigris::shift_geometry(out, position = "below")

    }

    if (class == "tibble"){

      ## remove geometry
      sf::st_geometry(out) <- NULL

      ## finalize tibble
      out <- tibble::as_tibble(out)

    }
  }

  # return output
  return(out)

}

## Sub Function for ZCTA5
zi_get_zcta5 <- function(year, return = "id", state, county, territory, cb,
                         starts_with, includes, excludes, method){

  # global variables
  GEOID10 = GEOID20 = GEOID = NULL

  # tigris call - TAG
  out <- zi_get_tigris(.f = "zctas", year = year, state = NULL, cb = cb)

  # process geometry
  if (!is.null(out)){
    if (!is.null(state) & is.null(county)) {

      ## generate vector of requested state ZCTAs
      zcta_vec <- zi_list_zctas(year = year, state = c(state, territory), method = method)

      ## add inclusions, remove exclusions
      zcta_vec <- unique(c(zcta_vec, includes))
      zcta_vec <- zcta_vec[!(zcta_vec %in% excludes)]

      ## rename year
      if (year < 2020){
        out <- dplyr::rename(out, GEOID = GEOID10)
      } else if (year >= 2020){
        out <- dplyr::rename(out, GEOID = GEOID20)
      }

      ## subset
      out <- dplyr::filter(out, GEOID %in% zcta_vec)

    } else if (!is.null(state) & !is.null(county)){

      ## geoprocess based on county to produced vector of ZCTAs
      zcta_vec <- zi_process_county(cb = cb, state = c(state, territory), county = county,
                                    year = year, zcta = out, method = method,
                                    style = "zcta5")

      if (!is.null(zcta_vec)){
        ## add inclusions, remove exclusions
        zcta_vec <- unique(c(zcta_vec, includes))
        zcta_vec <- zcta_vec[!(zcta_vec %in% excludes)]

        ## rename year
        if (year < 2020){
          out <- dplyr::rename(out, GEOID = GEOID10)
        } else if (year >= 2020){
          out <- dplyr::rename(out, GEOID = GEOID20)
        }

        ## subset
        out <- dplyr::filter(out, GEOID %in% zcta_vec)
      } else {
        out <- NULL
      }

    } else if (is.null(state) & is.null(county)){

      ## rename year
      if (year < 2020){
        out <- dplyr::rename(out, GEOID = GEOID10)
      } else if (year >= 2020){
        out <- dplyr::rename(out, GEOID = GEOID20)
      }

      ## manage territories
      if (is.null(territory)){

        ## all territories not including American Samoa
        out <- dplyr::filter(out, !(substr(GEOID, 1, 3) %in% c("006", "007", "008", "009", "969")))

        ## American Samoa
        out <- dplyr::filter(out, GEOID != "96799")

      } else if (!is.null(territory)){

        ## territory vector
        territory_vec <- c("AS", "GU", "MP", "PR", "VI")

        if (!all(territory == territory_vec)){

          ## construct list
          territory_vec <- territory_vec[!(territory_vec %in% territory)]

          ## create vector
          zcta_vec <- zi_list_zctas(year = year, state = territory_vec, method = "intersect")

          ## append to excludes
          excludes <- unique(sort(c(excludes, zcta_vec)))

        }
      }

      ## subset
      if (!is.null(excludes)){
        out <- dplyr::filter(out, !(GEOID %in% excludes))
      }

    }

    # subset based on starts with
    if (!is.null(out)){
      if (!is.null(starts_with)){
        out <- dplyr::filter(out, substr(GEOID, 1, 2) %in% starts_with)
      }

      # subset columns based on return
      if (return == "id"){
        out <- dplyr::select(out, GEOID)
      }

      # order output
      out <- dplyr::arrange(out, GEOID)
    }
  }

  # return output
  return(out)

}

## Sub Function for Processing County-level Data
zi_process_county <- function(cb, state, county, year, zcta, method, style){

  # global variables
  GEOID = GEOID10 = GEOID20 = NULL

  # tigris call - TAG
  counties <- zi_get_tigris(.f = "counties", year = year, state = state, cb = cb)

  # if tigris call successful, wrangle
  if (!is.null(counties)){
    counties <- dplyr::select(counties, GEOID)
    counties <- dplyr::filter(counties, GEOID %in% county)

    # calculate centroids
    if (method == "centroid"){
      zcta <- sf::st_centroid(zcta)
    }

    # create simplified data
    if (style == "zcta5"){
      if (year < 2020){
        zcta <- dplyr::select(zcta, GEOID10)
      } else if (year >= 2020) {
        zcta <- dplyr::select(zcta, GEOID20)
      }
    }

    # geoprocess
    intersect <- suppressWarnings(sf::st_intersection(zcta, counties))

    # create output
    if (style == "zcta5"){
      if (year < 2020){
        out <- intersect$GEOID10
      } else if (year >= 2020) {
        out <- intersect$GEOID20
      }
    } else if (style == "zcta3"){
      out <- intersect$ZCTA3
    }
  } else {
    out <- NULL
  }

  # return output
  return(out)

}

## Sub Function for ZCTA3
zi_get_zcta3 <- function(year, state, county, territory, cb, starts_with,
                         includes, excludes, method){

  # global variables
  ZCTA3 = GEOID10 = GEOID20 = NULL

  # create value
  val <- paste0("zcta3_", year)

  # download geometry
  out <- tryCatch(
    sf::st_read(zcta3_url[[val]], quiet = TRUE),
    error = function(e) {
      cli::cli_inform(message = c(
        "x" = "Failed to download ZCTA3 geometry data. Returning {.code NULL} instead.",
        "i" = "Original error: {conditionMessage(e)}"
      ))
      NULL
    }
  )

  if (is.null(out)) return(NULL)

  # process geometry
  if (!is.null(state) & is.null(county)) {

    ## generate vector of requested state ZCTAs
    zcta_vec <- zi_list_zctas(year = year, state = c(state, territory), method = method)
    zcta_vec <- unique(substr(zcta_vec, 1, 3))

    ## add inclusions, remove exclusions
    zcta_vec <- unique(c(zcta_vec, includes))
    zcta_vec <- zcta_vec[!(zcta_vec %in% excludes)]

    ## subset based on year
    if (year < 2020){
      out <- dplyr::filter(out, ZCTA3 %in% zcta_vec)
    } else if (year >= 2020){
      out <- dplyr::filter(out, ZCTA3 %in% zcta_vec)
    }

  } else if (!is.null(state) & !is.null(county)){

    ## geoprocess based on county to produced vector of ZTAs
    zcta_vec <- zi_process_county(cb = cb, state = c(state, territory), county = county,
                                  year = year, zcta = out, method = method,
                                  style = "zcta3")

    if (!is.null(zcta_vec)){
      ## add inclusions, remove exclusions
      zcta_vec <- unique(c(zcta_vec, includes))
      zcta_vec <- zcta_vec[!(zcta_vec %in% excludes)]

      ## subset based on year
      out <- dplyr::filter(out, ZCTA3 %in% zcta_vec)
    } else {
      out <- NULL
    }

  } else if (is.null(state) & is.null(county)){

    ## manage territories
    if (is.null(territory)){

      ## all territories not including American Samoa
      out <- dplyr::filter(out, !(ZCTA3 %in% c("006", "007", "008", "009", "969")))

      ## American Samoa
      out <- sf::st_difference(out, samoa_bounding_box)

    } else if (!is.null(territory)){

      ## territory vector
      territory_vec <- c("AS", "GU", "MP", "PR", "VI")

      if (!all(territory == territory_vec)){

        ## construct vector
        territory_vec <- territory_vec[!(territory_vec %in% territory)]

        ## remove American Samoa from vector list
        if ("AS" %in% territory_vec){

          ## revise vector
          territory_vec <- territory_vec[!(territory_vec %in% c("AS"))]

          ## geoprocess
          out <- sf::st_difference(out, samoa_bounding_box)

        }

        ## create vector
        zcta_vec <- zi_list_zctas(year = year, state = territory_vec, method = "intersect")
        zcta_vec <- unique(substr(zcta_vec, 1, 3))

        ## append to excludes
        excludes <- unique(sort(c(excludes, zcta_vec)))

      }
    }

    ## remove exclusions
    if (!is.null(excludes)){
      out <- dplyr::filter(out, !(ZCTA3 %in% excludes))
    }

  }

  # subset based on starts with
  if (!is.null(out)){
    if (!is.null(starts_with)){
      out <- dplyr::filter(out, substr(ZCTA3, 1, 2) %in% starts_with)
    }

    # order output
    out <- dplyr::arrange(out, ZCTA3)
  }

  # return output
  return(out)

}

# validate starts with
zi_validate_starts <- function(x){

  # ensure character
  if (!is.character(x)){
    chr_out <- FALSE
  } else {
    chr_out <- TRUE
  }

  # ensure all values are numeric digits only
  if (any(!grepl("^[0-9]+$", x))){
    num_out <- FALSE
  } else {
    num_out <- TRUE
  }

  # ensure length and padding
  chr_len <- unique(nchar(x))
  chr_len <- chr_len[!is.na(chr_len)]

  # inputs are too long
  if (max(chr_len, na.rm = TRUE) > 2){
    len_out1 <- FALSE
  } else {
    len_out1 <- TRUE
  }

  # inputs are too short
  if (max(chr_len, na.rm = TRUE) < 2){
    len_out2 <- FALSE
  } else {
    len_out2 <- TRUE
  }

  # result
  out <- all(chr_out, num_out, len_out1, len_out2)

  # return result
  return(out)

}
