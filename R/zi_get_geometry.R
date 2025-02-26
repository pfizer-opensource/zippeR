#' Download and Optionally Geoprocess ZCTAs
#'
#' @description This function returns geometric data for ZIP Code Tabulation
#'     Areas (ZCTAs), which are rough approximations of many (but not all)
#'     USPS ZIP codes. Downloading and processing these data will be heavily
#'     affected by your internet connection, your choice for the \code{cb}
#'     argument, and the processing power of your computer (if you select
#'     specific counties).
#'
#' @usage zi_get_geometry (year, style = "zcta5", return = "id", class = "sf",
#'     state = NULL, county = NULL, territory = NULL, cb = FALSE,
#'     starts_with = NULL, includes = NULL, excludes = NULL, method,
#'     shift_geo = FALSE)
#'
#' @param year A four-digit numeric scalar for year. \code{zippeR} currently
#'     supports data between 2010 and 2023
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
#' @return A \code{sf} object with ZCTAs matching the parameters specified above:
#'     either a nationwide file, a specific state or states, or a specific
#'     county or counties.
#'
#' @examples
#' \donttest{
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
#' }
#'
#' @export
zi_get_geometry <- function(year, style = "zcta5", return = "id", class = "sf",
                            state = NULL, county = NULL, territory = NULL,
                            cb = FALSE, starts_with = NULL, includes = NULL,
                            excludes = NULL, method = NULL, shift_geo = FALSE){

  # check inputs
  if (is.numeric(year) == FALSE){
    stop("The 'year' value provided is invalid. Please provide a numeric value between years 2010 and 2023.")
  }

  if (year %in% c(2010:2023) == FALSE){
    stop("The 'year' value provided is invalid. Please provide a year between 2010 and 2023.")
  }

  if (style %in% c("zcta5", "zcta3") == FALSE){
    stop("The 'style' value provided is invalid. Please select either 'zcta5' or 'zcta3'.")
  }

  if (return %in% c("id", "full") == FALSE){
    stop("The 'return' value provided is invalid. Please select either 'id' or 'full'.")
  }

  if (style == "zcta3" & return == "full"){
    warning("The 'full' option for 'return' is not available for 'zcta3' data. Please use 'id' instead.")
  }

  if (style == "zcta3" & cb == TRUE){
    warning("The 'cb' argument does not apply to 'zcta3' data.")
  }

  if (is.logical(shift_geo) == FALSE){
    stop("The 'shift_geo' value provided is invalid. Please select either 'TRUE' or 'FALSE'.")
  }

  if (shift_geo == TRUE & is.null(state) == FALSE){
    stop("The 'shift_geo' functionality can only be used when you are returning data for all states.")
  }

  if (any(state %in% c("AS", "GU", "MP", "PR", "VI")) == TRUE){
    stop("Please specify territories using the 'territory' argument instead. Valid territories are: 'AS', 'GU', 'MP', 'PR', or 'VI' (or their equivalent FIPS codes).")
  }

  if (is.null(state) == FALSE){
    state <- unlist(sapply(state, validate_state, USE.NAMES=FALSE))
  }

  if (is.null(county) == FALSE & is.null(state) == TRUE){
    stop("Please provide at least one state abbreviation or FIPS code for the 'state' argument that corresponds to data passed to the 'county' argument.")
  }

  if (is.null(state) == FALSE & missing(method) == TRUE){
    stop("Please select a valid method for returning ZCTA values. Your choices are 'centroid' and 'intersect'. See documentation for details.")
    }

  if (!is.null(method)){
    if (method %in% c("centroid", "intersect") == FALSE){
      stop("The two valid methods for returning ZCTA values are 'centroid' and 'intersect'. See documentation for details.")
    }
  }

  ## validate counties
  if (is.null(territory) == FALSE & any(territory %in% c("AS", "GU", "MP", "PR", "VI")) == FALSE){
    stop("An abbreviation given for the 'territory' argument is invalid. Please use one or more of: 'AS', 'GU', 'MP', 'PR', or 'VI' (or their equivalent FIPS codes).")
    }

  if (is.null(starts_with) == FALSE){
    valid <- zi_validate_starts(starts_with)

    if (valid == FALSE){
      stop("ZCTA data passed to the 'starts_with' argument are invalid. Please use a character vector with only two-digit values.")
    }
  }

  if (is.null(includes) == FALSE){
    valid <- zi_validate(includes, style = style)

    if (valid == FALSE){
      stop("ZCTA data passed to the 'includes' argument are invalid. Please use 'zi_validate()' with the 'verbose = TRUE' option to investgiate further. The 'zi_repair()' function may be used to address issues.")
    }
  }

  if (is.null(excludes) == FALSE){
    valid <- zi_validate(excludes, style = style)

    if (valid == FALSE){
      stop("ZCTA data passed to the 'excludes' argument are invalid. Please use 'zi_validate()' with the 'verbose = TRUE' option to investgiate further. The 'zi_repair()' function may be used to address issues.")
    }
  }

  # check year
  if (year == 2011){
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
    if (class == "sf" & shift_geo == TRUE){

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
    if (is.null(state) == FALSE & is.null(county) == TRUE) {

      ## generate vector of requested state ZCTAs
      zcta_vec <- zi_list_zctas(year = year, state = c(state, territory), method = method)

      ## add inclusions, remove exclusions
      zcta_vec <- unique(c(zcta_vec, includes))
      zcta_vec <- zcta_vec[zcta_vec %in% excludes == FALSE]

      ## rename year
      if (year < 2020){
        out <- dplyr::rename(out, GEOID = GEOID10)
      } else if (year >= 2020){
        out <- dplyr::rename(out, GEOID = GEOID20)
      }

      ## subset
      out <- dplyr::filter(out, GEOID %in% zcta_vec == TRUE)

    } else if (is.null(state) == FALSE & is.null(county) == FALSE){

      ## geoprocess based on county to produced vector of ZCTAs
      zcta_vec <- zi_process_county(cb = cb, state = c(state, territory), county = county,
                                    year = year, zcta = out, method = method,
                                    style = "zcta5")

      if (!is.null(zcta_vec)){
        ## add inclusions, remove exclusions
        zcta_vec <- unique(c(zcta_vec, includes))
        zcta_vec <- zcta_vec[zcta_vec %in% excludes == FALSE]

        ## rename year
        if (year < 2020){
          out <- dplyr::rename(out, GEOID = GEOID10)
        } else if (year >= 2020){
          out <- dplyr::rename(out, GEOID = GEOID20)
        }

        ## subset
        out <- dplyr::filter(out, GEOID %in% zcta_vec == TRUE)
      } else {
        out <- NULL
      }

    } else if (is.null(state) == TRUE & is.null(county) == TRUE){

      ## rename year
      if (year < 2020){
        out <- dplyr::rename(out, GEOID = GEOID10)
      } else if (year >= 2020){
        out <- dplyr::rename(out, GEOID = GEOID20)
      }

      ## manage territories
      if (is.null(territory) == TRUE){

        ## all territories not including American Samoa
        out <- dplyr::filter(out, substr(GEOID, 1,3) %in% c("006", "007", "008", "009", "969") == FALSE)

        ## American Samoa
        out <- dplyr::filter(out, GEOID != "96799")

      } else if (is.null(territory) == FALSE){

        ## territory vector
        territory_vec <- c("AS", "GU", "MP", "PR", "VI")

        if (all(territory == territory_vec) == FALSE){

          ## construct list
          territory_vec <- territory_vec[territory_vec %in% territory == FALSE]

          ## create vector
          zcta_vec <- zi_list_zctas(year = year, state = territory_vec, method = "intersect")

          ## append to excludes
          excludes <- unique(sort(c(excludes, zcta_vec)))

        }
      }

      ## subset
      if (is.null(excludes) == FALSE){
        out <- dplyr::filter(out, GEOID %in% excludes == FALSE)
      }

    }

    # subset based on starts with
    if (!is.null(out)){
      if (is.null(starts_with) == FALSE){
        out <- dplyr::filter(out, substr(GEOID, 1, 2) %in% starts_with == TRUE)
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
  out <- sf::st_read(zcta3_url[[val]], quiet = TRUE)

  # process geometry
  if (is.null(state) == FALSE & is.null(county) == TRUE) {

    ## generate vector of requested state ZCTAs
    zcta_vec <- zi_list_zctas(year = year, state = c(state, territory), method = method)
    zcta_vec <- unique(substr(zcta_vec, 1, 3))

    ## add inclusions, remove exclusions
    zcta_vec <- unique(c(zcta_vec, includes))
    zcta_vec <- zcta_vec[zcta_vec %in% excludes == FALSE]

    ## subset based on year
    if (year < 2020){
      out <- dplyr::filter(out, ZCTA3 %in% zcta_vec == TRUE)
    } else if (year >= 2020){
      out <- dplyr::filter(out, ZCTA3 %in% zcta_vec == TRUE)
    }

  } else if (is.null(state) == FALSE & is.null(county) == FALSE){

    ## geoprocess based on county to produced vector of ZTAs
    zcta_vec <- zi_process_county(cb = cb, state = c(state, territory), county = county,
                                  year = year, zcta = out, method = method,
                                  style = "zcta3")

    if (!is.null(zcta_vec)){
      ## add inclusions, remove exclusions
      zcta_vec <- unique(c(zcta_vec, includes))
      zcta_vec <- zcta_vec[zcta_vec %in% excludes == FALSE]

      ## subset based on year
      out <- dplyr::filter(out, ZCTA3 %in% zcta_vec == TRUE)
    } else {
      out <- NULL
    }

  } else if (is.null(state) == TRUE & is.null(county) == TRUE){

    ## manage territories
    if (is.null(territory) == TRUE){

      ## all territories not including American Samoa
      out <- dplyr::filter(out, ZCTA3 %in% c("006", "007", "008", "009", "969") == FALSE)

      ## American Samoa
      out <- sf::st_difference(out, samoa_bounding_box)

    } else if (is.null(territory) == FALSE){

      ## territory vector
      territory_vec <- c("AS", "GU", "MP", "PR", "VI")

      if (all(territory == territory_vec) == FALSE){

        ## construct vector
        territory_vec <- territory_vec[territory_vec %in% territory == FALSE]

        ## remove American Samoa from vector list
        if ("AS" %in% territory_vec == TRUE){

          ## revise vector
          territory_vec <- territory_vec[territory_vec %in% c("AS") == FALSE]

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
    if (is.null(excludes) == FALSE){
      out <- dplyr::filter(out, ZCTA3 %in% excludes == FALSE)
    }

  }

  # subset based on starts with
  if (!is.null(out)){
    if (is.null(starts_with) == FALSE){
      out <- dplyr::filter(out, substr(ZCTA3, 1, 2) %in% starts_with == TRUE)
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
  if (is.character(x) == FALSE){
    chr_out <- FALSE
  } else {
    chr_out <- TRUE
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
  out <- all(chr_out, len_out1, len_out2)

  # return result
  return(out)

}

zi_get_tigris <- function(.f, year, state, cb){

  ## attempt to use tigris
  out <- try(
    suppressWarnings(
      do.call(what = eval(parse(text = paste0("tigris::", .f))), args = list(year = year, state = state, cb = cb))
    ),
    silent = TRUE
  )

  if (inherits(out, what = "try-error")){
    cli::cli_inform(message = c("x" = "Errors occurred while attempting to download data from the Census Bureau FTP Server. Returning {.code NULL} instead."))

    out <- NULL
  }

  return(out)

}

