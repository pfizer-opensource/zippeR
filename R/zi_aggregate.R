#' Aggregate ZCTAs to Three-digit ZCTAs
#'
#' @description This function takes input ZCTA data and aggregates it to three-digit
#'    areas, which are considerably larger. These regions are sometimes used in
#'    American health care contexts for publishing geographic identifiers.
#'
#' @usage zi_aggregate(.data, year, extensive = NULL, intensive = NULL,
#'     intensive_method = "mean", survey, output = "tidy", zcta = NULL,
#'     key = NULL)
#'
#' @param .data A tidy set of demographic data containing one or more variables
#'     that should be aggregated to three-digit ZCTAs. This data frame or tibble
#'     should contain all five-digit ZCTAs within the three digit ZCTAs that you
#'     plan to use for aggregating data. See Details below for formatting
#'     requirements.
#' @param year A four-digit numeric scalar for year. \code{zippeR} currently
#'     supports data for from 2010 to 2022. Different \code{survey} products
#'     are available for different years. See the \code{survey} parameter
#'     for more details.
#' @param extensive A character scalar or vector listing all extensive (i.e.
#'     count data) variables you wish to aggregate. These will be summed. For
#'     American Community Survey data, the margin of error will be calculated by
#'     taking the square root of the summed, squared margins of error for each
#'     five-digit ZCTA within a given three-digit ZCTA.
#' @param intensive A character scalar or vector listing all intensive (i.e.
#'     ratio, percent, or median data) variables you wish to aggregate. These
#'     will be combined using the approach listed for \code{intensive_method}.
#' @param intensive_method A character scalar; either \code{"mean"} (default)
#'     or \code{"median"}. In either case, a weighted approach is used where
#'     total population for each five-digit ZCTA is used to calculate individual
#'     ZCTAs' weights. For American Community Survey Data, this is applied to
#'     the margin of error as well.
#' @param survey A character scalar representing the Census product. It can
#'     be either a Decennial Census product (either \code{"sf1"} or \code{"sf3"})
#'     or an American Community Survey product (either \code{"acs1"},
#'     \code{"acs3"}, or \code{"acs5"}). For Decennial Census calls, only the 2010
#'     Census is available. In addition, if a variable cannot be found in \code{"sf1"},
#'     the function will look in \code{"sf3"}. Also note that \code{"acs3"} was
#'     discontinued after 2013.
#' @param output A character scalar; one of \code{"tidy"} (long output) or
#'     \code{"wide"} depending on the type of data format you want. If you are
#'     planning to join these data with geometric data, \code{"wide"} is the
#'     strongly encouraged format.
#' @param zcta An optional vector of ZCTAs that demographic data are requested
#'     for. If this is \code{NULL}, data will be returned for all ZCTAs. If a
#'     vector is supplied, only data for those requested ZCTAs will be returned.
#'     The vector can be created with \code{zi_get_geometry()}. If
#'     \code{style = "zcta5"}, this vector should be made up of five-digit
#'     \code{GEOID} values. If \code{style = "zcta3"}, this vector should be
#'     made up of three-digital \code{ZCTA3} values.
#' @param key A Census API key, which can be obtained at
#'     \url{https://api.census.gov/data/key_signup.html}. This can be omitted if
#'     \code{tidycensus::census_api_key()} has been used to write your key to
#'     your \code{.Renviron} file. You can check whether an API key has been
#'     written to \code{.Renviron} by using \code{Sys.getenv("CENSUS_API_KEY")}.
#'
#' @return A tibble containing all aggregated data requested in either
#'     \code{"tidy"} or \code{"wide"} format.
#'
#' @examples
#' # load sample demographic data
#' mo22_demos <- zi_mo_pop
#'
#'   # the above data can be replicated with the following code:
#'   # zi_get_demographics(year = 2022, variables = c("B01003_001", "B19013_001"),
#'   #   survey = "acs5")
#'
#' # load sample geometric data
#' mo22_zcta3 <- zi_mo_zcta3
#'
#'   # the above data can be replicated with the following code:
#'   # zi_get_geometry(year = 2022, style = "zcta3", state = "MO",
#'   #   method = "intersect")
#'
#' # aggregate a single variable
#' zi_aggregate(mo22_demos, year = 2020, extensive = "B01003_001", survey = "acs5",
#'   zcta = mo22_zcta3$ZCTA3)
#'
#' \donttest{
#' # aggregate multiple variables, outputting wide data
#' zi_aggregate(mo22_demos, year = 2020,
#'   extensive = "B01003_001", intensive = "B19013_001", survey = "acs5",
#'   zcta = mo22_zcta3$ZCTA3, output = "wide")
#' }
#'
#' @export
zi_aggregate <- function(.data, year, extensive = NULL, intensive = NULL,
                         intensive_method = "mean", survey,
                         output = "tidy", zcta = NULL, key = NULL){

  # evaluate inputs
  # if (is.null(.data)){
  #   stop("The '.data' object provided is NULL. Please provide a dataframe.")
  # }

  if (missing(year)){
    stop("The 'year' value is missing. Please provide a numeric value between 2010 and 2022.")
  }

  if (!is.numeric(year)){
    stop("The 'year' value provided is invalid. Please provide a numeric value between 2010 and 2022.")
  }

  if (length(survey) > 1){
    stop("One only 'survey' product may be requested at a time.")
  }

  if (!survey %in% c("sf1", "sf3", "acs1", "acs3", "acs5")){
    stop("The 'survey' requested is invalid. Please choose one of 'sf1', 'sf3', 'acs1', 'acs3', or 'acs5'.")
  }

  if (survey %in% c("sf1", "sf3") == TRUE & year != 2010){
    stop("The 'year' value provided is invalid for Decennial Census data. Only 2010 may be requested currently.")
  }

  if (survey %in% c("acs1", "acs5") == TRUE & year %in% c(2010:2022) == FALSE){
    stop("The 'year' value provided is invalid for 1- or 5-year American Community Survey data. Please provide a year between 2010 and 2022.")
  }

  if (survey == "acs3" & year %in% c(2010:2013) == FALSE){
    stop("The 'year' value provided is invalid for 3-year American Community Survey data. Please provide a year between 2010 and 2013.")
  }

  if (!output %in% c("tidy", "wide")){
    stop("The 'output' requested is invalid. Please choose one of 'tidy' or 'wide'.")
  }

  if (!inherits(.data, what = "data.frame")){
    stop("The '.data' object provided is not a dataframe or dataframe like object. Please provide a dataframe.")
  }

  if (survey %in% c("sf1", "sf3")){
    error <- "Input data appear to be malformed - there should be three columns for Decennial Census data: 'GEOID', 'variable', and 'value'. Note that zi_aggregate() only accepts 'tidy' data."

    if (length(names(.data)) != 3){
      stop(error)
    }

    if (all(names(.data) == c("GEOID", "variable", "value")) == FALSE){
      stop(error)
    }
  } else if (survey %in% c("acs1", "acs3", "acs5") == TRUE){
    error <- "Input data appear to be malformed - there should be four columns for ACS data: 'GEOID', 'variable', 'estimate', and 'moe'. Note that zi_aggregate() only accepts 'tidy' data."

    if (length(names(.data)) != 4){
      stop(error)
    }

    if (all(names(.data) == c("GEOID", "variable", "estimate", "moe")) == FALSE){
      stop(error)
    }
  }

  if (!is.null(zcta)){
    valid <- zi_validate(zcta, style = "zcta3")

    if (valid == FALSE){
      stop("ZCTA data passed to the 'zcta' argument are invalid. Please use 'zi_validate()' with the 'verbose = TRUE' option to investgiate further. The 'zi_repair()' function may be used to address issues.")
    }
  }

  if (is.null(extensive) & is.null(intensive)){
    stop("At least one of 'extensive' or 'intensive' must be provided.")
  }

  # set additional arguments
  ## call type
  if (!is.null(extensive)){
    extensive_id <- TRUE
  } else {
    extensive_id <- FALSE
  }

  if (!is.null(intensive)){
    intensive_id <- TRUE
  } else {
    intensive_id <- FALSE
  }

  # prep data
  .data <- dplyr::mutate(.data, ZCTA3 = substr(GEOID, 1, 3), .before = GEOID)
  .data <- dplyr::arrange(.data, ZCTA3)

  # call underlying tidycensus data
  if (survey %in% c("sf1", "sf3") == TRUE){

    ## summarize data
    if (extensive_id == TRUE & intensive_id == FALSE){

      ## aggregate
      out <- zi_census_extensive(.data)

    } else if (extensive_id == FALSE & intensive_id == TRUE){

      ## calculate weights
      weights <- zi_census_weights(year = year, key = key)

      ## aggregate
      if (!is.null(weights)){
        out <- zi_census_intensive(.data, weights = weights, method = intensive_method)
      } else {
        out <- NULL
      }

    } else if (extensive_id == TRUE & intensive_id == TRUE){

      ## subset data
      extensive_df <- dplyr::filter(.data, variable %in% extensive == TRUE)
      intensive_df <- dplyr::filter(.data, variable %in% intensive == TRUE)

      ## calculate weights
      weights <- zi_census_weights(year = year, key = key)

      if (!is.null(weights)){
        ## aggregate
        extensive_df <- zi_census_extensive(extensive_df)
        intensive_df <- zi_census_intensive(intensive_df, weights = weights, method = intensive_method)

        ## combine
        out <- dplyr::bind_rows(extensive_df, intensive_df)
        out <- dplyr::arrange(out, ZCTA3, variable)
      } else {
        out <- NULL
      }

    }

  } else if (survey %in% c("acs1", "acs3", "acs5") == TRUE){

    ## summarize data
    if (extensive_id == TRUE & intensive_id == FALSE){

      ## aggregate
      out <- zi_acs_extensive(.data)

    } else if (extensive_id == FALSE & intensive_id == TRUE){

      ## calculate weights
      weights <- zi_acs_weights(year = year, survey = survey, key = key)

      ## aggregate
      if (!is.null(weights)){
        out <- zi_acs_intensive(.data, weights = weights, method = intensive_method)
      } else {
        out <- NULL
      }

    } else if (extensive_id == TRUE & intensive_id == TRUE){

      ## subset data
      extensive_df <- dplyr::filter(.data, variable %in% extensive == TRUE)
      intensive_df <- dplyr::filter(.data, variable %in% intensive == TRUE)

      ## calculate weights
      weights <- zi_acs_weights(year = year, survey = survey, key = key)

      if (!is.null(weights)){
        ## aggregate
        extensive_df <- zi_acs_extensive(extensive_df)
        intensive_df <- zi_acs_intensive(intensive_df, weights = weights, method = intensive_method)

        ## combine
        out <- dplyr::bind_rows(extensive_df, intensive_df)
        out <- dplyr::arrange(out, ZCTA3, variable)
      } else {
        out <- NULL
      }
    }

  }

  if (!is.null(out)){
    # optionally subset
    if (is.null(zcta) == FALSE){
      out <- dplyr::filter(out, ZCTA3 %in% zcta == TRUE)
    }

    # optionally pivot
    if (output == "wide"){

      ## prep names
      out <- dplyr::rename(out, "E" = "estimate", "M" = "moe")

      ## pivot
      out <- tidyr::pivot_wider(out, id_cols = "ZCTA3", names_from = "variable",
                                names_glue = "{variable}{.value}",
                                values_from = c("E", "M"))

      ## re-order names alphabetically
      wide_names <- names(out)
      wide_names <- wide_names[wide_names != "ZCTA3"]
      wide_names <- c("ZCTA3", sort(wide_names))

      ## re-order columns alphabetically
      out <- dplyr::select(out, wide_names)

    }
  }

  # return output
  return(out)

}


## Extensive Decennial Census
zi_census_extensive <- function(.data){

  # global variables
  ZCTA3 = variable = value = NULL

  ## group by and sum
  .data <- dplyr::group_by(.data, ZCTA3, variable)
  .data <- dplyr::summarise(.data, value = sum(value, na.rm = TRUE))

  ## return output
  return(.data)

}

## Intensive Decennial Census
zi_census_intensive <- function(.data, weights, method){

  # global variables
  ZCTA3 = variable = value = weight = NULL

  ## join
  .data <- dplyr::left_join(.data, weights, by = "ZCTA3")

  ## group_by
  .data <- dplyr::group_by(.data, ZCTA3, variable)

  ## summarise (method dependent)
  if (method == "mean"){
    .data <- dplyr::summarise(.data, value = stats::weighted.mean(value, weight, na.rm = TRUE))
  } else if (method == "median"){
    .data <- dplyr::summarise(
      .data,
      value = spatstat.univar::weighted.median(value, weight)
    )
  }

  ## return output
  return(.data)

}

## Intensive Census Weights
zi_census_weights <- function(year, key){

  # global variables
  GEOID = NAME = ZCTA3 = total_pop = value = weight = NULL

  ## call get_decennial
  out <- zi_get_decennial(geography = "zcta", variables = "P001001",
                          table = NULL, year = year, output = "tidy",
                          survey = NULL, key = key)

  if (!is.null(out)){
    ## prep data
    out <- dplyr::mutate(out, ZCTA3 = substr(GEOID, 1, 3), .before = GEOID)
    out <- dplyr::select(out, -NAME)
    out <- dplyr::arrange(out, ZCTA3)

    ## group by and sum
    totals <- dplyr::group_by(out, ZCTA3)
    totals <- dplyr::summarise(totals, total_pop = sum(value, na.rm = TRUE))

    ## join
    out <- dplyr::left_join(out, totals, by = "ZCTA3")

    ## calculate proportions
    out <- dplyr::mutate(out, weight = value/total_pop)

    ## subset
    out <- dplyr::select(out, ZCTA3, weight)
  }

  ## return output
  return(out)

}

## Extensive ACS
zi_acs_extensive <- function(.data){

  # global variables
  ZCTA3 = variable = estimate = moe = NULL

  ## square MOEs
  .data <- dplyr::mutate(.data, moe = moe^2)

  ## group by and sum
  .data <- dplyr::group_by(.data, ZCTA3, variable)
  .data <- dplyr::summarise(.data,
                            estimate = sum(estimate, na.rm = TRUE),
                            moe = sum(moe, na.rm = TRUE))

  ## square root of MOE
  .data <- dplyr::mutate(.data, moe = sqrt(moe))

  ## return output
  return(.data)

}

## Intensive ACS
zi_acs_intensive <- function(.data, weights, method){

  # global variables
  ZCTA3 = variable = estimate = weight = moe = NULL

  ## join
  .data <- dplyr::left_join(.data, weights, by = "ZCTA3")

  ## group_by
  .data <- dplyr::group_by(.data, ZCTA3, variable)

  ## summarise (method dependent)
  if (method == "mean"){
    .data <- dplyr::summarise(.data,
                              estimate = stats::weighted.mean(estimate, weight, na.rm = TRUE),
                              moe = stats::weighted.mean(moe, weight, na.rm = TRUE))
  } else if (method == "median"){
    .data <- dplyr::summarise(
      .data,
      estimate = spatstat.univar::weighted.median(estimate, weight),
      moe = spatstat.univar::weighted.median(moe, weight)
    )
  }

  ## return output
  return(.data)

}

## Intensive ACS Weights
zi_acs_weights <- function(year, survey, key){

  # global variables
  GEOID = NAME = ZCTA3 = total_pop = estimate = weight = NULL

  ## call get_acs
  out <- zi_get_acs(geography = "zcta", variables = "B01003_001",
                    table = NULL, year = year, output = "tidy",
                    survey = survey, key = key)

  if (!is.null(out)){
    ## prep data
    out <- dplyr::mutate(out, GEOID = stringr::word(NAME, 2))
    out <- dplyr::mutate(out, ZCTA3 = substr(GEOID, 1, 3), .before = GEOID)
    out <- dplyr::select(out, -NAME)
    out <- dplyr::arrange(out, ZCTA3)

    ## group by and sum
    totals <- dplyr::group_by(out, ZCTA3)
    totals <- dplyr::summarise(totals, total_pop = sum(estimate, na.rm = TRUE))

    ## join
    out <- dplyr::left_join(out, totals, by = "ZCTA3")

    ## calculate proportions
    out <- dplyr::mutate(out, weight = estimate/total_pop)

    ## subset
    out <- dplyr::select(out, ZCTA3, weight)
  }

  ## return output
  return(out)

}

