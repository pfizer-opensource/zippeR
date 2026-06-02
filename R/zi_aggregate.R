#' Aggregate ZCTAs to Three-digit ZCTAs
#'
#' @description This function takes input ZCTA data and aggregates it to three-digit
#'    areas, which are considerably larger. These regions are sometimes used in
#'    American health care contexts for publishing geographic identifiers.
#'
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
#'     \code{"tidy"} or \code{"wide"} format, or \code{NULL} if population
#'     weight data cannot be downloaded from the Census Bureau API.
#'
#' @examplesIf interactive()
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
#' # aggregate multiple variables, outputting wide data
#' zi_aggregate(mo22_demos, year = 2020,
#'   extensive = "B01003_001", intensive = "B19013_001", survey = "acs5",
#'   zcta = mo22_zcta3$ZCTA3, output = "wide")
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
    cli::cli_abort("{.arg year} is required. Please provide a numeric value between {.val 2010} and {.val 2022}.")
  }

  if (!is.numeric(year)){
    cli::cli_abort(c(
      "{.arg year} must be numeric.",
      "i" = "You provided {.val {year}}."
    ))
  }

  if (length(survey) > 1){
    cli::cli_abort(c(
      "{.arg survey} must contain a single value.",
      "i" = "You provided {.val {survey}}."
    ))
  }

  if (!survey %in% c("sf1", "sf3", "acs1", "acs3", "acs5")){
    cli::cli_abort(c(
      "{.arg survey} must be one of {.val sf1}, {.val sf3}, {.val acs1}, {.val acs3}, or {.val acs5}.",
      "i" = "You provided {.val {survey}}."
    ))
  }

  if (survey %in% c("sf1", "sf3") & year != 2010){
    cli::cli_abort(c(
      "{.arg year} must be {.val 2010} for Decennial Census data.",
      "i" = "You requested {.val {survey}} for {.val {year}}."
    ))
  }

  if (survey %in% c("acs1", "acs5") & !(year %in% c(2010:2022))){
    cli::cli_abort(c(
      "{.arg year} must be between {.val 2010} and {.val 2022} for {.arg survey} values {.val acs1} and {.val acs5}.",
      "i" = "You requested {.val {survey}} for {.val {year}}."
    ))
  }

  if (survey == "acs3" & !(year %in% c(2010:2013))){
    cli::cli_abort(c(
      "{.arg year} must be between {.val 2010} and {.val 2013} when {.arg survey} is {.val acs3}.",
      "i" = "You provided {.val {year}}."
    ))
  }

  if (!output %in% c("tidy", "wide")){
    cli::cli_abort(c(
      "{.arg output} must be {.val tidy} or {.val wide}.",
      "i" = "You provided {.val {output}}."
    ))
  }

  if (!intensive_method %in% c("mean", "median")){
    cli::cli_abort(c(
      "{.arg intensive_method} must be {.val mean} or {.val median}.",
      "i" = "You provided {.val {intensive_method}}."
    ))
  }

  if (!inherits(.data, what = "data.frame")){
    cli::cli_abort("{.arg .data} must be a data frame or data frame-like object.")
  }

  if (survey %in% c("sf1", "sf3")){
    error <- "Input data appear to be malformed - there should be three columns for Decennial Census data: 'GEOID', 'variable', and 'value'. Note that zi_aggregate() only accepts 'tidy' data."

    if (length(names(.data)) != 3){
      cli::cli_abort(error)
    }

    if (!all(names(.data) == c("GEOID", "variable", "value"))){
      cli::cli_abort(error)
    }
  } else if (survey %in% c("acs1", "acs3", "acs5")){
    error <- "Input data appear to be malformed - there should be four columns for ACS data: 'GEOID', 'variable', 'estimate', and 'moe'. Note that zi_aggregate() only accepts 'tidy' data."

    if (length(names(.data)) != 4){
      cli::cli_abort(error)
    }

    if (!all(names(.data) == c("GEOID", "variable", "estimate", "moe"))){
      cli::cli_abort(error)
    }
  }

  if (!is.null(zcta)){
    valid <- zi_validate(zcta, style = "zcta3")

    if (!valid){
      cli::cli_abort(c(
        "{.arg zcta} contains invalid ZCTA values.",
        "i" = "Use {.fn zi_validate} with {.code verbose = TRUE} to investigate further."
      ))
    }
  }

  if (is.null(extensive) & is.null(intensive)){
    cli::cli_abort("At least one of {.arg extensive} or {.arg intensive} must be provided.")
  }

  # verify requested variables exist in the data
  data_vars <- unique(.data$variable)
  if (!is.null(extensive)){
    missing_ext <- setdiff(extensive, data_vars)
    if (length(missing_ext) > 0){
      cli::cli_abort(c(
        "{.arg extensive} contains variable names not found in {.arg .data}: {.val {missing_ext}}.",
        "i" = "Available variables: {.val {data_vars}}."
      ))
    }
  }
  if (!is.null(intensive)){
    missing_int <- setdiff(intensive, data_vars)
    if (length(missing_int) > 0){
      cli::cli_abort(c(
        "{.arg intensive} contains variable names not found in {.arg .data}: {.val {missing_int}}.",
        "i" = "Available variables: {.val {data_vars}}."
      ))
    }
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
  if (survey %in% c("sf1", "sf3")){

    ## summarize data
    if (extensive_id & !intensive_id){

      ## aggregate
      out <- zi_census_extensive(.data)

    } else if (!extensive_id & intensive_id){

      ## calculate weights
      weights <- zi_census_weights(year = year, key = key)

      ## aggregate
      if (!is.null(weights)){
        out <- zi_census_intensive(.data, weights = weights, method = intensive_method)
      } else {
        out <- NULL
      }

    } else if (extensive_id & intensive_id){

      ## subset data
      extensive_df <- dplyr::filter(.data, variable %in% extensive)
      intensive_df <- dplyr::filter(.data, variable %in% intensive)

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

  } else if (survey %in% c("acs1", "acs3", "acs5")){

    ## summarize data
    if (extensive_id & !intensive_id){

      ## aggregate
      out <- zi_acs_extensive(.data)

    } else if (!extensive_id & intensive_id){

      ## calculate weights
      weights <- zi_acs_weights(year = year, survey = survey, key = key)

      ## aggregate
      if (!is.null(weights)){
        out <- zi_acs_intensive(.data, weights = weights, method = intensive_method)
      } else {
        out <- NULL
      }

    } else if (extensive_id & intensive_id){

      ## subset data
      extensive_df <- dplyr::filter(.data, variable %in% extensive)
      intensive_df <- dplyr::filter(.data, variable %in% intensive)

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
    if (!is.null(zcta)){
      out <- dplyr::filter(out, ZCTA3 %in% zcta)
    }

    # optionally pivot
    if (output == "wide"){

      if (survey %in% c("sf1", "sf3")){
        ## pivot decennial (single value column)
        out <- stats::reshape(as.data.frame(out), idvar = "ZCTA3", timevar = "variable",
                              direction = "wide", v.names = "value")
        names(out) <- sub("^value\\.", "", names(out))
        rownames(out) <- NULL
        out <- tibble::as_tibble(out)
      } else {
        ## prep names
        out <- dplyr::rename(out, "E" = "estimate", "M" = "moe")

        ## pivot ACS (estimate + moe columns)
        out <- stats::reshape(as.data.frame(out), idvar = "ZCTA3", timevar = "variable",
                              direction = "wide", v.names = c("E", "M"))
        nms <- names(out)
        nms <- sub("^E\\.(.+)$", "\\1E", nms)
        nms <- sub("^M\\.(.+)$", "\\1M", nms)
        names(out) <- nms
        rownames(out) <- NULL
        out <- tibble::as_tibble(out)
      }

      ## re-order names alphabetically
      wide_names <- names(out)
      wide_names <- wide_names[wide_names != "ZCTA3"]
      wide_names <- c("ZCTA3", sort(wide_names))

      ## re-order columns alphabetically
      out <- dplyr::select(out, dplyr::all_of(wide_names))

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
  ZCTA3 = GEOID = variable = value = weight = NULL

  ## join
  .data <- dplyr::left_join(.data, weights, by = c("ZCTA3", "GEOID"))

  ## group_by
  .data <- dplyr::group_by(.data, ZCTA3, variable)

  ## summarise (method dependent)
  if (method == "mean"){
    .data <- dplyr::summarise(.data, value = stats::weighted.mean(value, weight, na.rm = TRUE))
  } else if (method == "median"){
    .data <- dplyr::summarise(
      .data,
      value = weighted_median(value, weight)
    )
  } else {
    cli::cli_abort(c(
      "{.arg intensive_method} must be {.val mean} or {.val median}.",
      "i" = "You provided {.val {method}}."
    ))
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
    out <- dplyr::mutate(out, weight = value / total_pop)

    ## subset
    out <- dplyr::select(out, ZCTA3, GEOID, weight)
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
  ZCTA3 = GEOID = variable = estimate = weight = moe = NULL

  ## join
  .data <- dplyr::left_join(.data, weights, by = c("ZCTA3", "GEOID"))

  ## group_by
  .data <- dplyr::group_by(.data, ZCTA3, variable)

  ## summarise (method dependent)
  if (method == "mean"){
    .data <- dplyr::summarise(.data,
                              estimate = stats::weighted.mean(estimate, weight, na.rm = TRUE),
                              moe = stats::weighted.mean(moe, weight, na.rm = TRUE))
  } else if (method == "median"){
    .data <- dplyr::summarise(.data,
                              estimate = weighted_median(estimate, weight),
                              moe = weighted_median(moe, weight)
    )
  } else {
    cli::cli_abort(c(
      "{.arg intensive_method} must be {.val mean} or {.val median}.",
      "i" = "You provided {.val {method}}."
    ))
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
    out <- dplyr::mutate(out, GEOID = sub("^\\S+ ", "", NAME))
    out <- dplyr::mutate(out, ZCTA3 = substr(GEOID, 1, 3), .before = GEOID)
    out <- dplyr::select(out, -NAME)
    out <- dplyr::arrange(out, ZCTA3)

    ## group by and sum
    totals <- dplyr::group_by(out, ZCTA3)
    totals <- dplyr::summarise(totals, total_pop = sum(estimate, na.rm = TRUE))

    ## join
    out <- dplyr::left_join(out, totals, by = "ZCTA3")

    ## calculate proportions
    out <- dplyr::mutate(out, weight = estimate / total_pop)

    ## subset
    out <- dplyr::select(out, ZCTA3, GEOID, weight)
  }

  ## return output
  return(out)

}
