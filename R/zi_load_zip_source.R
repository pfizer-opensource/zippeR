# Load HUD and UDS Crosswalk Files
#
# @description These two helper functions load either the UDS crosswalk files
# or the HUD API crosswalk data based on the zip_source specified with
# zi_load_crosswalk.
#
# @param year A four-digit numeric scalar for year. \code{zippeR} currently
#     supports data for from 2010 to 2020.
#
# @param qtr Numbers 1-4 to select a quarter.
#
# @param target A string to select the crosswalk type. Options are 'TRACT',
# 'COUNTY', 'CBSA', 'CBSADIV', 'CD", and 'COUNTYSUB'.
#
# @param query This can be a five-digit numeric for zip code, a two-
# letter capitalized string state abbreviation, or 'ALL'.
#
# @return A tibble containing either the UDS Mapper crosswalk file for a given
# year the HUD API data for a particular year, quarter, target, and location.
#

zi_load_uds <- function(year) {
    # Read and bind all CSV files into a single dataframe
    out <- readr::read_csv(paste0("https://raw.githubusercontent.com/chris-prener/uds-mapper/main/data/uds_crosswalk_", year, ".csv"))

    out$zip <- stringr::str_pad(out$zip, width = 5, side = "left", pad = "0")
    out$zcta <- stringr::str_pad(out$zcta, width = 5, side = "left", pad = "0")

    # Remove military zip if 'zip_type' column exists
    if ("zip_type" %in% names(out)) {
        out <- dplyr::filter(out, grepl("^M", zip_type) == FALSE)
    }

    # Convert 'po_name' to title case if it exists
    if ("po_name" %in% names(out)) {
        out <- dplyr::mutate(out, po_name = stringr::str_to_title(po_name))
    }

    # Remove N/A zcta if 'zcta' column exists
    if ("zcta" %in% names(out)) {
        out <- dplyr::filter(out, zcta %in% c("N/A", NA) == FALSE)
    }

    ## remove non-ZCTA geometries
    out <- dplyr::filter(out, !zcta %in% "No ZCTA")

    # re-order output
    out <- dplyr::arrange(out, zip)

    # convert to tibble
    out <- tibble::as_tibble(out)

    # check validation
    valid_zip <- zi_validate(out$zip)

    if (valid_zip == FALSE) {
         warning("The 'zip' column failed initial validation. Inspect it closely and address issues found with 'zi_validate()' before using.")
    }

    valid_zcta <- zi_validate(out$zcta)

    if (valid_zcta == FALSE) {
         warning("The 'ZCTA' column failed initial validation. Inspect it closely and address issues found with 'zi_validate()' before using.")
    }

  names(out) <- toupper(names(out))
    # return output
    return(out)
}

zi_load_hud <- function(year, qtr, target, queries, key = NULL){

  if (is.null(key) == TRUE){
    key <- Sys.getenv("hud_key")
  }

  if (key == ""){
    stop("Please provide a valid HUD API key.")
  }

  url <- "https://www.huduser.gov/hudapi/public/usps"

  # Loop over queries using map_dfr
  result <- purrr::map_dfr(queries, function(query) {

    if (year <= 2020 & query %in% c(datasets::state.abb, "VI", "PR", "ALL") == TRUE){
      stop("Queries with two letter state abbreviations or ALL are only available from the 1st quarter of 2021 onwards.")
    }

    if (target == "CBSADIV" & year <= 2016 | target == 'CBSADIV' & year == 2017 & qtr < 4){
      stop("CBSADIV data is available from the 4th quarter of 2017 onwards.")
    }

    if (target == "COUNTYSUB" & year < 2018 | target == 'COUNTYSUB' & year == 2018 & qtr < 2){
      stop("COUNTYSUB data is available from the 2nd quarter of 2018 onwards.")
    }

    if (query %in% c(datasets::state.abb, "VI", "PR", "ALL") == FALSE & is.numeric(query) == FALSE && nchar(as.character(query)) != 5){
      stop("The 'query' value provided is invalid. Please input a valid state abbreviation or zip code.")
    }

  url <- "https://www.huduser.gov/hudapi/public/usps"

  # create request
  if (target == "TRACT"){
    type <- "?type=1&query="
  } else if (target == "COUNTY"){
    type <- "?type=2&query="
  } else if (target == "CBSA"){
    type <- "?type=3&query="
  } else if (target == "CBSADIV"){
    type <- "?type=4&query="
  } else if (target == "CD"){
    type <- "?type=5&query="
  } else if (target == "COUNTY_SUB"){
    type <- "?type=11&query="
  }

  # get data and format
  request <- httr::GET(paste0(url, type, query, "&year=", year, "&quarter=", qtr), httr::add_headers(Authorization = paste("Bearer", key, sep = " ")))
  content <- httr::content(request, "text", encoding = "UTF-8")
  json <- jsonlite::fromJSON(content)
  list <- lapply(json,"[[",5)

  # create output
  out <- as.data.frame(list)
  colnames(out) <- sub("data.", "", colnames(out))
  names(out) <- toupper(names(out))
  out <- dplyr::as_tibble(out)

  #return output
  return(out)

  })

}
