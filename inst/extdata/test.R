
zi_load_hud <- function(year, qtr, target, queries){
  key <- Sys.getenv("hud_key")

  url <- "https://www.huduser.gov/hudapi/public/usps"

  # Loop over queries using map_dfr
  result <- purrr::map_dfr(queries, function(query) {
    
    if (year <= 2020 & query %in% c(state.abb, "VI", "PR", "ALL") == TRUE){
      stop("Queries with two letter state abbreviations or ALL are only available from the 1st quarter of 2021 onwards.")
    }

    if (target == "CBSADIV" & year <= 2016 | target == 'CBSADIV' & year == 2017 & qtr < 4){
      stop("CBSADIV data is available from the 4th quarter of 2017 onwards.")
    }

    if (target == "COUNTYSUB" & year < 2018 | target == 'COUNTYSUB' & year == 2018 & qtr < 2){
      stop("COUNTYSUB data is available from the 2nd quarter of 2018 onwards.")
    }

    if (query %in% c(state.abb, "VI", "PR", "ALL") == FALSE & is.numeric(query) == FALSE && nchar(as.character(query)) != 5){
      stop("The 'query' value provided is invalid. Please input a valid state abbreviation or zip code.")
    }

  url <- "https://www.huduser.gov/hudapi/public/usps"

  if (target == "TRACT"){
    request <- httr::GET(paste0(url, "?type=1&query=", query, "&year=", year, "&quarter=", qtr), httr::add_headers(Authorization = paste("Bearer", key, sep = " ")))
    content <- httr::content(request, "text")
    json <- jsonlite::fromJSON(content)
    list <- lapply(json,"[[",5)
    out <- as.data.frame(list)
    colnames(out) <- sub("data.", "", colnames(out))

  } else if (target == "COUNTY"){
    request <- httr::GET(paste0(url, "?type=2&query=", query, "&year=", year, "&quarter=", qtr), httr::add_headers(Authorization = paste("Bearer", key, sep = " ")))
    content <- httr::content(request, "text")
    json <- jsonlite::fromJSON(content)
    list <- lapply(json,"[[",5)
    out <- as.data.frame(list)
    colnames(out) <- sub("data.", "", colnames(out))

  } else if (target == "CBSA"){
    request <- httr::GET(paste0(url, "?type=3&query=", query, "&year=", year, "&quarter=", qtr), httr::add_headers(Authorization = paste("Bearer", key, sep = " ")))
    content <- httr::content(request, "text")
    json <- jsonlite::fromJSON(content)
    list <- lapply(json,"[[",5)
    out <- as.data.frame(list)
    colnames(out) <- sub("data.", "", colnames(out))

  } else if (target == "CBSADIV"){
    request <- httr::GET(paste0(url, "?type=4&query=", query, "&year=", year, "&quarter=", qtr), httr::add_headers(Authorization = paste("Bearer", key, sep = " ")))
    content <- httr::content(request, "text")
    json <- jsonlite::fromJSON(content)
    list <- lapply(json,"[[",5)
    out <- as.data.frame(list)
    colnames(out) <- sub("data.", "", colnames(out))

  } else if (target == "CD"){
    request <- httr::GET(paste0(url, "?type=5&query=", query, "&year=", year, "&quarter=", qtr), httr::add_headers(Authorization = paste("Bearer", key, sep = " ")))
    content <- httr::content(request, "text")
    json <- jsonlite::fromJSON(content)
    list <- lapply(json,"[[",5)
    out <- as.data.frame(list)
    colnames(out) <- sub("data.", "", colnames(out))

  } else if (target == "COUNTY_SUB"){
    request <- httr::GET(paste0(url, "?type=11&query=", query, "&year=", year, "&quarter=", qtr), httr::add_headers(Authorization = paste("Bearer", key, sep = " ")))
    content <- httr::content(request, "text")
    json <- jsonlite::fromJSON(content)
    list <- lapply(json,"[[",5)
    out <- as.data.frame(list)
    colnames(out) <- sub("data.", "", colnames(out))
  }

  return(out)
})
}

# example run
# year <- 2020
# qtr <- 1
# target <- "COUNTY"
# query <- c(61270, 63130)

# out <- zi_load_hud(year = year, qtr = qtr, target = target, queries = query)

