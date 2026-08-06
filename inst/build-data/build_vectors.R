# Set Style
style <- usethis::ui_yeah("Do you want to use the local ZCTA data?")

# Dependencies ####
## Packages
library(dplyr)
library(tigris)
library(sf)
library(purrr)

## Functions
if (style == FALSE){

  create_vector <- function(.data, state){

    ## subset and sort
    out <- filter(.data, STUSPS == state)
    out <- sort(out$GEOID)

    ## return output
    return(out)

  }

  create_year <- function(year, zcta, intersect_by, method = "intersects"){

    ## download data
    if (missing(zcta) == TRUE){
      zcta <- zctas(year = year, class = "sf")
    }

    ## ensure correct projection
    zcta <- st_transform(zcta, crs = 3857)

    ## subset
    if (year >= 2020){
      zcta <- select(zcta, GEOID = GEOID20)
    } else if (year >= 2010 & year < 2020){
      zcta <- select(zcta, GEOID = GEOID10)
    }

    ## fix topology issue
    if (year %in% c(2010,2011,2012,2013,2014)){
      zcta <- st_make_valid(zcta)
    }

    ## create vector for processing
    processing_vector <- sort(intersect_by$STUSPS)

    ## geoprocess
    if (method == "centroid"){
      zcta <- st_centroid(zcta)
    }

    intersect_df <- st_intersection(zcta, intersect_by)

    if (method == "intersects"){
      intersect_df <- mutate(intersect_df, area = as.numeric(st_area(geometry)))
      intersect_df <- filter(intersect_df, area > 0)
      intersect_df <- select(intersect_df, -area)
    }

    st_geometry(intersect_df) <- NULL

    ## create named list for output
    processing_vector %>%
      set_names() %>%
      map(~create_vector(intersect_df, state = .x)) -> out

    ## return output
    return(out)

  }

}

compare_years <- function(year1, year2, name){

  x <- year1[[name]]
  y <- year2[[name]]

  ## test 1
  len <- length(x) == length(y)

  ## test 2
  if (len == TRUE){
    match <- all(names(x) == names(y))
  } else {
    match <- NA
  }

  ## evaluate
  out <- data.frame(
    state = name,
    len = len,
    match = match
  )

  ## return output
  return(out)

}

pull_changes <- function(year2, compare){

  change <- filter(compare, len == FALSE) %>% pull(state)
  out <- year2[change]

  ## return output
  return(out)

}

update_names <- function(update, year){

  if (length(update) > 0){
    post <- substr(year, nchar(year)-1, nchar(year))
    names(update) <- paste0(names(update), post)
  }

  ## return output
  return(update)

}

id_changes <- function(year1, year2, name, year){

  ## process
  name$STUSPS %>%
    unlist() %>%
    map_df(~compare_years(year1, year2, name = .x)) %>%
    pull_changes(year2, compare = .) %>%
    update_names(update = ., year = year) -> out

  ## return output
  return(out)

}

find_element <- function(state, ref, year){

  x <- names(ref[grep(state, names(ref))])

  y <- substr(x, nchar(x)-1, nchar(x))
  z <- as.numeric(paste0("20", y))

  out <- tail(z[z <= year], n = 1)
  out <- paste0(state, substr(out, nchar(out)-1, nchar(out)))

  return(out)

}

# Download State Data ####
## get state geometry
states <- states(class = "sf")
states <- st_transform(states, crs = 3857)

## create simplified versions of state geometry
states_abbrev <- select(states, STUSPS)

states_lookup <- select(states, fips = GEOID, abb = STUSPS, name = NAME)
st_geometry(states_lookup) <- NULL

# Process ZCTA Data, Intersects ####
if (style == FALSE){

  ## years for which cached inst/data-raw/ vectors may already exist
  available_years <- c(2010, 2012:2024)

  ## fetch + process only years whose cached .rda files are missing;
  ## years with both intersect and centroid caches present are skipped
  ## entirely (no re-download, no re-processing), enabling partial
  ## rebuilds (e.g. adding only a new year) without re-running the full
  ## nationwide TIGER/Line download for every prior year.
  for (yr in sort(available_years, decreasing = TRUE)){

    intersect_path <- paste0("inst/data-raw/zcta", yr, ".rda")
    centroid_path <- paste0("inst/data-raw/zcta", yr, "_centroid.rda")

    if (file.exists(intersect_path) & file.exists(centroid_path)){
      message("Skipping ", yr, " - cached .rda files already exist.")
      next
    }

    message("Building ", yr, " - downloading and processing ZCTA data.")

    zctas_yr <- zctas(year = yr, class = "sf")

    if (!file.exists(intersect_path)){
      zcta_out <- create_year(year = yr, zcta = zctas_yr, intersect_by = states_abbrev)
      assign(paste0("zcta", yr), zcta_out)
      save(list = paste0("zcta", yr), file = intersect_path)
    }

    if (!file.exists(centroid_path)){
      zcta_out <- create_year(year = yr, zcta = zctas_yr, intersect_by = states_abbrev, method = "centroid")
      assign(paste0("zcta", yr), zcta_out)
      save(list = paste0("zcta", yr), file = centroid_path)
    }

  }

  rm(yr, intersect_path, centroid_path, available_years)
  if (exists("zctas_yr")) rm(zctas_yr)
  if (exists("zcta_out")) rm(zcta_out)

}

# Compare ZCTA Data, Intersects ####
## Load ZCTA Data ####
c(2010,2012:2024) %>%
  unlist() %>%
  map(~load(file = paste0("inst/data-raw/zcta", .x, ".rda"), envir = .GlobalEnv))

## Compare ZCTA Data ####
### run comparisons
changes2012 <- id_changes(year1 = zcta2010, year2 = zcta2012,
                          name = states_abbrev, year = 2012)

names(zcta2010) <- paste0(names(zcta2010), "10")

changes2013 <- id_changes(year1 = zcta2012, year2 = zcta2013,
                          name = states_abbrev, year = 2013)

changes2014 <- id_changes(year1 = zcta2013, year2 = zcta2014,
                          name = states_abbrev, year = 2014)

changes2015 <- id_changes(year1 = zcta2014, year2 = zcta2015,
                          name = states_abbrev, year = 2015)

changes2016 <- id_changes(year1 = zcta2015, year2 = zcta2016,
                          name = states_abbrev, year = 2016)

changes2017 <- id_changes(year1 = zcta2016, year2 = zcta2017,
                          name = states_abbrev, year = 2017)

changes2018 <- id_changes(year1 = zcta2017, year2 = zcta2018,
                          name = states_abbrev, year = 2018)

changes2019 <- id_changes(year1 = zcta2018, year2 = zcta2019,
                          name = states_abbrev, year = 2019)

changes2020 <- id_changes(year1 = zcta2019, year2 = zcta2020,
                          name = states_abbrev, year = 2020)

changes2021 <- id_changes(year1 = zcta2020, year2 = zcta2021,
                          name = states_abbrev, year = 2021)

changes2022 <- id_changes(year1 = zcta2021, year2 = zcta2022,
                          name = states_abbrev, year = 2022)

changes2023 <- id_changes(year1 = zcta2022, year2 = zcta2023,
                          name = states_abbrev, year = 2023)

changes2024 <- id_changes(year1 = zcta2023, year2 = zcta2024,
                          name = states_abbrev, year = 2024)


### combine comparisons
changes <- c(zcta2010, changes2012, changes2013, changes2014, changes2015,
             changes2016, changes2017, changes2018, changes2019, changes2020,
             changes2021, changes2022, changes2023, changes2024)
changes <- changes[order(names(changes))]

### clean-up
rm(zcta2010, zcta2012, zcta2013, zcta2014, zcta2015, zcta2016, zcta2017,
   zcta2018, zcta2019, zcta2020, zcta2021, zcta2022, zcta2023, zcta2024)
rm(changes2012, changes2013, changes2014, changes2015, changes2016,
   changes2017, changes2018, changes2019, changes2020, changes2021,
   changes2022, changes2023, changes2024)

# Create Reference Table Data, Intersects ####
## reference data
reference <- data.frame(
  state = sort(rep(states_abbrev$STUSPS, length(2010:2024))),
  year = rep(2010:2024, length(states_abbrev$STUSPS))
)

reference <- left_join(reference, states_lookup, by = c("state" = "abb")) %>%
  select(state, fips, year) %>%
  arrange(state, year)

## identify elements in changes
obj <- pmap(list(reference$state, reference$year), ~ find_element(state = ..1, ref = changes, year = ..2))
obj <- unlist(obj)

## combine with reference data
reference <- cbind(reference, obj)

## fix names
reference_intersects <- reference
changes_intersects <- changes

## clean-up
rm(obj, reference, changes)
rm(states)

# Compare ZCTA Data, Centroids ####
## Load ZCTA Data ####
c(2010,2012:2024) %>%
  unlist() %>%
  map(~load(file = paste0("inst/data-raw/zcta", .x, "_centroid.rda"), envir = .GlobalEnv))

## Compare ZCTA Data ####
### run comparisons
changes2012 <- id_changes(year1 = zcta2010, year2 = zcta2012,
                          name = states_abbrev, year = 2012)

names(zcta2010) <- paste0(names(zcta2010), "10")

changes2013 <- id_changes(year1 = zcta2012, year2 = zcta2013,
                          name = states_abbrev, year = 2013)

changes2014 <- id_changes(year1 = zcta2013, year2 = zcta2014,
                          name = states_abbrev, year = 2014)

changes2015 <- id_changes(year1 = zcta2014, year2 = zcta2015,
                          name = states_abbrev, year = 2015)

changes2016 <- id_changes(year1 = zcta2015, year2 = zcta2016,
                          name = states_abbrev, year = 2016)

changes2017 <- id_changes(year1 = zcta2016, year2 = zcta2017,
                          name = states_abbrev, year = 2017)

changes2018 <- id_changes(year1 = zcta2017, year2 = zcta2018,
                          name = states_abbrev, year = 2018)

changes2019 <- id_changes(year1 = zcta2018, year2 = zcta2019,
                          name = states_abbrev, year = 2019)

changes2020 <- id_changes(year1 = zcta2019, year2 = zcta2020,
                          name = states_abbrev, year = 2020)

changes2021 <- id_changes(year1 = zcta2020, year2 = zcta2021,
                          name = states_abbrev, year = 2021)

changes2022 <- id_changes(year1 = zcta2021, year2 = zcta2022,
                          name = states_abbrev, year = 2022)

changes2023 <- id_changes(year1 = zcta2022, year2 = zcta2023,
                          name = states_abbrev, year = 2023)

changes2024 <- id_changes(year1 = zcta2023, year2 = zcta2024,
                          name = states_abbrev, year = 2024)

### combine comparisons
changes <- c(zcta2010, changes2012, changes2013, changes2014, changes2015,
             changes2016, changes2017, changes2018, changes2019, changes2020,
             changes2021, changes2022, changes2023, changes2024)
changes <- c(changes, AS12 = "96799")
changes <- changes[order(names(changes))]

### clean-up
rm(zcta2010, zcta2012, zcta2013, zcta2014, zcta2015, zcta2016, zcta2017,
   zcta2018, zcta2019, zcta2020, zcta2021, zcta2022, zcta2023, zcta2024)
rm(changes2012, changes2013, changes2014, changes2015, changes2016,
   changes2017, changes2018, changes2019, changes2020, changes2021,
   changes2022, changes2023, changes2024)
rm(compare_years, id_changes, pull_changes, update_names)

# Create Reference Table Data, Centroids ####
## reference data
reference <- data.frame(
  state = sort(rep(states_abbrev$STUSPS, length(2010:2024))),
  year = rep(2010:2024, length(states_abbrev$STUSPS))
)

reference <- left_join(reference, states_lookup, by = c("state" = "abb")) %>%
  select(state, fips, year) %>%
  arrange(state, year)

states_lookup <- mutate(states_lookup,
                        abb = tolower(abb),
                        name = tolower(name))

## identify elements in changes
obj <- pmap(list(reference$state, reference$year), ~ find_element(state = ..1, ref = changes, year = ..2))
obj <- unlist(obj)

## combine with reference data
reference <- cbind(reference, obj)

## fix names
reference_centroids <- reference
changes_centroids <- changes

## clean-up
rm(obj, reference, changes)
rm(find_element)
rm(states_abbrev)
rm(style)

# ZCTA3 URLS ####

zcta3_url <- list(
  zcta3_2010 = "https://raw.githubusercontent.com/chris-prener/zcta3/main/data/zcta3_2010.geojson",
  zcta3_2012 = "https://raw.githubusercontent.com/chris-prener/zcta3/main/data/zcta3_2012.geojson",
  zcta3_2013 = "https://raw.githubusercontent.com/chris-prener/zcta3/main/data/zcta3_2013.geojson",
  zcta3_2014 = "https://raw.githubusercontent.com/chris-prener/zcta3/main/data/zcta3_2014.geojson",
  zcta3_2015 = "https://raw.githubusercontent.com/chris-prener/zcta3/main/data/zcta3_2015.geojson",
  zcta3_2016 = "https://raw.githubusercontent.com/chris-prener/zcta3/main/data/zcta3_2016.geojson",
  zcta3_2017 = "https://raw.githubusercontent.com/chris-prener/zcta3/main/data/zcta3_2017.geojson",
  zcta3_2018 = "https://raw.githubusercontent.com/chris-prener/zcta3/main/data/zcta3_2018.geojson",
  zcta3_2019 = "https://raw.githubusercontent.com/chris-prener/zcta3/main/data/zcta3_2019.geojson",
  zcta3_2020 = "https://raw.githubusercontent.com/chris-prener/zcta3/main/data/zcta3_2020.geojson",
  zcta3_2021 = "https://raw.githubusercontent.com/chris-prener/zcta3/main/data/zcta3_2021.geojson",
  zcta3_2022 = "https://raw.githubusercontent.com/chris-prener/zcta3/main/data/zcta3_2022.geojson",
  zcta3_2023 = "https://raw.githubusercontent.com/chris-prener/zcta3/main/data/zcta3_2023.geojson",
  zcta3_2024 = "https://raw.githubusercontent.com/chris-prener/zcta3/main/data/zcta3_2024.geojson"
)

# Create American Samoa Bounding Box ####
samoa_bounding_box <- zctas(cb = TRUE, year = 2019)
samoa_bounding_box <- filter(samoa_bounding_box, GEOID10 == "96799")
samoa_bounding_box <- st_as_sfc(st_bbox(samoa_bounding_box))

# Create Output ####
save(changes_intersects, changes_centroids, reference_intersects,
     reference_centroids, states_lookup, samoa_bounding_box, zcta3_url,
     file = "R/sysdata.rda", version = 2, compress = "xz")


