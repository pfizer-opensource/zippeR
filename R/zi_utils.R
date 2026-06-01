# Internal weighted median helper (replaces spatstat.univar::weighted.median)
# Computes the weighted median of x using weights w.
# NA values in x or w are silently dropped (consistent with na.rm = TRUE in
# the weighted.mean path used for method = "mean").
weighted_median <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  x <- x[ok]
  w <- w[ok]
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  cum_w <- cumsum(w) / sum(w)
  x[which(cum_w >= 0.5)[1]]
}

zi_get_tigris <- function(.f, year, state, cb){
  ## resolve function from tigris namespace
  tigris_fn <- tryCatch(
    getExportedValue("tigris", .f),
    error = function(e) {
      cli::cli_abort("Function {.fn tigris::{.f}} does not exist. Check the function name.")
    }
  )

  ## attempt to use tigris
  out <- tryCatch(
    suppressWarnings(
      do.call(what = tigris_fn, args = list(year = year, state = state, cb = cb))
    ),
    error = function(e) {
      cli::cli_inform(message = c(
        "x" = "Download from the Census Bureau FTP Server failed. Returning {.code NULL} instead.",
        "i" = "Original error: {conditionMessage(e)}"
      ))
      NULL
    }
  )

  return(out)

}

zi_get_decennial <- function(geography, variables, table, year, output, survey, key){

  ## attempt to use tidycensus
  out <- tryCatch(
    suppressWarnings(suppressMessages(
      tidycensus::get_decennial(geography = geography, variables = variables,
                                table = table, year = year, output = output,
                                sumfile = survey, key = key)
    )),
    error = function(e) {
      cli::cli_inform(message = c(
        "x" = "Download from the Census Bureau API failed. Returning {.code NULL} instead.",
        "i" = "Original error: {conditionMessage(e)}"
      ))
      NULL
    }
  )

  return(out)

}

zi_get_acs <- function(geography, variables, table, year, output, survey, key){

  ## attempt to use tidycensus
  out <- tryCatch(
    suppressWarnings(suppressMessages(
      tidycensus::get_acs(geography = geography, variables = variables,
                          table = table, year = year, output = output,
                          survey = survey, key = key)
    )),
    error = function(e) {
      cli::cli_inform(message = c(
        "x" = "Download from the Census Bureau API failed. Returning {.code NULL} instead.",
        "i" = "Original error: {conditionMessage(e)}"
      ))
      NULL
    }
  )

  return(out)

}

# these are all functions from the tigris package that are not exported
# https://github.com/walkerke/tigris/blob/master/R/utils.R
# used based on terms of the MIT License used by the package's author, Kyle Walker
# https://github.com/walkerke/tigris/blob/master/DESCRIPTION

# validate state
validate_state <- function(state, .msg=interactive()) {

  states_lookup <- states_lookup

  # global variables
  simpleCapSO = NULL

  # original tigris function
  if (is.null(state)) return(NULL)

  state <- tolower(trimws(state)) # forgive white space

  if (grepl("^[[:digit:]]+$", state)) { # we prbly have FIPS

    state <- sprintf("%02d", as.numeric(state)) # forgive 1-digit FIPS codes

    if (state %in% states_lookup$fips) {
      return(state)
    } else {
      # perhaps they passed in a county FIPS by accident so forgive that, too,
      # but warn the caller
      state_sub <- substr(state, 1, 2)
      if (state_sub %in% states_lookup$fips) {
        message(sprintf("Using first two digits of %s - '%s' (%s) - for FIPS code.",
                        state, state_sub,
                        states_lookup[states_lookup$fips == state_sub, "name"]),
                call.=FALSE)
        return(state_sub)
      } else {
        warning(sprintf("'%s' is not a valid FIPS code or state name/abbreviation", state), call.=FALSE)
        return(NULL)
      }
    }

  } else if (grepl("^[[:alpha:]]+", state)) { # we might have state abbrev or name

    if (nchar(state) == 2 & state %in% states_lookup$abb) { # yay, an abbrev!

      if (.msg)
        message(sprintf("Using FIPS code '%s' for state '%s'",
                        states_lookup[states_lookup$abb == state, "fips"],
                        toupper(state)))
      return(states_lookup[states_lookup$abb == state, "fips"])

    } else if (nchar(state) > 2 & state %in% states_lookup$name) { # yay, a name!

      if (.msg)
        message(sprintf("Using FIPS code '%s' for state '%s'",
                        states_lookup[states_lookup$name == state, "fips"],
                        simpleCapSO(state)))
      return(states_lookup[states_lookup$name == state, "fips"])

    } else {
      warning(sprintf("'%s' is not a valid FIPS code or state name/abbreviation", state), call.=FALSE)
      return(NULL)
    }

  } else {
    warning(sprintf("'%s' is not a valid FIPS code or state name/abbreviation", state), call.=FALSE)
    return(NULL)
  }

}

# Capitalization
simpleCapSO <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
