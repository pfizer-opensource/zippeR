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

  state <- tolower(stringr::str_trim(state)) # forgive white space

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

# HUD API key

hud_key <- function(hud_key, overwrite = FALSE, install = FALSE){

  if (install) {
    home <- Sys.getenv("HOME")
    renv <- file.path(home, ".Renviron")
    if(file.exists(renv)){
      # Backup original .Renviron before doing anything else here.
      file.copy(renv, file.path(home, ".Renviron_backup"))
    }
    if(!file.exists(renv)){
      file.create(renv)
    }
    else{
      if(isTRUE(overwrite)){
        message("Your original .Renviron will be backed up and stored in your R HOME directory if needed.")
        oldenv=utils::read.table(renv, stringsAsFactors = FALSE)
        newenv <- oldenv[-grep("hud_key", oldenv),]
        utils::write.table(newenv, renv, quote = FALSE, sep = "\n",
                    col.names = FALSE, row.names = FALSE)
      }
      else{
        tv <- readLines(renv)
        if(any(grepl("hud_key",tv))){
          stop("A HUD_KEY already exists. You can overwrite it with the argument overwrite=TRUE", call.=FALSE)
        }
      }
    }

    keyconcat <- paste0("hud_key='", hud_key, "'")
    # Append API key to .Renviron file
    write(keyconcat, renv, sep = "\n", append = TRUE)
    message('Your HUD API key has been stored in your .Renviron and can be accessed by Sys.getenv("hud_key"). \nTo use now, restart R or run `readRenviron("~/.Renviron")`')
    return(hud_key)
  } else {
    message("To install your HUD API key for use in future sessions, run this function with `install = TRUE`.")
    Sys.setenv(hud_key = hud_key)
  }

}

