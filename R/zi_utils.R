# Internal left join helper (replaces dplyr::left_join())
# Wraps base merge() to reproduce dplyr::left_join()'s row-order and
# column-order contract: all rows of x are kept in their original order,
# and the output columns are x's original columns followed by any new
# (non-key) columns from y. merge() itself neither preserves x's row
# order nor keeps that column layout, so both are restored explicitly.
#
# `by` follows dplyr's convention: an unnamed character vector joins on
# identically-named columns in x and y; a named character vector (e.g.
# stats::setNames("y_col", "x_col")) joins x's named column against y's
# value column, keeping only x's column name in the output (mirroring
# dplyr::left_join(by = c("x_col" = "y_col"))).
#
# When x and y share a non-key column name, merge() (like dplyr::left_join())
# disambiguates both copies with `suffixes` (default ".x"/".y"); resolve_col()
# below maps each original column name to whichever name actually survived in
# merge()'s output (suffixed or not) so collisions are preserved rather than
# erroring out on a stale, pre-collision column name.
left_join_base <- function(x, y, by, suffixes = c(".x", ".y")) {

  idx_col <- ".zi_join_idx"
  x[[idx_col]] <- seq_len(nrow(x))

  if (is.null(names(by)) || all(names(by) == "")) {
    by_x <- by
    by_y <- by
  } else {
    by_x <- names(by)
    by_y <- unname(by)
  }

  out <- merge(x, y, by.x = by_x, by.y = by_y, all.x = TRUE, sort = FALSE,
               suffixes = suffixes)

  ## restore x's original row order
  out <- out[order(out[[idx_col]]), , drop = FALSE]
  out[[idx_col]] <- NULL

  ## restore column order: x's original columns, then new y columns,
  ## resolving each original name to its (possibly suffixed) name in `out`
  resolve_col <- function(orig, suffix) {
    if (orig %in% names(out)) return(orig)
    suffixed <- paste0(orig, suffix)
    if (suffixed %in% names(out)) return(suffixed)
    cli::cli_abort("Could not resolve output column for {.val {orig}} after joining.")
  }

  x_cols_orig <- setdiff(names(x), idx_col)
  y_cols_orig <- setdiff(names(y), by_y)

  x_cols <- vapply(x_cols_orig, resolve_col, character(1), suffix = suffixes[1])
  y_new_cols <- vapply(y_cols_orig, resolve_col, character(1), suffix = suffixes[2])

  out <- out[, c(x_cols, y_new_cols), drop = FALSE]

  rownames(out) <- NULL

  return(out)

}

# Internal group_by()+summarise() helper (replaces dplyr::group_by()+dplyr::summarise())
# Performs a split-apply-combine grouped aggregation in Base R, matching
# dplyr::group_by(group_vars)+dplyr::summarise()'s output contract: one row
# per unique combination of `group_vars` present in `.data`, sorted ascending
# by those columns (in the order given, C-locale/byte-order via
# order(method = "radix") - matching dplyr::group_by()'s locale-independent
# sort exactly, including NA sorting last), with the group-key columns first
# followed by whatever columns `summarise_fun` returns.
#
# `summarise_fun` is called once per group with that group's subset data.frame
# and must return a named list or single-row data.frame of the aggregated
# columns (e.g. function(d) list(value = sum(d$value, na.rm = TRUE))).
#
# Grouping is done by detecting run-boundaries in the sorted data (NA treated
# as equal to a preceding NA, mirroring dplyr::group_by()'s NA-as-a-real-level
# semantics), rather than by building string labels via factor()/interaction().
# The latter would silently collide a real NA with the literal string "NA"
# (both stringify to "NA" as an interaction label), merging otherwise-distinct
# groups - the run-boundary approach never stringifies keys, so this can't happen.
group_summarise_base <- function(.data, group_vars, summarise_fun) {

  if (nrow(.data) == 0) {
    ## preserve grouping-column types/names on empty input; the summarised
    ## column schema is unknown without at least one row, so probe it with a
    ## single all-NA row of .data's own column types, then discard that row
    probe <- .data[NA_integer_, , drop = FALSE]
    summarised_probe <- as.data.frame(summarise_fun(probe))
    out <- cbind(.data[group_vars], summarised_probe[0, , drop = FALSE])
    out <- out[0, , drop = FALSE]
    rownames(out) <- NULL
    return(out)
  }

  ## order ascending by group_vars using radix (C-locale, locale-independent)
  ## sort, matching dplyr::group_by()'s ordering contract exactly, including
  ## NA-last placement. is.nan(v) is included as a secondary sort key
  ## immediately after each column's value: order()'s radix method treats NA
  ## and NaN as tied (same relative position preserved, not sub-sorted by
  ## kind), so without this, interleaved NA/NaN values in the same column
  ## would not end up contiguous after sorting, breaking the run-boundary
  ## detection below (which does distinguish NaN from real NA).
  ord_keys <- list()
  for (col in group_vars) {
    v <- .data[[col]]
    ord_keys[[length(ord_keys) + 1]] <- v
    ord_keys[[length(ord_keys) + 1]] <- is.nan(v)
  }
  ord <- do.call(order, c(ord_keys, list(method = "radix")))
  .data <- .data[ord, , drop = FALSE]

  ## identify group boundaries by detecting where any group_var's value
  ## changes from the previous (now-sorted) row. NaN and NA are both
  ## "missing" under is.na(), but dplyr treats them as distinct group keys
  ## (numeric NaN != NA); is.nan() distinguishes them here so a NaN row is
  ## never merged into a real-NA group (or vice versa). is.nan() is safe to
  ## call on any vector type (returns all-FALSE for non-double types).
  n <- nrow(.data)
  changed <- rep(FALSE, n)
  for (col in group_vars) {
    v <- .data[[col]]
    both_missing_same_kind <- is.na(v[-1]) & is.na(v[-n]) & (is.nan(v[-1]) == is.nan(v[-n]))
    both_present_equal <- !is.na(v[-1]) & !is.na(v[-n]) & v[-1] == v[-n]
    same_as_prev <- c(FALSE, both_present_equal | both_missing_same_kind)
    changed <- changed | !same_as_prev
  }
  changed[1] <- TRUE
  group_id <- cumsum(changed)

  ## split into groups (already in ascending key order) and summarise each
  groups <- split(.data, group_id)
  summarised <- lapply(groups, summarise_fun)

  ## recover the group-key values (one row per group, same order as `groups`)
  group_key_rows <- do.call(rbind, lapply(groups, function(d) d[1, group_vars, drop = FALSE]))
  rownames(group_key_rows) <- NULL

  summarised_rows <- do.call(rbind, lapply(summarised, as.data.frame))
  rownames(summarised_rows) <- NULL

  out <- cbind(group_key_rows, summarised_rows)
  rownames(out) <- NULL

  out
}

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
