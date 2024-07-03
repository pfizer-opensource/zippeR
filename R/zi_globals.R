# Define global variables for use in zippeR functions

## zi_load_uds
utils::globalVariables(c("po_name", "zcta", "zip", "zip_type"))

## zi_prep_hud
utils::globalVariables(c("STATEFP", "STUSPS", "bus_ratio", "fips", "geoid",
                         "ratio", "res_ratio", "state", "state_fips",
                         "tot_ratio", "zip5"))
