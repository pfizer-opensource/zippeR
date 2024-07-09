# Define global variables for use in zippeR functions

## zi_aggregate
utils::globalVariables(c("GEOID", "ZCTA3", "key", "variable"))

## zi_crosswalk
utils::globalVariables(c("ZIP", "ZCTA"))

## zi_get_demographics
utils::globalVariables(c("GEOID10", "GEOID20", "NAME"))

## zi_list_zcta
utils::globalVariables("fips")

## zi_load_labels
utils::globalVariables(c("PO_NAME", "STATE", "ZIP_TYPE", "destination_area",
                         "destination_state", "scf_id", "scf_name", "scf_state"))

## zi_load_uds
utils::globalVariables(c("po_name", "zcta", "zip", "zip_type"))

## zi_prep_hud
utils::globalVariables(c("STATEFP", "STUSPS", "bus_ratio", "fips", "geoid",
                         "ratio", "res_ratio", "state", "state_fips",
                         "tot_ratio", "zip5"))
