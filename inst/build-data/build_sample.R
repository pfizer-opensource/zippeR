devtools::load_all()
library(dplyr)

# Create Sample Data ####
zi_mo_zcta3 <- zi_get_geometry(year = 2022, style = "zcta3", state = "MO",
                               method = "intersect")

zi_mo_pop <- zi_get_demographics(year = 2022, variables = c("B01003_001", "B19013_001"),
                                 survey = "acs5")

zi_mo_hud <- zi_load_crosswalk(zip_source = "HUD", year = 2023, qtr = 1,
                               target = "COUNTY", query = "MO")

# create variable with first three digits of GEOID
zi_mo_pop <- mutate(zi_mo_pop, zcta3 = substr(GEOID, 1, 3))
zi_mo_pop <- filter(zi_mo_pop, zcta3 %in% zi_mo_zcta3$ZCTA3)
zi_mo_pop <- select(zi_mo_pop, -zcta3)

save(zi_mo_zcta3, file = "data/zi_mo_zcta3.rda", version = 2, compress = "xz")
save(zi_mo_pop, file = "data/zi_mo_pop.rda", version = 2, compress = "xz")
save(zi_mo_hud, file = "data/zi_mo_hud.rda", version = 2, compress = "xz")

# Aggregate Data

zi_mo_pop_result <- zi_aggregate(zi_mo_pop, year = 2020,
                         extensive = "B01003_001",
                         intensive = "B19013_001",
                         survey = "acs5", zcta = zi_mo_zcta3$ZCTA3,
                         output = "wide")

save(zi_mo_pop_result, file = "inst/testdata/zi_mo_pop_result.rda", version = 2, compress = "xz")
