devtools::load_all()

## currently returns territories
sample <- zi_get_geometry(year = 2020, state = "MO", method = "centroid")

x <- filter(sample, substr(GEOID20, 1, 2) %in% c("02") == TRUE)

sample2 <- zi_get_geometry(year = 2020, state = "MO", county = "29510")


vt <-tidycensus:: get_acs(geography = "county",
                          variables = c(medincome = "B19013_001"),
                          state = "VT",
                          year = 2020) %>%
  dplyr:: select(-NAME) %>%
  zi_aggregate(year= 2020, survey = "acs5", .data = vt )
