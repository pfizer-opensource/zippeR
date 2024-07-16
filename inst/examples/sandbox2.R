devtools::load_all()
library(magrittr)

geo10 <- zi_get_geometry(year = 2010)

dec10 <- zi_get_demographics(year = 2010, variables = "P001001",
                             survey = "sf1")
dec10_zcta3 <- zi_aggregate(dec10, year = 2010, extensive = "P001001",
                            survey = "sf1")

dec10 <- zi_get_demographics(year = 2010, variables = c("P001001", "P013001"),
                             survey = "sf1")
dec10 <- zi_get_demographics(year = 2010, variables = c("P001001", "P013001"),
                             survey = "sf1", debug = "messages")
dec10 <- zi_get_demographics(year = 2010, variables = c("P001001", "P013001"),
                             survey = "sf1", debug = "call")
dec10_zcta3 <- zi_aggregate(dec10, year = 2010, extensive = "P001001", intensive = "P013001",
                            survey = "sf1")

geo10_3 <- zi_get_geometry(year = 2010, style = "zcta3", state = "MO", method = "centroid",
                           includes = c("516", "525"))

dec10_zcta3 <- zi_aggregate(dec10, year = 2010, extensive = "P001001", intensive = "P013001",
                            survey = "sf1", zcta = geo10_3$ZCTA3)

geo12 <- zi_get_geometry(year = 2012, state = "MO", method = "centroid", includes = c("51640", "52542", "52573", "52626"))
geo12_3 <- zi_get_geometry(year = 2012, style = "zcta3", state = "MO", method = "centroid",
                           includes = c("516", "525"))

dec12 <- zi_get_demographics(year = 2012, table = "B19083", survey = "acs5")
dec12 <- zi_get_demographics(year = 2012, table = "B19083", survey = "acs5", zcta = geo12$GEOID)
dec12 <- zi_get_demographics(year = 2012, table = "B19083", survey = "acs5", zcta = geo12$GEOID, debug = "messages")
dec12 <- zi_get_demographics(year = 2012, table = "B19083", survey = "acs5", zcta = geo12$GEOID, debug = "call")

zi_get_demographics(year = 2012, variables = "B01003_001", survey = "acs5") %>%
  zi_aggregate(year = 2012, extensive = "B01003_001", survey = "acs5", zcta = geo12_3$ZCTA3) -> dec12_zcta3

zi_get_demographics(year = 2020, variables = c("B01003_001", "B19083_001"), survey = "acs5") %>%
  zi_aggregate(year = 2020, extensive = "B01003_001", intensive = "B19083_001", survey = "acs5") -> dec20

zi_get_demographics(year = 2020, variables = c("B01003_001", "B19083_001"), survey = "acs5") %>%
  zi_aggregate(year = 2020, extensive = "B01003_001", intensive = "B19083_001", survey = "acs5",
               output = "wide") -> dec20_wide
