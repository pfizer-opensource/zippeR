
zipdat <- data.frame(zips = as.character(c(63130, 61270, 61081)))

zi_crosswalk(.data = zipdat, 
             input_zip =  zips, 
             dict = "UDS 2021", 
             dict_zip = "zip",
            dict_zcta = "zcta", 
            style = "zcta5", 
            zip_source = "UDS")



zi_load_crosswalk(zip_source = "UDS", 2021, qtr = NULL, target = NULL,
                              query = NULL, key = NULL)



zi_validate(zipdat$zips, verbose=T)
