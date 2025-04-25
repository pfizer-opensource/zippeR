# zippeR 0.1.2

* Address issues with Census Bureau API being offline

# zippeR 0.1.1

* Updates to `README.md` and `NEWS.md` to reflect the fact that the package is now on CRAN
* Resolve R CMD check issue where examples in `zi_crosswalk` and `zi_load_crosswalk` fail on CI because a HUD key is not available to them
* Fail informatively if `tigris` is not working due to U.S. Census Bureau servers being unavailable. At this time TigerWeb is not used as a backup due to the limited availability of ZCTA data

# zippeR 0.1.0

* Initial CRAN submission version that contains functionality for working with UDS and HUD ZIP Code crosswalk files as well as Census Bureau ZCTA geometries and demographic data
