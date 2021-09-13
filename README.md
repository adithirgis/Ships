# Ships

## App deployed 

https://aruapps.shinyapps.io/ships/

## Data Description 

The data provided was in `.csv` format which was converted into `.RData` file for faster processing. The data was also cleaned using `janitor::clean_names()`. 

`LAT` - ship's latitude

`LON` - ship's longitude

`SPEED` - ship's speed in knots

`COURSE` - ship's course as angle

`HEADING` - ship's compass direction

`DESTINATION` - ship's destination (reported by the crew)

`FLAG` - ship's flag

`LENGTH` - ship's length in meters

`SHIPNAME` - ship's name

`SHIPTYPE` - ship's type

`SHIP_ID` - ship's unique identifier

`WIDTH` - ship's width in meters

`DWT` - ship's deadweight in tones

`DATETIME` - date and time of the observation

`PORT` - current port reported by the vessel

`Date` - date extracted from DATETIME

`Week_nb` - week number extracted from date

`Ship_type` - ship's type from SHIPTYPE

`Port` - current port assigned based on the ship's location

`Is_parked` - indicator whether the ship is moving or not 


## Session Information and packages used

```{r}
# For styling used 
styler::style_file()

# Session details
sessionInfo()

R version 4.0.5 (2021-03-31)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19042)

Matrix products: default

locale:
[1] LC_COLLATE=English_India.1252  LC_CTYPE=English_India.1252   
[3] LC_MONETARY=English_India.1252 LC_NUMERIC=C                  
[5] LC_TIME=English_India.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] forcats_0.5.1        stringr_1.4.0        dplyr_1.0.5          purrr_0.3.4         
 [5] readr_1.4.0          tidyr_1.1.3          tibble_3.1.0         ggplot2_3.3.3       
 [9] tidyverse_1.3.0      shiny.semantic_0.4.0 shiny_1.6.0         

loaded via a namespace (and not attached):
 [1] fs_1.5.0          lubridate_1.7.10  httr_1.4.2        rprojroot_2.0.2  
 [5] R.cache_0.15.0    tools_4.0.5       backports_1.2.1   bslib_0.2.4      
 [9] utf8_1.2.1        R6_2.5.0          DT_0.17           DBI_1.1.1        
[13] colorspace_2.0-0  withr_2.4.1       tidyselect_1.1.0  prettyunits_1.1.1
[17] processx_3.5.0    leaflet_2.0.4.1   curl_4.3          compiler_4.0.5   
[21] cli_2.4.0         rvest_1.0.0       xml2_1.3.2        desc_1.3.0       
[25] sass_0.3.1        scales_1.1.1      callr_3.6.0       digest_0.6.27    
[29] R.utils_2.10.1    pkgconfig_2.0.3   htmltools_0.5.1.1 styler_1.4.1     
[33] dbplyr_2.1.0      fastmap_1.1.0     htmlwidgets_1.5.3 rlang_0.4.10     
[37] readxl_1.3.1      rstudioapi_0.13   jquerylib_0.1.3   generics_0.1.0   
[41] jsonlite_1.7.2    crosstalk_1.1.1   R.oo_1.24.0       magrittr_2.0.1   
[45] Rcpp_1.0.6        munsell_0.5.0     fansi_0.4.2       lifecycle_1.0.0  
[49] R.methodsS3_1.8.1 stringi_1.5.3     snakecase_0.11.0  debugme_1.1.0    
[53] pkgbuild_1.2.0    grid_4.0.5        promises_1.2.0.1  crayon_1.4.1     
[57] haven_2.3.1       hms_1.0.0         ps_1.6.0          pillar_1.5.1     
[61] pkgload_1.2.1     reprex_1.0.0      XML_3.99-0.6      glue_1.4.2       
[65] remotes_2.3.0     modelr_0.1.8      vctrs_0.3.7       httpuv_1.5.5     
[69] testthat_3.0.2    cellranger_1.1.0  gtable_0.3.0      rematch2_2.1.2   
[73] assertthat_0.2.1  cachem_1.0.4      xfun_0.22         janitor_2.1.0    
[77] mime_0.10         xtable_1.8-4      broom_0.7.6       later_1.1.0.1    
[81] rsconnect_0.8.16  tinytex_0.31      ellipsis_0.3.1    here_1.0.1  


# Using law of haversines for calculating the shortest distance between two points in degrees and to convert it into distance in meters
  shortest_distance_measure <- function(start_lat, start_long, end_lat, end_long) {
    RadE <- 6378.137 # radius of earth in km
    diff_lat_rad <- end_lat * pi / 180 - start_lat * pi / 180 
    diff_lon_rad <- end_long * pi / 180 - start_long * pi / 180 
    a <- sin(diff_lat_rad / 2) * sin(diff_lat_rad / 2) +
      cos(start_lat * pi / 180) * cos(end_lat * pi / 180) *
        sin(diff_lon_rad / 2) * sin(diff_lon_rad / 2)
    cir <- 2 * atan2(sqrt(a), sqrt(1 - a))
    dist_m <- (RadE * cir) * 1000
  }
```
