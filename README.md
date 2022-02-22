This repository contains R programs for the cleaning, collation, and harmonization for the data from the Lake Sunapee Protective Association (LSPA)/Global Lakes Ecological Observation Network (GLEON) instrumented buoy. It is maintained by B. Steele (steeleb@caryinstitute.org). All data produced by these scripts are stored in the [Environmental Data Intitiative Data Portal](https://portal.edirepository.org/) as EDI packages edi.499 (underwater sensors) and edi.234 (weather data from the buoy).

Scripts apply [ODM2](http://vocabulary.odm2.org/) variable names and units when available. 



## background scripts

- *LSPA buoy DO Visual History.Rmd*: this script creates a document that depicts the history of DO measurements at the buoy as well as the manual measurements collated in the *buoy_manual_do_collation.R* script
- *Sunapee Buoy Data Logger DO Offset 2013-2015.Rmd*: this script describes and visualizes the handling of offsets applied ot the dissolved oxygen data that were applied at the Campbell datalogger.

## general function scripts

- *library_func_lists.R*: this script contains all packages, functions, and lists used in the cleaning and collation scripts

## data cleaning and collation scripts

- *buoy_manual_do_collation.R*: this script collates manual dissolved oxygen (DO) measurements from the LSPA's Longterm Monitoring Program database and from manual measurements taken at the buoy location.
- *Sunapee_buoy_YYYY.R*: these scripts are the cleaning, collation, and harmonization for each calendar year (YYYY) of buoy data.
    - note that 2021 has two files: one for version 1 of the buoy (through March), and one for version 2 of the buoy (June to end of year)
- *Sunapee_hobo_2015.R*: this script cleans and collates the hobo temperature and light pendants that were deployed in the summer of 2015.
- *Sunapee_hobodo_2018.R*: this script cleans and collates the dissolved oxygen data from the HOBO DO sensor deployed in the summer of 2018.
