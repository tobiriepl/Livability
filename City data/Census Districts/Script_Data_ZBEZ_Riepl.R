## R Code: mapping the Foundational Infrastructure in Vienna

library(sf) 
library(osmdata)
library(tidyverse)
library(mapview) 
library(jsonlite)
library(readr)
library(here)
library(tidygeocoder)




# read raw data   --------------------------------------------------------------
census_districts_vienna_raw = read_sf("Data/Census Districts/ZAEHLBEZIRKOGD.json")  

census_districts_vienna_final <- census_districts_vienna_raw |>
  select(ZBEZ, BEZ, geometry)



View(census_districts_vienna_final)


# write to disk
output_path = here("Data/Census Districts/data_zbez.geojson")

# delete old version
unlink(output_path)
write_sf(census_districts_vienna_final, output_path)




# schindler höhe. geospatial point

# Your address
address <- "Wienerbergstraße 25, 1100 Wien"

# Geocode the address

schindler_höhe <- tidygeocoder::geocode(address = address, method = "osm") 


address


