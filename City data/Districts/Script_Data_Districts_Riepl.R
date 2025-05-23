## R Code: mapping the Foundational Infrastructure in Vienna

library(sf) 
library(osmdata)
library(tidyverse)
library(mapview) 
library(jsonlite)
library(readr)
library(here)



# read raw data   --------------------------------------------------------------
districts_vienna_raw = read_sf("Data/Districts/BEZIRKSGRENZEOGD.json")  

districts_vienna_clean <- districts_vienna_raw |>
  select(name = "NAMEK", BEZ, DISTRICT_CODE, geometry)

View(districts_vienna_clean)


# write to disk
output_path = here("Data/Districts/data_bez.geojson")

# delete old version
unlink(output_path)
write_sf(districts_vienna_clean, output_path)








