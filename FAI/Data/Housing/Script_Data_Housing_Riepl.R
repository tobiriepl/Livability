## R Code: mapping the Foundational Infrastructure in Vienna

library(sf)    # spatial formation package 
library(osmdata)
library(tidyverse)
library(mapview) # interkative, schnelle karten
library(readxl)
library(here)
library(glue)
library(tidygeocoder)
library(tmaptools)



#read raw data for housing: no osm data, only official data from data.gv.at 

municipal_official = read_sf("Data/Housing/Gemeindebau/GEMBAUTENFLOGD.json")
municipal_official_clean = municipal_official |>
  select("name" = HOFNAME) |>
  mutate(type = "municipal",
         geometry = st_centroid(geometry))

housing_data = municipal_official_clean   # no osm data available for social housing

# housing_data_unique <- housing_data_merged[!duplicated(housing_data_merged$name), ]


# data preparation for visualization --------------------------------------

data_zbez = read_sf("Data/Census Districts/data_zbez.geojson")

#healthcare points 
housing_data_final <- st_join(housing_data, data_zbez, join = st_within) |>   
  filter(!is.na(ZBEZ)) |>    # to cut providers not in vienna (they have no ZBEZ data)   # quite unnecessary 
  select(name, type, ZBEZ, geometry) |>
  group_by(ZBEZ) 

View(housing_data_final)

# write to disk
output_path = here("Data/Housing/housing_points.geojson")

# delete old version
unlink(output_path)
write_sf(housing_data_final, output_path)





