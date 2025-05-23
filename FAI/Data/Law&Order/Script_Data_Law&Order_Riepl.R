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



#read raw data for law and order: only official police data

police_official = read_sf("Data/Law&Order/POLIZEIOGD.json")
mapview(police_official)
police_official_clean = police_official |>
  select("name" = NAME) |>
  mutate(type = "police")




# official city data for pharmacy stores and doctors
data_zbez = read_sf("Data/Census Districts/data_zbez.geojson")


#osm data for dentists and opticians
vienna_bb = osmdata::opq(st_bbox(data_zbez))  # previously: opq("austria vienna")   

#dentist  # also public data available but from 2002
police_osm = vienna_bb |>  # no osm data on physiotherapists and psychoterapists
  osmdata::add_osm_feature(key="amenity", value= c("police")) |>    
  osmdata_sf()  

police_points_osm = police_osm$osm_points
police_centroids_osm = st_centroid(police_osm$osm_polygons) 
police_osm_clean <- bind_rows(police_points_osm, police_centroids_osm) |>   # 264 observations
  filter(!is.na(name)) |>  # no filtering of private vs public possible 
  mutate(type = "police") |>
  select(name, type, geometry) 

View(police_osm_clean)



# data preparation for visualization --------------------------------------

#healthcare points 
laworder_data_final <- st_join(police_official_clean, data_zbez, join = st_within) |>   
  filter(!is.na(ZBEZ)) |>    # to cut providers not in vienna (they have no ZBEZ data)
  select(name, type, ZBEZ, geometry) |>
  group_by(ZBEZ) 

View(laworder_data_final)
# write to disk
output_path = here("Data/Law&Order/laworder_points.geojson")

# delete old version
unlink(output_path)
write_sf(laworder_data_final, output_path)





