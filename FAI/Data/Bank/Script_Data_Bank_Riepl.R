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



#read raw data for bank: only osm data

data_zbez = read_sf("Data/Census Districts/data_zbez.geojson")
vienna_bb = osmdata::opq(st_bbox(data_zbez))  # previously: opq("austria vienna")   

bank1_osm = vienna_bb |>  
  osmdata::add_osm_feature(key="amenity", value= c( "atm", "bank")) |>
  osmdata_sf()  

bank1_points_osm = bank1_osm$osm_points
bank1_centroids_osm = st_centroid(bank1_osm$osm_polygons) 
bank1_osm_clean <- bind_rows(bank1_points_osm, bank1_centroids_osm)    |>
filter(!is.na(name))  |> 
filter(!is.na(amenity)) |>
  mutate(type = amenity) |>
  select(name, type, geometry) 


bank2_osm = vienna_bb |>  
  osmdata::add_osm_feature(key="amenity", value= c( "bureau_de_change")) |>
  osmdata_sf()  

bank2_points_osm = bank2_osm$osm_points
bank2_centroids_osm = st_centroid(bank2_osm$osm_polygons) 
bank2_osm_clean <- bind_rows(bank2_points_osm, bank2_centroids_osm)    |>
  filter(!is.na(name))  |> 
  filter(!is.na(amenity)) |>
  mutate(type = amenity,
         type = "exchange office") |>
  select(name, type, geometry) 




# data preparation for visualization --------------------------------------

bank_osm_clean_merged = bind_rows(bank1_osm_clean, bank2_osm_clean)

#final post data 
bank_data_final <- st_join(bank_osm_clean_merged, data_zbez, join = st_within) |>   
  filter(!is.na(ZBEZ)) |>    # to cut providers not in vienna (they have no ZBEZ data)
  select(name, type, ZBEZ, geometry) |>
  group_by(ZBEZ) 

View(bank_data_final)


# write to disk
output_path = here("Data/Bank/bank_points.geojson")

# delete old version
unlink(output_path)
write_sf(bank_data_final, output_path)





