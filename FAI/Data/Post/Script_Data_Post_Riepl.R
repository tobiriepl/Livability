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



#read raw data for postal services: only osm

# official city data for pharmacy stores and doctors
data_zbez = read_sf("Data/Census Districts/data_zbez.geojson")


#osm data for dentists and opticians
vienna_bb = osmdata::opq(st_bbox(data_zbez))  # previously: opq("austria vienna")   

#postal offices 
post_osm = vienna_bb |>  # no osm data on physiotherapists and psychoterapists
  osmdata::add_osm_feature(key="amenity", value= c("post_office")) |>    
  osmdata_sf()  

post_points_osm = post_osm$osm_points
post_centroids_osm = st_centroid(post_osm$osm_polygons) 
post_osm_clean <- bind_rows(post_points_osm, post_centroids_osm) |> 
  filter(!is.na(name)) |>  
  mutate(type = "post office") |>
  select(name, type, geometry) 

View(post_osm_clean)



# data preparation for visualization --------------------------------------

# final post data 
post_data_final <- st_join(post_osm_clean, data_zbez, join = st_within) |>   
  filter(!is.na(ZBEZ)) |>    # to cut providers not in vienna (they have no ZBEZ data)
  select(name, type, ZBEZ, geometry) |>
  group_by(ZBEZ) 


# write to disk
output_path = here("Data/Post/post_points.geojson")

# delete old version
unlink(output_path)
write_sf(post_data_final, output_path)





