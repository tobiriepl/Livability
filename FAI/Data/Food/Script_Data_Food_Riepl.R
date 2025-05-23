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



#read raw data for food: only osm data

data_zbez = read_sf("Data/Census Districts/data_zbez.geojson")
vienna_bb = osmdata::opq(st_bbox(data_zbez))  # previously: opq("austria vienna")   


shops_osm = vienna_bb |>  
  osmdata::add_osm_feature(key="shop", value= c("bakery", "kiosk", "supermarket")) |>
  osmdata_sf()  

shops_points_osm = shops_osm$osm_points
shops_centroids_osm = st_centroid(shops_osm$osm_polygons) 
shops_osm_clean <- bind_rows(shops_points_osm, shops_centroids_osm)    |>
filter(!is.na(name))  |> 
  mutate(type = shop) |>
  select(name, type, geometry) 

View(shops_osm_clean)


amenity_osm = vienna_bb |>  
  osmdata::add_osm_feature(key="amenity", value= c("bar", "restaurant", "cafe")) |>
  osmdata_sf()  
amenity_points_osm = amenity_osm$osm_points
amenity_centroids_osm = st_centroid(amenity_osm$osm_polygons) 
amenity_osm_clean <- bind_rows(amenity_points_osm, amenity_centroids_osm)    |>
  filter(!is.na(name)) |>  
  mutate(type = amenity) |>
  select(name, type, geometry) 


# data preparation for visualization --------------------------------------

food_data = bind_rows(shops_osm_clean, amenity_osm_clean) |>
  filter(!is.na(type) & type %in% c("restaurant", "supermarket", "bar", "cafe", "bakery", "kiosk"))
  

View(food_data)

#final food data 
food_data_final <- st_join(food_data, data_zbez, join = st_within) |>   
  filter(!is.na(ZBEZ)) |>    # to cut providers not in vienna (they have no ZBEZ data)
  select(name, type, ZBEZ, geometry) |>
  group_by(ZBEZ) 

View(food_data_final)


# write to disk
output_path = here("Data/Food/food_points.geojson")

# delete old version
unlink(output_path)
write_sf(food_data_final, output_path)





