## R Code: mapping the Foundational Infrastructure in Vienna

library(sf) # mittlerweile standard paket für vektor geodaten in R    # spatial formation package 
library(osmdata)
library(tidyverse)
library(mapview) # interkative, schnelle karten
library(ggthemes) # pretty ggplots

library(here)
library(glue)
# library(rajudas) 
library(jsonlite)
library(hereR)
library(readr)

library(stringi)

#read raw data
data_zbez = read_sf("Data/Census Districts/ZAEHLBEZIRKOGDPolygon.shp")   #  census district data from city of Vienna

recycling_center_official = read_sf("Data/Utilities/MISTPLATZOGD.json")
recycling_center_official_clean = recycling_center_official |>
  mutate(type = "recycling center") |>
  select(name = "ADRESSE", type, geometry)
  
prob_mat_official = read_sf("Data/Utilities/MOBPROBLEMSTOFFOGD.json")
prob_mat_official_clean <- prob_mat_official   |>
  mutate(type = "special deposits") |>
  select(name = "ADRESSE", type, geometry) 



mapview(recycling_center_official, col.region= "blue") + mapview(test, col.region = "red") + mapview(prob_mat_official_clean, col.region= "green")



#complement with osm library data 
vienna_bb = osmdata::opq(st_bbox(data_zbez))   # bb bounding box 

waste_containers_osm = vienna_bb |>
  osmdata::add_osm_feature(key="amenity", value="recycling") |>  #
  osmdata_sf() 

waste_containers_points_osm = waste_containers_osm$osm_points  
waste_containers_polygons_osm = waste_containers_osm$osm_polygons
waste_containers_centroids_osm = st_centroid(waste_containers_osm$osm_polygons)

#data cleaning: no double counting of points in polygones
points_that_touch = st_touches(waste_containers_points_osm, waste_containers_polygons_osm)
points_that_are_within =  st_within(waste_containers_points_osm, waste_containers_polygons_osm)

# make binary version
touches_binary = lengths(points_that_touch) > 0
within_binary = lengths(points_that_are_within) > 0

waste_containers_points_osm[["touches_polygon"]] = touches_binary
waste_containers_points_osm[["within_polygon"]] = within_binary

# wenn einer von beinden wahr ist, dann pounkt rauswerfen
waste_containers_points_osm_clean <- waste_containers_points_osm |>
  mutate(
    punkt_behalten = case_when(
      !touches_polygon & !within_polygon ~ TRUE,
      .default =  FALSE )) |>
  filter(punkt_behalten)    #delete points

View(waste_containers_data_osm_clean)

waste_containers_data_osm_clean2 <- waste_containers_points_osm_clean |>
 filter(!is.na(amenity)) |>
  mutate(type = "waste containers") |>
  select("name" = osm_id, type, geometry)

View(waste_containers_data_osm_clean2)


# merge data and remove duplicates --------------------------------------------------------------
utilities_data_merged <- bind_rows(recycling_center_official_clean, prob_mat_official_clean, waste_containers_data_osm_clean2) |>
    select(name, type, geometry)

#utilities points 
utilities_data_final <- st_join(utilities_data_merged, data_zbez, join = st_within) |>   
  filter(!is.na(ZBEZ)) |>    # to cut providers not in vienna (they have no ZBEZ data)
  select(name, type, ZBEZ, geometry) |>
  group_by(ZBEZ) 

View(utilities_data_final)

# data saving  --------------------------------------
output_path = here("Data/Utilities/utilities_points.geojson")   # set file path
unlink(output_path)   # delete old version
write_sf(utilities_data_final, output_path)





