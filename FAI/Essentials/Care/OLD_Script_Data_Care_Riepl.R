## R Code: mapping the Foundational Infrastructure in Vienna

library(sf) # mittlerweile standard paket für vektor geodaten in R    # spatial formation package 
library(osmdata)
library(tidyverse)
library(mapview) # interkative, schnelle karten
library(jsonlite)
library(readr)


#read raw data
census_districts_vienna = read_sf("Data/Census Districts/ZAEHLBEZIRKOGDPolygon.shp")   #  census district data from city of Vienna
  
# care: nursing_home, social_facility, kindergarten, childcare

# overpass query = API (application programming interface) von OSM
vienna_bb = osmdata::opq(st_bbox(census_districts_vienna))    # bb bounding box 

care_osm = vienna_bb |>
  osmdata::add_osm_feature(key="amenity", value= c("nursing_home", "social_facility", "kindergarten", "childcare", "hospice", "nurse")) |>
  osmdata_sf()  
  

# data preparation  --------------------------------------

# extract suitable osm data

care_osm = care_osm$osm_points  # get osm data points, data clean, no spatial cleaning needed for libraries
care_osm_clean <- care_osm |>
  filter(!is.na(name)) |>
  select(name, amenity, geometry) |>   # no polygons needed. data clean (or small enough providers)
  drop_na()  ### check if it works ?!?!?!?!

# remove duplicates
care_data_unique <- care_osm_clean[!duplicated(care_osm_clean$name), ]


# care points 
care_data_final <- st_join(care_data_unique, census_districts_vienna, join = st_within) |>   
  filter(!is.na(ZBEZ)) |>    # to cut providers not in vienna (they have no ZBEZ data)
  select(name, amenity, ZBEZ, geometry) |>
  group_by(ZBEZ) 

mapview(care_data_final)
write_sf(care_data_final, "Data/Care/care_points.geojson")





