## R Code: mapping the Foundational Infrastructure in Vienna

library(sf) 
library(osmdata)
library(tidyverse)
library(mapview) 
library(jsonlite)
library(readr)


#read raw data: museum, theatre, cinema, religious places (e.g., church), gathering place (e.g., community_centre, social_centre, marketplace, pitch, stadium, attraction, playground), event venues (e.g., music avenue, events_venue, exhibition, arts_centre)

#census district data from the city of Vienna
census_districts_vienna = read_sf("Data/Census Districts/ZAEHLBEZIRKOGDPolygon.shp")   

# official city data for museums, osm data for rest 
#museum data
museums_official = read_sf("Data/Culture/Museums/MUSEUMOGDPoint.shp")  # official data on schools from the City of Vienna
museums_official_clean <- museums_official |>
  select(name = "NAME") |>
  mutate(amenity = "museum")

#theatre data
vienna_bb = osmdata::opq(st_bbox(census_districts_vienna))   # bb bounding box 

theatre_osm = vienna_bb |>
  osmdata::add_osm_feature(key= "amenity", value= "theatre") |>
  osmdata_sf() 
theatre_osm_clean <- theatre_osm$osm_points |>
  filter(!is.na(name)) |>
  select(name, amenity, geometry)

# cinema
cinema_osm = vienna_bb |>
  osmdata::add_osm_feature(key= "amenity", value= "cinema") |>
  osmdata_sf() 
cinema_osm_clean <- cinema_osm$osm_points |>
  filter(!is.na(name)) |>
  select(name, amenity, geometry)

# gathering places: two osm request not possible, thats why splitting up in amenity and leisure
gathering_places_osm_amenity = vienna_bb |>
  osmdata::add_osm_feature(key= "amenity", value= c("community_centre", "social_centre", "marketplace")) |>
  osmdata_sf()

gathering_places_osm_amenity_points <- gathering_places_osm_amenity$osm_points |>
  filter(!is.na(name)) |>
  select(name, amenity, geometry)

gathering_places_osm_amenity_polygons = gathering_places_osm_amenity$osm_polygons
gathering_places_osm_amenity_centroids = st_centroid(gathering_places_osm_amenity_polygons)

gathering_places_osm_amenity_clean <- bind_rows(gathering_places_osm_amenity_points, gathering_places_osm_amenity_centroids)

gathering_places_osm_leisure = vienna_bb |>
  osmdata::add_osm_feature(key = "leisure", value = c("pitch", "playground", "stadium", "attraction")) |>
  osmdata_sf() 

gathering_places_osm_leisure_points <- gathering_places_osm_leisure$osm_points |>
  filter(!is.na(name)) |>
  select(name, amenity, geometry)

gathering_places_osm_leisure_polygons = gathering_places_osm_leisure$osm_polygons
gathering_places_osm_leisure_centroids = st_centroid(gathering_places_osm_leisure_polygons)

gathering_places_osm_leisure_clean <- bind_rows(gathering_places_osm_leisure_points, gathering_places_osm_leisure_centroids)


gathering_places_osm_clean <- bind_rows(gathering_places_osm_leisure_clean, gathering_places_osm_amenity_clean) |>
  filter(!is.na(name)) |>
  select(name, amenity, geometry) |>
  mutate(amenity = "gathering_places")
  


# event venues
event_venues_osm = vienna_bb |>
  osmdata::add_osm_feature(key= "amenity", value= c("events_venue", "exhibition", "music_venue", "arts_centre")) |>
  osmdata_sf() 
event_venues_osm_clean <- event_venues_osm$osm_points |>
  filter(!is.na(name)) |>
  select(name, amenity, geometry) |>
  mutate(amenity = "event_venues")

#religious places
religious_places_osm = vienna_bb |>
    osmdata::add_osm_feature(key="building", value = c("cathedral", "church", "mosque", "synagogue")) |>
    osmdata_sf()  
religious_places_osm_points <-religious_places_osm$osm_points |>
  filter(!is.na(name)) |>
  select(name, amenity, geometry)

religious_places_osm_polygons = religious_places_osm$osm_polygons
religious_places_osm_centroids  = st_centroid(religious_places_osm_polygons)

religious_places_osm_clean <- bind_rows(religious_places_osm_points, religious_places_osm_centroids) |>
  mutate(amenity = "religious_place")

  


# merge data and remove duplicates  --------------------------------------------------------------

culture_data_merged <- bind_rows(museums_official_clean, theatre_osm_clean, cinema_osm_clean, gathering_places_osm_clean, event_venues_osm_clean, religious_places_osm_clean) |>
    select(name, amenity, geometry) 

culture_data_unique <- culture_data_merged[!duplicated(culture_data_merged$name), ]


# final data preparation and saving  --------------------------------------

culture_data_final <- st_join(culture_data_unique, census_districts_vienna, join = st_within) |>   
  filter(!is.na(ZBEZ)) |>    # to cut providers not in Vienna (they have no ZBEZ data)
  select(name, amenity, ZBEZ, geometry) |>
  group_by(ZBEZ) 

mapview(culture_data_final)

write_sf(culture_data_final, "Data/culture/culture_points.geojson")







