## R Code: mapping the Foundational Infrastructure in Vienna

library(sf) 
library(osmdata)
library(tidyverse)
library(mapview) 
library(jsonlite)
library(readr)
library(stringr)



#read raw data: museum, theatre, and cinema

#census district data from the city of Vienna
census_districts_vienna = read_sf("Data/Census Districts/ZAEHLBEZIRKOGDPolygon.shp")   

# culture data: museums (official), theatre (osm), cinema (osm), playgrounds (official) and attractions (official)  
#museum data
museums_official = read_sf("Data/Culture/Museums/MUSEUMOGDPoint.shp")  # official data on schools from the City of Vienna
museums_official_clean <- museums_official |>
  select(name = "NAME") |>
  mutate(type = "museum")


# meeting zones
meeting_zones_official = read_sf("Data/Culture/Meeting Zones/BEGEGNUNGSZONEOGD.json")
meeting_zones_official_clean <- st_centroid(meeting_zones_official) |>
  select(name = "id") |>
  mutate(type = "meeting zone")

attractions_official = read_sf("Data/Culture/Attractions/SEHENSWUERDIGOGD.json")
attractions_official_clean <- attractions_official |>
  select(name = "NAME") |>
  mutate(type = "attraction")


#theatre data
vienna_bb = osmdata::opq(st_bbox(census_districts_vienna))   # bb bounding box 

theatre_osm = vienna_bb |>
  osmdata::add_osm_feature(key= "amenity", value= "theatre") |>
  osmdata_sf() 
theatre_points_osm <- theatre_osm$osm_points 
theatre_polygons_osm = theatre_osm$osm_polygons
theatre_centroids_osm = st_centroid(theatre_polygons_osm)

theatre_data_clean <- bind_rows(theatre_points_osm, theatre_centroids_osm) |>
  filter(!is.na(name)) |>
  select(name, type = amenity, geometry)


# cinema
cinema_osm = vienna_bb |>
  osmdata::add_osm_feature(key= "amenity", value= "cinema") |>
  osmdata_sf() 
cinema_points_osm <- cinema_osm$osm_points 
cinema_polygons_osm = cinema_osm$osm_polygons
cinema_centroids_osm  = st_centroid(cinema_polygons_osm)

cinema_data_clean <- bind_rows(cinema_points_osm, cinema_polygons_osm) |>
  filter(!is.na(name)) |>
  select(name, type = amenity, geometry)


# community centre
community_centre_osm = vienna_bb |>
  osmdata::add_osm_feature(key= "amenity", value= c("community_centre", "social_centre", "marketplace")) |>
  osmdata_sf()

community_centre_points_osm <- community_centre_osm$osm_points
community_centre_centroids_osm <- st_centroid(community_centre_osm$osm_polygons)

community_centre_clean = bind_rows(community_centre_centroids_osm, community_centre_points_osm) |>
  filter(!is.na(name)) |>
  select(name, type = amenity, geometry) |>
  mutate(type = "community centre")


# event venues
event_venues_osm = vienna_bb |>
  osmdata::add_osm_feature(key= "amenity", value= c("events_venue", "exhibition", "music_venue", "arts_centre")) |>
  osmdata_sf() 
event_venues_points_osm <- event_venues_osm$osm_points 
event_venues_centroids_osm <- st_centroid(event_venues_osm$osm_polygons)

event_venues_clean <- bind_rows(event_venues_points_osm, event_venues_centroids_osm) |>
  filter(!is.na(name)) |>
  select(name, type = amenity, geometry) |>
  mutate(type = "event venue")


#public_art_official <- read_sf("Data/Culture/Public Art/KUNSTWERKOGD.json")    #deceives results because so big 
#public_art_official_clean <- public_art_official |>
#  select(name = "OBJEKTTITEL") |>
#  mutate(amenity = "public_art")  |>
#  filter(!str_detect(name, "Gedenktafel|Brunnen|Denkmal"))

# markets_official = read_sf("Data/Culture/Markets/MAERKTEOGD.json")   # too little observations 
# markets_official_clean <- markets_official |>
#  select(name = "NAME") |>
#  mutate(amenity = "market")


#playground
#playground_official = read_sf("Data/Culture/Playgrounds/SPIELPLATZPUNKTOGD.json")
#playground_official_clean <- playground_official |> 
##  mutate(amenity = "playground") |>
#  select(name = "ANL_NAME", amenity, SPIELPLATZ_DETAIL, geometry)


# religion
#religion_osm = vienna_bb |>
#  osmdata::add_osm_feature(key = "amenity", value = "place_of_worship") |>
#  osmdata_sf()

# religion_points = religion_osm$osm_points
# religion_centroids = st_centroid(religion_osm$osm_polygon) 

# religion_clean <- bind_rows(religion_centroids, religion_points) |>
#  filter(!is.na(name)) |>
#  select(name, amenity, geometry)

#sport facilities
#sport_facilities = read_sf("Data/Culture/Sport Facilities/SPORTSTAETTENOGD.json")
#sport_facilities_clean <- sport_facilities |>
#    filter(str_detect(KATEGORIE_TXT, "Öffentlich indoor|Öffentlich outdoor"))


# merge data and remove duplicates  --------------------------------------------------------------
culture_data_merged <- bind_rows(museums_official_clean, theatre_data_clean, cinema_data_clean, attractions_official_clean, event_venues_clean, community_centre_clean) 
culture_data_unique <- culture_data_merged[!duplicated(culture_data_merged$name), ]


# final data preparation and saving  --------------------------------------

culture_data_final <- st_join(culture_data_unique, census_districts_vienna, join = st_within) |>   
  filter(!is.na(ZBEZ)) |>    # to cut providers not in Vienna (they have no ZBEZ data)
  select(name, type, ZBEZ, geometry) |>
  group_by(ZBEZ) |>
  drop_na()


# write to disk
output_path = here("Data/Culture/culture_points.geojson")

# delete old version
unlink(output_path)
write_sf(culture_data_final, output_path)






