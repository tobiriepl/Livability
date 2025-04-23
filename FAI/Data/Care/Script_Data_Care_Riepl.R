## R Code: mapping the Foundational Infrastructure in Vienna

library(sf) # mittlerweile standard paket für vektor geodaten in R    # spatial formation package 
library(osmdata)
library(tidyverse)
library(mapview) # interkative, schnelle karten
library(jsonlite)
library(readr)
library(stringr)


#read raw data
census_districts_vienna = read_sf("Data/Census Districts/ZAEHLBEZIRKOGDPolygon.shp")   #  census district data from city of Vienna
  
# care: nursing_home, social_facility, kindergarten, childcare

# nursing_homes and kindergarten from data.gv.at, social facilty and childcare from osm 

nursing_homes_official = read_sf("Data/Care/Wohnungs- und Pflegehäuser/WOHNPFLEGEHAUSOGD.json")   #no private public filtering possible
nursing_homes_official_clean <- nursing_homes_official |> 
  mutate(type = "nursing home") |>
  select(name = "BEZEICHNUNG", type, geometry)

kindergarten_official = read_sf("Data/Care/Kindergarten/KINDERGARTENOGD.json")  # only 400 of 1450 kindergartens are public
kindergarten_official_clean <- kindergarten_official |> 
  mutate(type = "kindergarten") |>
#  filter(TXTATT1 != "Privat") |>
  select(name = "BEZEICHNUNG", type, geometry)

#get social facilities like homeless shelters via osm
# overpass query = API (application programming interface) von OSM
vienna_bb = osmdata::opq(st_bbox(census_districts_vienna))    # bb bounding box 

social_facility_osm = vienna_bb |>
  osmdata::add_osm_feature(key = "amenity", value = c("social_facility")) |>
  osmdata_sf()

social_facility_points_osm = social_facility_osm$osm_points 
social_facility_centroids_osm = st_centroid(social_facility_osm$osm_polygons) 

#data merge
social_facility_merged_osm <- bind_rows(social_facility_points_osm, social_facility_centroids_osm) |>
  filter(!is.na(name)) |>
  mutate(type = "social facility") |>
  select(name, type, geometry) |>
  drop_na()

# remove duplicates
social_facility_unique_osm <- social_facility_merged_osm[!duplicated(social_facility_merged_osm$name), ]

# re-categorize care data: some social facilities are nursing homes
 
 social_facility_rearranged_osm <- social_facility_unique_osm |>  # maybe smoother way to do so
   filter(!str_detect(name, "Pflege|pflege|Carolusheim|Haus Rossau|Senioren Residenz|Haus Rudolfsheim|Sonores|Haus Gustav Klimt|Haus An der Türkenschanze|Pensionistenwohnhaus Prater|Haus Wienerberg für Pensionisten|Pensionistenheim|Pensionistenheim Trazerberg|Haus Jedlersdorf|Haus St. Bernadette|Haus Penzing|Senior|SeneCura Residenz Oberdöbling|Haus Klosterneuburg|Caritas Haus Klosterneuburg|Haus Schmelz|Haus Neubau|Haus der Barmherzigkeit|Haus St. Elisabeth|Haus Hohe Warte|Haus Brigittenau|Kursana|Haus St. Barbara|Haus St. Lukas|Haus Laaerberg|Haus Döbling|Haus am Mühlengrund|Haus Haidehof|Haus Atzgersdorf|Haus Rosenberg|ÖJAB-Haus Neumargareten")) |>   
  filter(!str_detect(name, "hotel")) # data bug

care_data_merged <- bind_rows(social_facility_rearranged_osm, kindergarten_official_clean, nursing_homes_official_clean)
View(care_data_merged)   # no name duplicate filtering possible because of incomplete names of kindergartens

# final care points
care_data_final <- st_join(care_data_merged, census_districts_vienna, join = st_within) |>   
  filter(!is.na(ZBEZ)) |>    # to cut providers not in vienna (they have no ZBEZ data)
  select(name, type, ZBEZ, geometry) |>
  group_by(ZBEZ) |>
  drop_na()   # get rid of one NA in data
 
View(care_data_final)

# data saving  --------------------------------------
output_path = here("Data/Care/care_points.geojson")   # set file path
unlink(output_path)   # delete old version
write_sf(care_data_final, output_path)
