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
census_districts_vienna = read_sf("Data/Census Districts/ZAEHLBEZIRKOGDPolygon.shp")   #  census district data from city of Vienna

universities_official = read_sf("Data/Education/Universität/UNIVERSITAETOGD.json")
universities_official_clean = universities_official |>
  mutate(type = "university") |>
  select(name = "NAME", type, geometry)
  
schools_official = read_sf("Data/Education/Schools/SCHULEOGDPoint.shp")  # official data on schools from the City of Vienna
schools_official_clean <- schools_official   |>
 # mutate(ART_TXT = stri_trans_tolower(ART_TXT, locale = "en_US.UTF-8")) |>
  #filter(!str_detect(tolower(ART_TXT), "privat")) |>  # 170 schools of 800 are private
  mutate(type = "school") |>
  select(name = "NAME", type, geometry, ART_TXT) 
# schools_official_clean <- schools_official_clean[!apply(schools_official_clean, 1, function(row) any(grepl('privat', row, ignore.case = TRUE))), ]   # dplyr filtering not possible because of data  # no private filtering

music_schools_official = read_sf("Data/Education/Music School/MUSIKSINGSCHULEOGD.json")
music_schools_official_clean <- music_schools_official |>
  select(name = "NAME") |>
  mutate(type = "music school")

education_center_official = read_sf("Data/Education/Volkshochschule/VOLKSHOCHSCHULEOGD.json")
education_center_official_clean <- education_center_official |>
  select(name = "NAME") |>
  mutate(type = "education center")

#complement with osm data

vienna_bb = osmdata::opq(st_bbox(census_districts_vienna))   # bb bounding box 

education_center_osm = vienna_bb |>
  osmdata::add_osm_feature(key="amenity", value="training") |>  #
  osmdata_sf() 

education_center_points_osm = education_center_osm$osm_points    # only points, no polygons exist
education_center_points_osm_clean <- education_center_points_osm |>
  filter(!is.na(name)) |>
  mutate(type = "education center") |>
  select(name, type, geometry)

View(education_center_points_osm_clean)
library_points_merged <- bind_rows(library_points_osm_clean, libraries_official_clean)
library_points_unique <- library_points_merged[!duplicated(library_points_merged$name), ]



libraries_official = read_sf("Data/Education/Libraries/BUECHEREIOGD.json")
libraries_official_clean <- libraries_official |>
  select(name = "NAME") |>
  mutate(type = "library")

#complement with osm library data 
vienna_bb = osmdata::opq(st_bbox(census_districts_vienna))   # bb bounding box 

library_osm = vienna_bb |>
  osmdata::add_osm_feature(key="amenity", value="library") |>  #
  osmdata_sf() 

library_points_osm = library_osm$osm_points    # only points, no polygons exist
library_points_osm_clean <- library_points_osm |>
  filter(!is.na(name)) |>
  mutate(type = "library") |>
  select(name, type, geometry)

library_points_merged <- bind_rows(library_points_osm_clean, libraries_official_clean)
library_points_unique <- library_points_merged[!duplicated(library_points_merged$name), ]

#add langugage schools from osm
language_school_osm = vienna_bb |>
  osmdata::add_osm_feature(key="amenity", value="language_school") |>  #
  osmdata_sf()  

language_school_points_osm = language_school_osm$osm_points
language_school_centroids_osm = st_centroid(language_school_osm$osm_polygons)

language_school_points_osm_clean = bind_rows(language_school_points_osm, language_school_centroids_osm)  
# language_school_points_osm_clean <- language_school_points_osm_clean[!apply(language_school_points_osm_clean, 1, function(row) any(grepl('privat', row, ignore.case = TRUE))), ]   #no private filtering
language_school_points_osm_clean = language_school_points_osm_clean |> 
  filter(!is.na(name)) |>
  mutate(type = "language school") |>
  select(name, type, geometry)

# merge data and remove duplicates --------------------------------------------------------------
education_data_merged <- bind_rows(universities_official_clean, schools_official_clean, library_points_unique, music_schools_official_clean, education_center_official_clean, language_school_points_osm_clean) |>
    select(name, type, geometry)

#education_data_unique <- education_data_merged[!duplicated(education_data_merged[c("name", "geometry")]), ]



#education points 
education_data_final <- st_join(education_data_merged, census_districts_vienna, join = st_within) |>   
  filter(!is.na(ZBEZ)) |>    # to cut providers not in vienna (they have no ZBEZ data)
  select(name, type, ZBEZ, geometry) |>
  group_by(ZBEZ) |>
  drop_na()

View(education_data_final)

# data saving  --------------------------------------
output_path = here("Data/Education/education_points.geojson")   # set file path
unlink(output_path)   # delete old version
write_sf(education_data_final, output_path)




# private vs public

# schools
View(schools_official)  # 150 of 850 are private

#library 
View(libraries_official)  # no private ones 

#education center
View(education_center_official)  # no private ones in dataset, mostly VHS

# music schools
View(music_schools_official)   # not clear, but official data. so probably mostly public

#language schools
View(language_school_points_osm)   # 4 of 18 private

