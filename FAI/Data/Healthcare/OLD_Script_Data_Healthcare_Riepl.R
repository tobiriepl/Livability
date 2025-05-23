## R Code: mapping the Foundational Infrastructure in Vienna

library(sf)    # spatial formation package 
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


#read raw data for healthcare: hospitals, pharmacy stores, doctors, physiotherapist, psychotherapist, and optician
# official city data for hospitals, pharmacy stores, adn doctors

census_districts_vienna = read_sf("Data/Census Districts/ZAEHLBEZIRKOGDPolygon.shp")   #  census district data from city of Vienna
  
hospitals_official = read_sf("Data/Healthcare/Hospitals/KRANKENHAUSOGDPoint.shp")  # official data on hospitals from Vienna

hospitals_official_clean <- hospitals_official |>
  select("name" = BEZEICHNUN)  |> 
  mutate(amenity = "hospital") 

View(dentist)

pharmacy_official = read_sf("Data//Healthcare/Pharmacy/APOTHEKEOGDPoint.shp")   #official data on pharmacies from Vienna

pharmacy_official_clean <- pharmacy_official |> 
  select("name" = BEZEICHNUN) |>
  mutate(amenity = "pharmacy") 

doctors_official = read_sf("Data/Healthcare/Doctors/ARZTOGDPoint.shp")  # official data on doctors including physiothereapists and psychotherapists

doctors_official_clean <- doctors_official |>  # official doctor data includes psychiatrists
  select(name = "NAME") |>
  mutate(amenity = "doctor") 


#osm data for dentists and opticians

vienna_bb = osmdata::opq(st_bbox(census_districts_vienna))  # previously: opq("austria vienna")   

dentist_osm = vienna_bb |>  # no osm data on physiotherapists and psychoterapists
  osmdata::add_osm_feature(key="amenity", value= c("dentist")) |>    
  osmdata_sf()  
dentist_points_osm = dentist_osm$osm_points
dentist_points_osm_clean <- dentist_points_osm |>
  filter(!is.na(name)) |>
  select(name, amenity, geometry) |>   # check if true?!?! no polygons needed. data clean (or small enough providers)
  drop_na()  ### check if it works ?!?!?!?!

optician_osm = vienna_bb |>
  osmdata::add_osm_feature(key = "shop", value = c("optician")) |>
  osmdata_sf()
optician_points_osm = optician_osm$osm_points

optician_points_osm_clean <- optician_points_osm |>
  filter(!is.na(name)) |>
  mutate(amenity = "optician") |>
  select(name, amenity, geometry) |>   # check if true?!?! no polygons needed. data clean (or small enough providers)
  drop_na()  ### check if it works ?!?!?!?!

healthcare_points_osm_clean <- bind_rows(dentist_points_osm_clean, optician_points_osm_clean)   # get osm data points, data clean, no spatial cleaning needed for libraries



# data merge and remove duplicates  --------------------------------------------------------------

healthcare_data_merged <- bind_rows(hospitals_official_clean, pharmacy_official_clean, doctors_official_clean, healthcare_points_osm_clean) |>
    select(name, amenity, geometry) 

healthcare_data_unique <- healthcare_data_merged[!duplicated(healthcare_data_merged$name), ]


# data preparation for visualization --------------------------------------

#healthcare points 
healthcare_data_final <- st_join(healthcare_data_unique, census_districts_vienna, join = st_within) |>   
  filter(!is.na(ZBEZ)) |>    # to cut providers not in vienna (they have no ZBEZ data)
  select(name, amenity, ZBEZ, geometry) |>
  group_by(ZBEZ) 

write_sf(healthcare_data_final, "Data/healthcare/healthcare_points.geojson")





