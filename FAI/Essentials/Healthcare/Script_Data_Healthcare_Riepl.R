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



#read raw data for healthcare: pharmacy stores, doctors (including physiotherapist and psychotherapist), dentist, and optician
# hospitals INCLUDED

hospitals_official = read_sf("FAI/Data/Healthcare/Hospitals/KRANKENHAUSOGDPoint.shp")

hospitals_official_clean = hospitals_official |>
  select("name"= BEZEICHNUN) |>
  mutate(type = "hospital")

#view(hospitals_official_clean)

# official city data for pharmacy stores and doctors

census_districts_vienna = read_sf("City data/Census Districts/ZAEHLBEZIRKOGDPolygon.shp")   #  census district data from city of Vienna

pharmacy_official = read_sf("FAI/Data/Healthcare/Pharmacy/APOTHEKEOGDPoint.shp")   #official data on pharmacies from Vienna
pharmacy_official_clean <- pharmacy_official |> 
  select("name" = BEZEICHNUN) |>
  mutate(type = "pharmacy") 


doctors_ärtzekammer = read_excel("FAI/Data/Healthcare/Doctors/Doctors_Aertzekammer_adjusted.xlsx")   #7190 observations

doctors_ärtzekammer_clean = doctors_ärtzekammer[!duplicated(doctors_ärtzekammer$Name), ]  #4574 observations
doctors_ärtzekammer_clean$strasse <- sub("/.*", "", doctors_ärtzekammer_clean$strasse)
doctors_ärtzekammer_clean$name <- paste(doctors_ärtzekammer_clean$Vorname, doctors_ärtzekammer_clean$Name, sep = " ")

#View(doctors_ärtzekammer_clean)
#public doctors; private ones are excluded because only privately insured people can go there (regardless their income)
public_doctors_ärtzekammer = doctors_ärtzekammer_clean |> 
  filter(OEGK == "OEGK") |>  # kassenarzt definiert wenn vetrag mit OEGK    # 1171 observations
  mutate(strasse = paste0(strasse, ", Wien")) |> 
  tidygeocoder::geocode(address = "strasse", method = "osm") 

public_doctors_ärtzekammer = public_doctors_ärtzekammer |>
  mutate(amenity = "doctor") |>
  st_as_sf(coords = c("long", "lat"), crs = 4326)

output_path = here("FAI/Data/Healthcare/doctors/public_doctors.geojson")
unlink(output_path)
write_sf(public_doctors_ärtzekammer, output_path)

#private doctors
private_doctors_ärtzekammer = doctors_ärtzekammer_clean |>     # 3403 observations 
  filter(is.na(OEGK)) |>   #privatarzt definiert wenn kein vetrag mit OEGK 
  mutate(strasse = paste0(strasse, ", Wien")) |> 
   tidygeocoder::geocode(address = "strasse", method = "osm") 

private_doctors_ärtzekammer = private_doctors_ärtzekammer |> 
  mutate(amenity = "doctor") |>
  st_as_sf(coords = c("long", "lat"), crs = 4326) |>
  select(name, amenity, geometry)

 output_path = here("FAI/Data/Healthcare/doctors/private_doctors.geojson")
unlink(output_path)
write_sf(private_doctors_ärtzekammer, output_path)

#all doctors 
all_doctors_ärtzekammer = doctors_ärtzekammer_clean |>     # 4574 observations 
  mutate(strasse = paste0(strasse, ", Wien")) |> 
  tidygeocoder::geocode(address = "strasse", method = "osm") 

all_doctors_ärtzekammer = all_doctors_ärtzekammer |> 
  mutate(amenity = "doctor") |>
  st_as_sf(coords = c("long", "lat"), crs = 4326) |>
  select(name, amenity, geometry)

all_doctors_ärtzekammer = read_sf("FAI/Data/Healthcare/doctors/all_doctors.geojson") |>
  mutate(type = "doctor") |>
  select(name, type, geometry)

View(all_doctors_ärtzekammer)
 
output_path = here("FAI/Data/Healthcare/doctors/all_doctors.geojson")
unlink(output_path)
write_sf(all_doctors_ärtzekammer, output_path)

#osm data for dentists and opticians
vienna_bb = osmdata::opq(st_bbox(census_districts_vienna))  # previously: opq("austria vienna")   

#dentist  # also public data available but from 2002
dentist_osm = vienna_bb |>  # no osm data on physiotherapists and psychoterapists
  osmdata::add_osm_feature(key="amenity", value= c("dentist")) |>    
  osmdata_sf()  
dentist_points_osm = dentist_osm$osm_points
dentist_centroids_osm = st_centroid(dentist_osm$osm_polygons) 

dentist_osm_clean <- bind_rows(dentist_points_osm, dentist_centroids_osm) |>   # 264 observations
  filter(!is.na(name)) |>  # no filtering of private vs public possible 
  mutate(type = "dentist") |>
select(name, type, geometry) 

#optician
optician_osm = vienna_bb |>
  osmdata::add_osm_feature(key = "shop", value = c("optician")) |>
  osmdata_sf()
optician_points_osm = optician_osm$osm_points
optician_centroids_osm = st_centroid(optician_osm$osm_polygons) 

optician_osm_clean <- bind_rows(optician_points_osm, optician_centroids_osm) |>    # 191 observations
  filter(!is.na(name)) |>    # no filtering of private vs public possible
  mutate(type = "optician") |>
  select(name, type, geometry)



# data merge and remove duplicates  --------------------------------------------------------------

healthcare_data_merged <- bind_rows(hospitals_official_clean, pharmacy_official_clean, all_doctors_ärtzekammer, dentist_osm_clean, optician_osm_clean) |>
    select(name, type, geometry) 

healthcare_data_unique <- healthcare_data_merged[!duplicated(healthcare_data_merged$name), ]


# data preparation for visualization --------------------------------------

#healthcare points 
healthcare_data_final <- st_join(healthcare_data_unique, census_districts_vienna, join = st_within) |>   
  filter(!is.na(ZBEZ)) |>    # to cut providers not in vienna (they have no ZBEZ data)
  select(name, type, ZBEZ, geometry) |>
  group_by(ZBEZ) 

#View(healthcare_data_final)
# write to disk
output_path = here("FAI/Data/Healthcare/healthcare_points.geojson")

# delete old version
unlink(output_path)
write_sf(healthcare_data_final, output_path)




##### public and private distinction??

#doctors 
#View(doctors_official)  # no difference between private and public

# maybe more information on insuarance in osm data 
doctors_osm = vienna_bb |>  
  osmdata::add_osm_feature(key="amenity", value= c("doctors")) |>    
  osmdata_sf()  
doctors_points_osm = doctors_osm$osm_points
#View(doctors_points_osm)  # only 20 of 1000 observations tagged with private


