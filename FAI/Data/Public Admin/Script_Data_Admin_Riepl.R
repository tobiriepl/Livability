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



#read raw data for law and order: only official police data

admin_official = read_sf("Data/Public Admin/AMTOGD.json")
admin_official_clean = admin_official |>
  select("name" = NAME) |>
  mutate(type = "admin office")

# official city data for pharmacy stores and doctors
data_zbez = read_sf("Data/Census Districts/data_zbez.geojson")


# data preparation for visualization --------------------------------------

#final public admin data 
admin_data_final <- st_join(admin_official_clean, data_zbez, join = st_within) |>   
  filter(!is.na(ZBEZ)) |>    # to cut providers not in vienna (they have no ZBEZ data)
  select(name, type, ZBEZ, geometry) |>
  group_by(ZBEZ) 

View(admin_data_final)
# write to disk
output_path = here("Data/Admin/admin_points.geojson")

# delete old version
unlink(output_path)
write_sf(admin_data_final, output_path)





