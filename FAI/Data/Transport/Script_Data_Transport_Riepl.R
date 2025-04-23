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

car_sharing_official = read_sf("Data/Transport/CARSHARINGOGD.json")
car_sharing_official  
car_sharing_official_clean = car_sharing_official |>
  select("name" = ADRESSE) |>
  mutate(type = "car sharing")

# dataset contains no data. 
# city_bike_official = read_sf("Data/Transport/CITYBIKEOGD.json")

# data on taxi not relevant

#biking data

bike_parking_official = read_sf("Data/Transport/FAHRRADABSTELLANLAGEOGD.json")
bike_parking_official_clean = bike_parking |>
  select("name" = id) |>
  mutate(type = "bike parking")

#public transport
public_transport_official = read_sf("Data/Transport/OEFFHALTESTOGD.json")
public_transport_official_final = public_transport_official |>
  select("name" = HLINIEN) |> 
  mutate(type = "public transport")

View(public_transport_official_final)

# official city data for pharmacy stores and doctors
data_zbez = read_sf("Data/Census Districts/data_zbez.geojson")


# data preparation for visualization --------------------------------------

# transport data 

transport_data1 = bind_rows(car_sharing_official_clean, bike_parking_official_clean)
transport_data2 = bind_rows(transport_data1, public_transport_official_final)
transport_data_final <- st_join(transport_data2, data_zbez, join = st_within) |>   
  filter(!is.na(ZBEZ)) |>    # to cut providers not in vienna (they have no ZBEZ data)
  select(name, type, ZBEZ, geometry) |>
  group_by(ZBEZ) 

View(transport_data_final)


# write to disk
output_path = here("Data/Transport/transport_points.geojson")

# delete old version
unlink(output_path)
write_sf(transport_data_final, output_path)





