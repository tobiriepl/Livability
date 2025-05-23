## R Code: mapping the Foundational Infrastructure in Vienna

library(sf) # standard paket für vektor geodaten in R    # spatial formation package
library(osmdata)
library(tidyverse)
library(mapview) # interkative, schnelle karten
library(ggthemes) # pretty ggplots
library(here)
library(glue)
library(jsonlite)
library(hereR)
library(readr)
library(ggthemes)   # to get theme map from tom



# data --------------------------------------------------------------------
healthcare_points = read_sf("Data/Healthcare/healthcare_points.geojson")  
data_zbez = read_sf("Data/Census Districts/ZAEHLBEZIRKOGDPolygon.shp")
data_iso = read_sf(here("Data/Isochrones/Isochrones.geojson")) 


# healthcare data of neighborhoods, saved on disc ----------------------------------------------------------

# für jeden zbez ----------------------------------------------------------
cents = st_centroid(data_zbez)
isochrones=c(15)
apiKey = readLines(here("Data/API/api.txt"))
set_key(api_key = apiKey)


result = map(1:250, function(i){
  
  cat(glue(i, "/", 250, "\r"))
  
  # zbez nr i (zeilennumnner)
  
  # centroid
  row = cents[i,]
  
  
  # zbez
  zbez = data_zbez[i, ]
  
  # ISOCHRONE
  times = isochrones *60 # time in seconds
  
  iso = hereR::isoline(        # use hereR package to send API request
    row,
    datetime = Sys.time(),
    range = times,
    range_type = "time",
    transport_mode = "pedestrian"
  )
  
  
  # find points in zbez, similar to: st_intersection(data_points, zbez)
  healthcare_in_zbez = data_points[zbez, ] %>%
    mutate(
      inZbez = TRUE
    )
  
  # find points in isochrone
  healthcare_in_isochrone = data_points[iso, ] %>%
    mutate(
      inZbez = FALSE
    )
  
  #  in isochrone, aber nicht in zbez
  healthcare_in_iso_but_no_zbez = anti_join(healthcare_in_isochrone, healthcare_in_zbez %>% st_drop_geometry(), join_by(name))
  
  # in zbez + in isochrone (aber nicht in zbez)
  healthcare_iso_zbez = bind_rows(healthcare_in_zbez, healthcare_in_iso_but_no_zbez)
  
  
  # um welchen zbez handelt es sich
  healthcare_iso_zbez[["zbez"]] = zbez[["ZBEZ"]]
  # mapview(healthcare_iso_zbez, zcol="inZbez")
  
  # weights
  # make long to wide!
  healthcare_iso_zbez %>%
    st_drop_geometry() %>%
    group_by(zbez, inZbez, amenity) %>%
    summarise(
      n_amenities = n()
    ) %>%
    pivot_wider(
      names_from = c("inZbez", "amenity"),
      values_from = "n_amenities"
    ) -> final
  
  
  return(final)
  
  
  
})


# alle Zählbezirke zusammenschreiben
all_results = result |> bind_rows() |>
  arrange(zbez)  

# append the ZBEZ number of the missing ones
# the ones filtered out above, will have 0s in all columns
all_results_final = right_join(all_results, data_zbez, join_by(zbez == ZBEZ)) |>
  select( "ZBEZ" = zbez, "Doctor_Iso" = FALSE_doctor, "Pharmacy_Iso" = FALSE_pharmacy, "Doctor_Zbez" = TRUE_doctor, "Hospital_Zbez" = TRUE_hospital, "Pharmacy_Zbez" = TRUE_pharmacy, "Hospital_Iso" = FALSE_hospital)

#data cleaning and FAI preparation (for weightening + per capita)
all_results_final[is.na(all_results_final)] <- 0   # to replace NAs with 0 for FAI calculation

View(all_results_final)

# include population
data_pop <- read_sf("Data/Population/Population_Vienna.geojson")


#FAI calculation
FAI_healthcare <- merge(all_results_final, data_pop, by="ZBEZ") |> 
  mutate( FAI_Zbez = Hospital_Zbez * 20 + Pharmacy_Zbez *5 + Doctor_Zbez * 1,     # weighted per amenity
          FAI_Iso = (Hospital_Iso * 20 + Pharmacy_Iso *5 + Doctor_Iso * 1) * 0.5, # weighted per amenity and neighboring area
          FAI_total = (FAI_Zbez + FAI_Iso) / POP_TOTAL,    # how to integrate population?? 
          FAI_mean = mean(FAI_total, na.rm = T),   # to get the FAI standardized: mean, sd and z variation
          FAI_sd = sd(FAI_total, na.rm = T), 
          FAI_final = (FAI_total - FAI_mean) / FAI_sd) |> 
  select(ZBEZ, FAI_final, geometry)  |>
  st_as_sf()

FAI_healthcare[is.na(FAI_healthcare)] <- 0   # to replace NaNs with 0 for plotting

View(FAI_healthcare)


# write to disk
output_path = here("FAI/Healthcare/FAI_healthcare.geojson")
# delete old version
unlink(output_path)
write_sf(healthcare_data, output_path)



# just checking..plotting final FAI data ----------------------------------------------------------

# easier to remove geometry column and merge again with census district dataset


ggplot() +
  geom_sf(data = healthcare_data, aes(fill= FAI_final),
          color = "grey20", 
          size = 0.4) +
  geom_sf(data = healthcare_points, 
          aes(shape = amenity), 
          size = 0.4) +
  theme_map() +
  scale_fill_continuous(type = "viridis",
                        direction = -1) +
  scale_shape_discrete() + 
  
  theme(legend.position = "left")





