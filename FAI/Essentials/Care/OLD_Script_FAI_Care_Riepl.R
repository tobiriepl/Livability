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
care_points = read_sf("Data/Care/care_points.geojson")  
data_zbez = read_sf("Data/Census Districts/ZAEHLBEZIRKOGDPolygon.shp")
data_iso = read_sf(here("Data/Isochrones/Isochrones.geojson")) 


# care data of neighborhoods, saved on disc ----------------------------------------------------------

# für jeden zbez ----------------------------------------------------------
cents = st_centroid(data_zbez)
isochrones=c(15)
apiKey = readLines(here("Data/API/api.txt"))
set_key(api_key = apiKey)


result = map(1:250, function(i){
  
  cat(glue(i, "/", 250, "\r"))
  
  # zbez nr i (zeilennummer)
  
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
  care_in_zbez = care_points[zbez, ] %>%
    mutate(
      inZbez = TRUE
    )
  
  # find points in isochrone
  care_in_isochrone = care_points[iso, ] %>%
    mutate(
      inZbez = FALSE
    )
  
  #  in isochrone, aber nicht in zbez
  care_in_iso_but_no_zbez = anti_join(care_in_isochrone, care_in_zbez %>% st_drop_geometry(), join_by(name))
  
  # in zbez + in isochrone (aber nicht in zbez)
  care_iso_zbez = bind_rows(care_in_zbez, care_in_iso_but_no_zbez)
  
  
  # um welchen zbez handelt es sich
  care_iso_zbez[["zbez"]] = zbez[["ZBEZ"]]
  # mapview(care_iso_zbez, zcol="inZbez")
  
  # weights
  care_iso_zbez |>
    st_drop_geometry() |>
    group_by(zbez, inZbez, amenity) |>
    summarise(
      n_amenities = n()
    ) |>
    pivot_wider(        # make long to wide!
      names_from = c("inZbez", "amenity"),
      values_from = "n_amenities"
    ) -> final
  
  
  return(final)
  
  
  
})

# alle Zählbezirke zusammenschreiben
all_results = result |> bind_rows() |>
  arrange(zbez)  

View(all_results)

# append the ZBEZ number of the missing ones
# the ones filtered out above, will have 0s in all columns
all_results_final = right_join(all_results, data_zbez, join_by(zbez == ZBEZ)) |>
  select( "ZBEZ" = zbez, "Social_Facility_Iso" = FALSE_social_facility, "Childcare_Iso" = FALSE_childcare, "Kindergarten_Iso" = FALSE_kindergarten, "Social_Facility_Zbez" = TRUE_social_facility, "Childcare_Zbez" = TRUE_childcare, "Kindergarten_Zbez" = TRUE_kindergarten)

#data cleaning and FAI preparation (for weightening + per capita)
all_results_final[is.na(all_results_final)] <- 0   # to replace NAs with 0 for FAI calculation

View(all_results_final)

# include population
data_pop <- read_sf("Data/Population/Population_Vienna.geojson")


#FAI calculation
FAI_care <- merge(all_results_final, data_pop, by="ZBEZ") |> 
  mutate( FAI_Zbez = Childcare_Zbez * 1 + Kindergarten_Zbez *1 + Social_Facility_Zbez * 1,     # weighted per amenity: how ?!
          FAI_Iso = (Childcare_Iso * 1 + Kindergarten_Iso *1 + Social_Facility_Iso * 1) * 0.5, # weighted per amenity and neighboring area
          FAI_total = (FAI_Zbez + FAI_Iso) / POP_TOTAL,    # how to integrate population?? 
          FAI_mean = mean(FAI_total, na.rm = T),   # to get the FAI standardized: mean, sd and z variation
          FAI_sd = sd(FAI_total, na.rm = T), 
          FAI_final = (FAI_total - FAI_mean) / FAI_sd) |> 
  select(ZBEZ, FAI_final, geometry)  |>
  st_as_sf()

FAI_care[is.na(FAI_care)] <- 0   # to replace NaNs with 0 for plotting

View(care_points)


# write to disk
output_path = here("FAI/Care/FAI_care.geojson")
# delete old version
unlink(output_path)
write_sf(FAI_care, output_path)



# just checking..plotting final FAI data ----------------------------------------------------------

# easier to remove geometry column and merge again with census district dataset


ggplot() +
  geom_sf(data = FAI_care, aes(fill= FAI_final),
          color = "grey20", 
          size = 0.4) +
  geom_sf(data = care_points, 
          aes(shape = amenity), 
          size = 0.4) +
  theme_map() +
  scale_fill_continuous(type = "viridis",
                        direction = -1) +
  scale_shape_discrete() + 
  
  theme(legend.position = "left")





