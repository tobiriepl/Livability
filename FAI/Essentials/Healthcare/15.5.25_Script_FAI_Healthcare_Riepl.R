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
healthcare_points = read_sf("FAI/Data/Healthcare/healthcare_points.geojson")  
data_zbez = read_sf("City data/Census Districts/ZAEHLBEZIRKOGDPolygon.shp")
data_bez = read_sf("City data/Districts/data_bez.geojson")
data_iso = read_sf(here("City data/Isochrones/Isochrones15min.geojson")) 
data_pop = read_sf("City data/Population/data_pop.geojson")  |>
  group_by(BEZ) |>
  summarize(POP_TOTAL = sum(POP_TOTAL, na.rm = TRUE))
data_ring = read_sf("City data/Aesthetics/data_ring.geojson")
data_gürtel = read_sf("City data/Aesthetics/data_gürtel.geojson")
data_borders = read_sf("City data/Aesthetics/data_borders.geojson")
data_donau = read_sf("City data/Aesthetics/data_donau.geojson")
data_donaukanal = read_sf("City data/Aesthetics/data_donaukanal.geojson")
data_lake = read_sf("City data/Aesthetics/data_lake.geojson")

# healthcare data of neighborhoods, saved on disc ----------------------------------------------------------

data_healthcare = merge(data_zbez, healthcare_points |> st_drop_geometry(), by = "ZBEZ") |>
  select(BEZ, name, type, geometry)

#View(data_healthcare)

result = map(1:23, function(i){  # für jeden zbez
  
  cat(glue(i, "/", 23, "\r"))
  
  iso = data_iso[i, ]
  
  bez = data_bez[i, ]
  
  # find points in zbez, similar to: st_intersection(data_points, zbez)
  healthcare_in_bez = data_healthcare[bez, ] %>%
    mutate(inBez = TRUE)
  
  # find points in isochrone
  healthcare_in_isochrone = data_healthcare[iso, ] %>%
    mutate(inBez = FALSE)
  
  #  in isochrone, aber nicht in zbez
  healthcare_in_iso_but_no_bez = anti_join(healthcare_in_isochrone, healthcare_in_bez %>% st_drop_geometry(), join_by(name))
  
  # in bez + in isochrone (aber nicht in bez)
  healthcare_iso_bez = bind_rows(healthcare_in_bez, healthcare_in_iso_but_no_bez)
  
  
  # um welchen zbez handelt es sich
  healthcare_iso_bez[["bez"]] = bez[["BEZ"]]
  
  # weights
  healthcare_iso_bez |>
    st_drop_geometry() |>
    group_by(bez, inBez, type) |>
    summarise(
      n_amenities = n()
    ) |>
    pivot_wider(        # make long to wide
      names_from = c("inBez", "type"),
      values_from = "n_amenities"
    ) -> final
  
  return(final)
  
})

# alle Zählbezirke zusammenschreiben
all_results = result |> bind_rows() |>
  arrange(bez)  

#View(healthcare_points)
# append the ZBEZ number of the missing ones
# the ones filtered out above, will have 0s in all columns
all_results_final = right_join(all_results, data_bez, join_by(bez == BEZ)) |>
  select( "BEZ" = bez, "Hospital_Iso" = FALSE_hospital, "Doctor_Iso" = FALSE_doctor, "Pharmacy_Iso" = FALSE_pharmacy, "Dentist_Iso" = FALSE_dentist, "Optician_Iso" = FALSE_optician, 
          "Hospital_Bez" = TRUE_hospital, "Doctor_Bez" = TRUE_doctor, "Pharmacy_Bez" = TRUE_pharmacy, "Dentist_Bez" = TRUE_dentist, "Optician_Bez" = TRUE_optician)

#data cleaning and FAI preparation (for weightening + per capita)
all_results_final[is.na(all_results_final)] <- 0   # to replace NAs with 0 for FAI calculation

#FAI calculation with all values
FAI_healthcare <- merge(all_results_final, data_pop, by="BEZ") |>
  distinct(BEZ, .keep_all = TRUE) |>

  mutate( FAI_Bez = Hospital_Bez + Pharmacy_Bez  + Doctor_Bez + Dentist_Bez + Optician_Bez,     # no weighting of different amenities. hospitals excluded
          FAI_Iso = (Hospital_Iso + Pharmacy_Iso + Doctor_Iso + Dentist_Iso + Optician_Iso) * 1, # no weighting of neighboring area
          FAI_total = (FAI_Bez + FAI_Iso) / POP_TOTAL,     
          FAI_mean = mean(FAI_total, na.rm = T),   # to get the FAI standardized: mean, sd and z variation
          FAI_sd = sd(FAI_total, na.rm = T), 
          FAI_final = (FAI_total - FAI_mean) / FAI_sd) |> 
  select(BEZ, FAI_final, geometry)  |>
  st_as_sf()

#View(FAI_healthcare)
# data polishing 
FAI_healthcare[is.na(FAI_healthcare)] <- 0   # to replace NaNs with 0 for plotting
quintile <- quantile(FAI_healthcare$FAI_final, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
FAI_healthcare$quintile <- cut(FAI_healthcare$FAI_final, breaks = quintile, labels = FALSE, include.lowest = TRUE)

#View(FAI_healthcare)
# write to disk
output_path = here("FAI/Essentials/Healthcare/FAI_healthcare.geojson")
# delete old version
unlink(output_path)
write_sf(FAI_healthcare, output_path)

