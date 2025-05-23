## R Code: mapping the Foundational Infrastructure in Vienna

library(sf) 
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

#view(food_points)

# data --------------------------------------------------------------------
food_points = read_sf("FAI/Data/Food/food_points.geojson")  
data_zbez = read_sf("City data/Census Districts/ZAEHLBEZIRKOGDPolygon.shp")
data_bez = read_sf("City data/Districts/data_bez.geojson")
data_iso = read_sf(here("City data/Isochrones/Isochrones15min.geojson")) 

data_pop = read_sf("City data/Population/data_pop.geojson") |>
  group_by(BEZ) |>
  summarize(POP_TOTAL = sum(POP_TOTAL, na.rm = TRUE))

data_ring = read_sf("City data/Aesthetics/data_ring.geojson")
data_gürtel = read_sf("City data/Aesthetics/data_gürtel.geojson")
data_borders = read_sf("City data/Aesthetics/data_borders.geojson")
data_donau = read_sf("City data/Aesthetics/data_donau.geojson")
data_donaukanal = read_sf("City data/Aesthetics/data_donaukanal.geojson")
data_lake = read_sf("City data/Aesthetics/data_lake.geojson")
# FAI_food = read_sf("FAI/Food/FAI_food.geojson")  # shortcut, no new calculation

# food data of neighborhoods, saved on disc ----------------------------------------------------------

data_food = merge(data_zbez, food_points |> st_drop_geometry(), by = "ZBEZ") |>
  select(BEZ, name, type, geometry)

#View(data_food)

result = map(1:23, function(i){  # für jeden zbez
  
  cat(glue(i, "/", 23, "\r"))
  
  iso = data_iso[i, ]
  
  bez = data_bez[i, ]
  
  # find points in zbez, similar to: st_intersection(data_points, zbez)
  food_in_bez = data_food[bez, ] %>%
    mutate(inBez = TRUE)
  
  # find points in isochrone
  food_in_isochrone = data_food[iso, ] %>%
    mutate(inBez = FALSE)
  
  #  in isochrone, aber nicht in zbez
  food_in_iso_but_no_bez = anti_join(food_in_isochrone, food_in_bez %>% st_drop_geometry(), join_by(name))
  
  # in bez + in isochrone (aber nicht in bez)
  food_iso_bez = bind_rows(food_in_bez, food_in_iso_but_no_bez)
  
  
  # um welchen zbez handelt es sich
  food_iso_bez[["bez"]] = bez[["BEZ"]]

  # weights
  food_iso_bez |>
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

#View(all_results)

# append the ZBEZ number of the missing ones
# the ones filtered out above, will have 0s in all columns
all_results_final = right_join(all_results, data_bez, join_by(bez == BEZ)) |>
  select( "BEZ" = bez, "Bakery_Iso" = FALSE_bakery, "Cafe_Iso" = FALSE_cafe, "Bar_Iso" = FALSE_bar, "Supermarket_Iso" = "FALSE_supermarket", "Restaurant_Iso" = "FALSE_restaurant", "Kiosk_Iso" = "FALSE_kiosk", 
          "Bakery_Bez" = TRUE_bakery, "Cafe_Bez" = TRUE_cafe,  "Bar_Bez" = TRUE_bar, "Supermarket_Bez" = "TRUE_supermarket", "Restaurant_Bez" = "TRUE_restaurant", "Kiosk_Bez" = "TRUE_kiosk") 

#data cleaning and FAI preparation (for weightening + per capita)
all_results_final[is.na(all_results_final)] <- 0   # to replace NAs with 0 for FAI calculation

#FAI calculation # update needed but what to do with training facilities in isochrones??
FAI_food <- merge(all_results_final, data_pop, by="BEZ") |> 
  distinct(BEZ, .keep_all = TRUE) |>
  
  mutate( FAI_Bez = Bakery_Bez + Cafe_Bez  + Bar_Bez + Supermarket_Bez + Restaurant_Bez + Kiosk_Bez,     # no weighting of different amenities
          FAI_Iso = Bakery_Iso + Cafe_Iso   + Bar_Iso + Supermarket_Iso + Restaurant_Iso + Kiosk_Iso, # no weighting of neighboring area
          FAI_total = (FAI_Bez + FAI_Iso) / POP_TOTAL)
FAI_food$FAI_total[is.infinite(FAI_food$FAI_total)] <- 0      # because one Bez population = 0  
          
FAI_food = FAI_food |>
  mutate( FAI_mean = mean(FAI_total, na.rm = T),   # to get the FAI standardized: mean, sd and z variation
          FAI_sd = sd(FAI_total, na.rm = T), 
          FAI_final = (FAI_total - FAI_mean) / FAI_sd) |> 
  select(BEZ, FAI_final, geometry)  |>
  st_as_sf()

FAI_food[is.na(FAI_food)] <- 0   # to replace NaNs with 0 for plotting

quintile <- quantile(FAI_food$FAI_final, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
# quintile <- quintile + seq(0, 0.001, length.out = length(quintile))
FAI_food$quintile <- cut(FAI_food$FAI_final, breaks = quintile, labels = FALSE, include.lowest = TRUE)

#View(FAI_food)
# write to disk
output_path = here("FAI/Essentials/Food/FAI_food.geojson")
# delete old version
unlink(output_path)
write_sf(FAI_food, output_path)


