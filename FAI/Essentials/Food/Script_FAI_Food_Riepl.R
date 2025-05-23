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

view(food_points)

# data --------------------------------------------------------------------
food_points = read_sf("Data/Food/food_points.geojson")  
data_zbez = read_sf("Data/Census Districts/ZAEHLBEZIRKOGDPolygon.shp")
data_bez = read_sf("Data/Districts/data_bez.geojson")
data_iso = read_sf(here("Data/Isochrones/Isochrones15min.geojson")) 
data_pop = read_sf("Data/Population/data_pop.geojson")
data_ring = read_sf("Aesthetics/data_ring.geojson")
data_gürtel = read_sf("Aesthetics/data_gürtel.geojson")
data_borders = read_sf("Aesthetics/data_borders.geojson")
data_donau = read_sf("Aesthetics/data_donau.geojson")
data_donaukanal = read_sf("Aesthetics/data_donaukanal.geojson")
data_lake = read_sf("Aesthetics/data_lake.geojson")
# FAI_food = read_sf("FAI/Food/FAI_food.geojson")  # shortcut, no new calculation

# food data of neighborhoods, saved on disc ----------------------------------------------------------

result = map(1:250, function(i){  # für jeden zbez
  
  cat(glue(i, "/", 250, "\r"))
  
  iso = data_iso[i, ]
  
  zbez = data_zbez[i, ]
  
  # find points in zbez, similar to: st_intersection(data_points, zbez)
  food_in_zbez = food_points[zbez, ] %>%
    mutate(inZbez = TRUE)
  
  # find points in isochrone
  food_in_isochrone = food_points[iso, ] %>%
    mutate(inZbez = FALSE)
  
  #  in isochrone, aber nicht in zbez
  food_in_iso_but_no_zbez = anti_join(food_in_isochrone, food_in_zbez %>% st_drop_geometry(), join_by(name))
  
  # in zbez + in isochrone (aber nicht in zbez)
  food_iso_zbez = bind_rows(food_in_zbez, food_in_iso_but_no_zbez)
  
  
  # um welchen zbez handelt es sich
  food_iso_zbez[["zbez"]] = zbez[["ZBEZ"]]
  # mapview(care_iso_zbez, zcol="inZbez")
  
  # weights
  food_iso_zbez |>
    st_drop_geometry() |>
    group_by(zbez, inZbez, type) |>
    summarise(
      n_amenities = n()
    ) |>
    pivot_wider(        # make long to wide
      names_from = c("inZbez", "type"),
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
  select( "ZBEZ" = zbez, "Bakery_Iso" = FALSE_bakery, "Cafe_Iso" = FALSE_cafe, "Bar_Iso" = FALSE_bar, "Supermarket_Iso" = "FALSE_supermarket", "Restaurant_Iso" = "FALSE_restaurant", "Kiosk_Iso" = "FALSE_kiosk", 
          "Bakery_Zbez" = TRUE_bakery, "Cafe_Zbez" = TRUE_cafe,  "Bar_Zbez" = TRUE_bar, "Supermarket_Zbez" = "TRUE_supermarket", "Restaurant_Zbez" = "TRUE_restaurant", "Kiosk_Zbez" = "TRUE_kiosk") 

#data cleaning and FAI preparation (for weightening + per capita)
all_results_final[is.na(all_results_final)] <- 0   # to replace NAs with 0 for FAI calculation

#FAI calculation # update needed but what to do with training facilities in isochrones??
FAI_food <- merge(all_results_final, data_pop, by="ZBEZ") |> 
  mutate( FAI_Zbez = Bakery_Zbez + Cafe_Zbez  + Bar_Zbez + Supermarket_Zbez + Restaurant_Zbez + Kiosk_Zbez,     # no weighting of different amenities
          FAI_Iso = Bakery_Iso + Cafe_Iso   + Bar_Iso + Supermarket_Iso + Restaurant_Iso + Kiosk_Iso, # no weighting of neighboring area
          FAI_total = (FAI_Zbez + FAI_Iso) / POP_TOTAL)
FAI_food$FAI_total[is.infinite(FAI_food$FAI_total)] <- 0      # because one zbez population = 0  
          
FAI_food = FAI_food |>
  mutate( FAI_mean = mean(FAI_total, na.rm = T),   # to get the FAI standardized: mean, sd and z variation
          FAI_sd = sd(FAI_total, na.rm = T), 
          FAI_final = (FAI_total - FAI_mean) / FAI_sd) |> 
  select(ZBEZ, FAI_final, geometry)  |>
  st_as_sf()

FAI_food[is.na(FAI_food)] <- 0   # to replace NaNs with 0 for plotting

quintile <- quantile(FAI_food$FAI_final, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
# quintile <- quintile + seq(0, 0.001, length.out = length(quintile))
FAI_food$quintile <- cut(FAI_food$FAI_final, breaks = quintile, labels = FALSE, include.lowest = TRUE)

# write to disk
output_path = here("FAI/Food/FAI_food.geojson")
# delete old version
unlink(output_path)
write_sf(FAI_food, output_path)



# FIGURE FOR ARTICLE

# filtered FAI
percentile_5 <- quantile(data_pop$POP_TOTAL, probs = 0.05)
data_pop_filtered <- data_pop |>
  filter(POP_TOTAL > percentile_5)

# new FAI calculation
FAI_food_filtered <- merge(all_results_final, data_pop_filtered, by="ZBEZ") |> 
  mutate( FAI_Zbez = Bakery_Zbez + Cafe_Zbez  + Bar_Zbez + Supermarket_Zbez + Restaurant_Zbez + Kiosk_Zbez,     # no weighting of different amenities
          FAI_Iso = Bakery_Iso + Cafe_Iso   + Bar_Iso + Supermarket_Iso + Restaurant_Iso + Kiosk_Iso, # no weighting of neighboring area
          FAI_total = (FAI_Zbez + FAI_Iso) / POP_TOTAL,   # FAI per capita 
          FAI_mean = mean(FAI_total, na.rm = T),   # to get the FAI standardized: mean, sd and z variation
          FAI_sd = sd(FAI_total, na.rm = T), 
          FAI_final = (FAI_total - FAI_mean) / FAI_sd) |> 
  select(ZBEZ, FAI_final, geometry)  |>
  st_as_sf()

quintile <- quantile(FAI_food_filtered$FAI_final, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
# quintile <- quintile + seq(0, 0.001, length.out = length(quintile))
FAI_food_filtered$quintile <- cut(FAI_food_filtered$FAI_final, breaks = quintile, labels = FALSE, include.lowest = TRUE)

# write to disk
output_path = here("FAI/Food/FAI_food_filtered.geojson")
# delete old version
unlink(output_path)
write_sf(FAI_food_filtered, output_path)

#visualization
data_iso = read_sf("Data/Schindler/isochrone20_data_center.geojson")  # 20 min 
data_cir = read_sf("Data/Schindler/circle_1km_data_center.geojson")  # 1 km

ggplot() +
  geom_sf(data = data_zbez) +
  geom_sf(data = FAI_food_filtered, aes(fill= quintile),
          color = "grey20") +
  geom_sf(data = food_points, 
             aes(shape = type), 
             size = 0.6, 
             show.legend = "point") +
  theme_map() +
  scale_fill_continuous(type = "viridis",
                        direction = -1) +
  ggtitle("Food") +
  scale_shape_discrete()  +
  geom_sf(data = data_ring, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_gürtel, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_donau, fill = "transparent", color = "lightblue", lwd = 2.5) +
  geom_sf(data = data_lake, fill = "transparent", color = "lightblue", lwd = 1.7) +
  geom_sf(data = data_donaukanal, fill = "transparent", color = "lightblue", lwd = 3.5) +
  geom_sf(data = data_borders, fill = "transparent", color = "black", lwd = 1.5) +
  theme(legend.position = "left", plot.title = element_text(size = 32, hjust = 0.15, vjust = -10, face = "bold", family = "serif"), 
        legend.text = element_text(size = 20, family = "serif"), legend.title = element_text(size = 24, face = "bold", family = "serif")) +    
  geom_text(data = data_bez, aes(x = st_coordinates(st_centroid(data_bez))[, "X"], y = st_coordinates(st_centroid(data_bez))[, "Y"], label = BEZ), size = 5, family = "serif", fontface = "bold", color = "red")  +
  #  geom_sf(data = data_iso, fill = "transparent", color ="pink", lwd = 1) +
  geom_sf(data = data_cir, fill = "transparent", color = "pink", lwd = 2) +
  geom_sf(data = data_center, fill = "transparent", color = "pink", lwd = 35, size = 3) 

View(food_points)

# save image
ggsave(file = "FAI/Overview/FAI_food_filtered.png", plot = last_plot(), width = 10, height = 8, units = "in")




# additional figures ----------------------------------------------------------

# easier to remove geometry column and merge again with census district dataset


ggplot() +
  geom_sf(data = FAI_food, aes(fill= quintile),
          color = "grey20") +
  geom_sf(data = food_points, 
          aes(shape = type), 
          size = 0.4) +
  theme_map() +
  scale_fill_continuous(type = "viridis",
                        direction = -1) +
  ggtitle("Access to food in Vienna") +
  scale_shape_discrete() +  
  theme(legend.position = "left", plot.title = element_text(hjust = 0.2, vjust = -30)) +
  geom_text(data = FAI_care, aes(x = st_coordinates(st_centroid(FAI_care))[, "X"], y = st_coordinates(st_centroid(FAI_care))[, "Y"], label = ZBEZ), size = 2) 



# displaying results on aggregated district level for better data display 
# how to see FAI per district?

# shortcut of FAI since no weighting done and isochrone counts irrelvant for bez analysis

food_points_agg <- data_bez |> 
  mutate(count = lengths(st_intersects(data_bez, food_points)))

data_pop_agg <- data_pop |>
  st_drop_geometry() |>
  group_by(BEZ) |>
  aggregate(POP_TOTAL ~ BEZ, sum) 

FAI_food_agg <- merge(food_points_agg, data_pop_agg, by = "BEZ") |> 
  mutate( FAI_total = count/POP_TOTAL,
          FAI_mean = mean(FAI_total, na.rm = T),   # to get the FAI standardized: mean, sd and z variation
          FAI_sd = sd(FAI_total, na.rm = T), 
          FAI_final = (FAI_total - FAI_mean) / FAI_sd) |>
  st_as_sf()

quintile <- quantile(FAI_food_agg$FAI_final, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
FAI_food_agg$quintile <- cut(FAI_food_agg$FAI_final, breaks = quintile, labels = FALSE, include.lowest = TRUE)

# write to disk
output_path = here("FAI/food/FAI_food_agg.geojson")
# delete old version
unlink(output_path)
write_sf(FAI_food_agg, output_path)

# plotting
ggplot()  +
  geom_sf(data = FAI_food_agg, aes(fill= quintile),
          color = "grey20", 
          size = 0.4) +
  geom_sf(data = food_points, 
          aes(shape = type), 
          size = 0.4)  + 
  theme_map() +
  scale_fill_continuous(type = "viridis",
                        direction = -1) + 
  #ggtitle("Access to food in Vienna") +
  scale_shape_discrete() + 
  theme(legend.position = "left") +   #, plot.title = element_text(hjust = 0.2, vjust = -35)
  geom_text(data = data_bez, aes(x = st_coordinates(st_centroid(data_bez))[, "X"], y = st_coordinates(st_centroid(data_bez))[, "Y"], label = BEZ), size = 2) 

# save image
ggsave(file = "FAI/Overview/FAI_food_agg.png", plot = last_plot(), width = 10, height = 8, units = "in")





