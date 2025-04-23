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



# data --------------------------------------------------------------------
nature_points = read_sf("Data/Nature/nature_points.geojson")  
data_zbez = read_sf("Data/Census Districts/ZAEHLBEZIRKOGDPolygon.shp")
data_bez = read_sf("Data/Districts/data_bez.geojson")
data_iso  = read_sf("Data/Isochrones/Isochrones15min.geojson")
data_pop = read_sf("Data/Population/data_pop.geojson")
data_ring = read_sf("Aesthetics/data_ring.geojson")
data_gürtel = read_sf("Aesthetics/data_gürtel.geojson")
data_borders = read_sf("Aesthetics/data_borders.geojson")
data_donau = read_sf("Aesthetics/data_donau.geojson")
data_donaukanal = read_sf("Aesthetics/data_donaukanal.geojson")
data_lake = read_sf("Aesthetics/data_lake.geojson")
# FAI_nature = read_sf("FAI/Nature/FAI_nature.geojson")  # shortcut, no new calculation


# counts nature per zbez ----------------------------------------------------------

result = map(1:250, function(i){  # für jeden zbez
  
  cat(glue(i, "/", 250, "\r"))
  
  iso = data_iso[i, ]
  
  zbez = data_zbez[i, ]
  
  
  # find points in zbez, similar to: st_intersection(data_points, zbez)
   nature_in_zbez = nature_points[zbez, ] %>%
    mutate( inZbez = TRUE)
  
  # find points in isochrone
  nature_in_isochrone = nature_points[iso, ] %>%
    mutate( inZbez = FALSE)
  
  #  in isochrone, aber nicht in zbez
  nature_in_iso_but_no_zbez = anti_join(nature_in_isochrone, nature_in_zbez %>% st_drop_geometry(), join_by(name))
  
  # in zbez + in isochrone (aber nicht in zbez)
  nature_iso_zbez = bind_rows(nature_in_zbez, nature_in_iso_but_no_zbez)
  
  
  # um welchen zbez handelt es sich
  nature_iso_zbez[["zbez"]] = zbez[["ZBEZ"]]
  # mapview(care_iso_zbez, zcol="inZbez")
  
  # weights
  nature_iso_zbez |>
    st_drop_geometry() |>
    group_by(zbez, inZbez, type) |>
    summarise(n_amenities = n()) |>
    pivot_wider(        # make long to wide
      names_from = c("inZbez", "type"),
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
  select( "ZBEZ" = zbez, "Park_Iso" = FALSE_park, "Garden_Iso" = FALSE_garden, "Green_Space_Iso" = "FALSE_green space", "Forest_Iso" = FALSE_forest, 
          "Park_Zbez" = TRUE_park, "Garden_Zbez" = TRUE_garden, "Green_Space_Zbez" = "TRUE_green space", "Forest_Zbez" = TRUE_forest)

all_results_final[is.na(all_results_final)] <- 0   # to replace NaNs with 0 for further calculations

#FAI calculation
FAI_nature <- merge(all_results_final, data_pop, by="ZBEZ") |> 
  mutate( FAI_Zbez = Park_Zbez + Garden_Zbez + Forest_Zbez + Green_Space_Zbez,     # no weighting of different amenity types
          FAI_Iso = Park_Iso + Garden_Iso + Forest_Iso + Green_Space_Iso,     # no weighting of different neighborhoods
          FAI_total = (FAI_Zbez + FAI_Iso) / POP_TOTAL)

FAI_nature$FAI_total[is.infinite(FAI_nature$FAI_total)] <- 0 

FAI_nature <- FAI_nature |>   # if not working, add again "_final"
  mutate(FAI_mean = mean(FAI_total, na.rm = T),   # to get the FAI standardized: mean, sd and z variation
         FAI_sd = sd(FAI_total, na.rm = T), 
         FAI_final = (FAI_total - FAI_mean) / FAI_sd) |> 
  select(ZBEZ, FAI_final, geometry)  |>
  st_as_sf()

quintile <- quantile(FAI_nature$FAI_final, probs = seq(0, 1, by = 0.2), na.rm = TRUE)

FAI_nature$quintile <- cut(FAI_nature$FAI_final, breaks = quintile, labels = FALSE, include.lowest = TRUE)

# write to disk
output_path = here("FAI/Nature/FAI_nature.geojson")
# delete old version
unlink(output_path)
write_sf(FAI_nature, output_path)


# FIGURE FOR ARTICLE

# filtered FAI
percentile_5 <- quantile(data_pop$POP_TOTAL, probs = 0.05)
data_pop_filtered <- data_pop |>
  filter(POP_TOTAL > percentile_5)

# new FAI calculation
FAI_nature_filtered <- merge(all_results_final, data_pop_filtered, by="ZBEZ") |> 
  mutate( FAI_Zbez = Park_Zbez + Garden_Zbez + Forest_Zbez + Green_Space_Zbez,     # no weighting of different amenity types
          FAI_Iso = Park_Iso + Garden_Iso + Forest_Iso + Green_Space_Iso,     # no weighting of different neighborhoods
          FAI_total = (FAI_Zbez + FAI_Iso) / POP_TOTAL)

FAI_nature_filtered$FAI_total[is.infinite(FAI_nature_filtered$FAI_total)] <- 0 

FAI_nature_filtered <- FAI_nature_filtered |>   # if not working, add again "_final"
  mutate(FAI_mean = mean(FAI_total, na.rm = T),   # to get the FAI standardized: mean, sd and z variation
         FAI_sd = sd(FAI_total, na.rm = T), 
         FAI_final = (FAI_total - FAI_mean) / FAI_sd) |> 
  select(ZBEZ, FAI_final, geometry)  |>
  st_as_sf()

quintile <- quantile(FAI_nature_filtered$FAI_final, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
# quintile <- quintile + seq(0, 0.001, length.out = length(quintile))
FAI_nature_filtered$quintile <- cut(FAI_nature_filtered$FAI_final, breaks = quintile, labels = FALSE, include.lowest = TRUE)

# write to disk
output_path = here("FAI/Overview/FAI_nature_filtered.geojson")
# delete old version
unlink(output_path)
write_sf(FAI_nature_filtered, output_path)

#visualization
data_iso = read_sf("Data/Schindler/isochrone20_data_center.geojson")  # 20 min 
data_cir = read_sf("Data/Schindler/circle_1km_data_center.geojson")  # 1 km

ggplot() +
  geom_sf(data = FAI_nature_filtered, aes(fill= quintile),
          color = "grey20") +
  geom_sf(data = nature_points, 
          aes(shape = type), 
          size = 0.6, 
          show.legend = "point") +
  theme_map() +
  scale_fill_continuous(type = "viridis",
                        direction = -1) +
  ggtitle("Nature") +
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



# save image
ggsave(file = "FAI/Overview/FAI_nature_filtered.png", plot = last_plot(), width = 10, height = 8, units = "in")







# additional figures ----------------------------------------------------------

ggplot() +
  geom_sf(data = FAI_nature, aes(fill= quintile),
          color = "grey20", 
          size = 0.4) +
  geom_sf(data = nature_points, 
          aes(shape = type), 
          size = 0.7, 
          show.legend = "point") +
  theme_map() +
  scale_fill_continuous(type = "viridis",
                        direction = -1) +
  ggtitle("Nature") + 
  theme(legend.position = "left", plot.title = element_text(hjust = 0.3, vjust = -50)) +
  geom_text(data = data_zbez, aes(x = st_coordinates(st_centroid(data_zbez))[, "X"], y = st_coordinates(st_centroid(data_zbez))[, "Y"], label = ZBEZ), size = 2) 

# save image
ggsave(file = "FAI/Nature/FAI_nature.png", plot = last_plot(), width = 10, height = 8, units = "in")



# displaying results on aggregated district level for better data display 
# how to see FAI per district?

# shortcut of FAI since no weighting done

nature_points_agg <- data_bez |> 
  mutate(count = lengths(st_intersects(data_bez, nature_points)))

data_pop_agg <- data_pop |>
  st_drop_geometry() |>
  group_by(BEZ) |>
  aggregate(POP_TOTAL ~ BEZ, sum) 

FAI_nature_agg <- merge(nature_points_agg, data_pop_agg, by = "BEZ") |> 
  mutate( FAI_total = count/POP_TOTAL,
          FAI_mean = mean(FAI_total, na.rm = T),   # to get the FAI standardized: mean, sd and z variation
          FAI_sd = sd(FAI_total, na.rm = T), 
          FAI_final = (FAI_total - FAI_mean) / FAI_sd) |>
  st_as_sf()

quintile <- quantile(FAI_nature_agg$FAI_final, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
FAI_nature_agg$quintile <- cut(FAI_nature_agg$FAI_final, breaks = quintile, labels = FALSE, include.lowest = TRUE)

# plotting
ggplot()  +
  geom_sf(data = FAI_nature_agg, aes(fill= quintile),
          color = "grey20", 
          size = 0.4) +
  geom_sf(data = nature_points, 
          aes(shape = type), 
          size = 0.4)  + 
  theme_map() +
  scale_fill_continuous(type = "viridis",
                        direction = -1) +
  scale_shape_discrete() + 
  theme(legend.position = "left", plot.title = element_text(hjust = 0.3, vjust = -50)) +
  geom_text(data = data_bez, aes(x = st_coordinates(st_centroid(data_bez))[, "X"], y = st_coordinates(st_centroid(data_bez))[, "Y"], label = BEZ), size = 2) 

# save image
ggsave(file = "FAI/Nature/FAI_nature_agg.png", plot = last_plot(), width = 10, height = 8, units = "in")






