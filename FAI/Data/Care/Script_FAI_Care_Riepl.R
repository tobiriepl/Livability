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


View(care_points)
# data --------------------------------------------------------------------
care_points = read_sf("Data/Care/care_points.geojson")  
data_zbez = read_sf("Data/Census Districts/ZAEHLBEZIRKOGDPolygon.shp")
data_bez = read_sf("Data/Districts/data_bez.geojson")
data_iso = read_sf("Data/Isochrones/Isochrones15min.geojson")
data_pop = read_sf("Data/Population/data_pop.geojson")
data_ring = read_sf("Aesthetics/data_ring.geojson")
data_gürtel = read_sf("Aesthetics/data_gürtel.geojson")
data_borders = read_sf("Aesthetics/data_borders.geojson")
data_donau = read_sf("Aesthetics/data_donau.geojson")
data_donaukanal = read_sf("Aesthetics/data_donaukanal.geojson")
data_lake = read_sf("Aesthetics/data_lake.geojson")
# FAI_care = read_sf("FAI/Care/FAI_care.geojson")  # shortcut, no new calculation

View(care_points)

# care data of neighborhoods, saved on disc ----------------------------------------------------------

result = map(1:250, function(i){  # für jeden zbez
  
  cat(glue(i, "/", 250, "\r"))
  
  iso = data_iso[i, ]
  zbez = data_zbez[i, ]
  
  
  care_in_zbez = care_points[zbez, ] |>
    mutate(inZbez = TRUE) 
  
  # if there are no points in the zbez
  points_in_zbez = TRUE
  if(nrow(care_in_zbez) == 0){
    points_in_zbez = FALSE
  }
  
  care_in_isochrone = care_points[iso, ] |>
    mutate(inZbez = FALSE)
  
  # if there are no points in the isochrone
  points_in_iso = FALSE
  if(nrow(care_in_isochrone) == 0){
    points_in_iso = FALSE
  }
  
  care_in_iso_but_no_zbez = anti_join(care_in_isochrone, care_in_zbez |> st_drop_geometry(), join_by(name)) 
  care_iso_zbez = bind_rows(care_in_zbez, care_in_iso_but_no_zbez)
  
  # if there are no points, neither in the zbez nor in the iso return NA
  if(nrow(care_iso_zbez) == 0){
    return(NA)
  }
  
  care_iso_zbez[["zbez"]] = zbez[["ZBEZ"]]   
  care_iso_zbez = st_join(care_iso_zbez, zbez, join = st_within)
  #healthcare_iso_zbez =  st_join(healthcare_iso_zbez, zbez, left = TRUE)
  
  care_iso_zbez |>
    st_drop_geometry() |>
    group_by(zbez, inZbez, type) |>
    summarise(n_amenities = n()) |>
    pivot_wider(names_from = c("inZbez", "type"),
                values_from = "n_amenities") -> final
  
  return(final)
})

# filter out the NAs
result = result[!is.na(result)]

all_results = result |> bind_rows() |>
  arrange(zbez) 
View(all_results)

# append the ZBEZ number of the missing ones
# the ones filtered out above, will have 0s in all columns
all_results_final = right_join(all_results, data_zbez, join_by(zbez == ZBEZ)) |>
  select( "ZBEZ" = zbez, "Social_Facility_Iso" = "FALSE_social facility",  "Nursing_Home_Iso" = "FALSE_nursing home", "Kindergarten_Iso" = FALSE_kindergarten, 
          "Social_Facility_Zbez" = "TRUE_social facility", "Nursing_Home_Zbez" = "TRUE_nursing home","Kindergarten_Zbez" = TRUE_kindergarten)

View(all_results_final)
#data cleaning and FAI preparation (for weightening + per capita)
all_results_final[is.na(all_results_final)] <- 0   # to replace NAs with 0 for FAI calculation

#FAI calculation
FAI_care <- merge(all_results_final, data_pop, by="ZBEZ") |> 
  mutate( FAI_Zbez = Nursing_Home_Zbez + Kindergarten_Zbez + Social_Facility_Zbez,     # no weighting 
          FAI_Iso = ( Nursing_Home_Iso + Kindergarten_Iso + Social_Facility_Iso) * 1, # weighted per amenity and neighboring area
          FAI_total = (FAI_Zbez + FAI_Iso) / POP_TOTAL) 
FAI_care$FAI_total[is.infinite(FAI_care$FAI_total)] <- 0  

FAI_care <- FAI_care |>
  mutate( FAI_mean = mean(FAI_total, na.rm = T),   # to get the FAI standardized: mean, sd and z variation
          FAI_sd = sd(FAI_total, na.rm = T), 
          FAI_final = (FAI_total - FAI_mean) / FAI_sd) |> 
  select(ZBEZ, FAI_final, geometry)  |>
  st_as_sf()

FAI_care[is.na(FAI_care)] <- 0   # to replace NaNs with 0 for plotting

quintile <- quantile(FAI_care$FAI_final, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
FAI_care$quintile <- cut(FAI_care$FAI_final, breaks = quintile, labels = FALSE, include.lowest = TRUE)

# write to disk
output_path = here("FAI/Care/FAI_care.geojson")
# delete old version
unlink(output_path)
write_sf(FAI_care, output_path)


# FIGURE FOR THE ARTICLE

# filtered FAI
percentile_5 <- quantile(data_pop$POP_TOTAL, probs = 0.05)
data_pop_filtered <- data_pop |>
  filter(POP_TOTAL > percentile_5)

# new FAI calculation
FAI_care_filtered <- merge(all_results_final, data_pop_filtered, by="ZBEZ") |> 
  mutate( FAI_Zbez = Nursing_Home_Zbez + Kindergarten_Zbez + Social_Facility_Zbez,     # weighted per amenity: how ?!
          FAI_Iso = ( Nursing_Home_Iso + Kindergarten_Iso + Social_Facility_Iso) * 1, # weighted per amenity and neighboring area
          FAI_total = (FAI_Zbez + FAI_Iso) / POP_TOTAL,    # how to integrate population?? 
          FAI_mean = mean(FAI_total, na.rm = T),   # to get the FAI standardized: mean, sd and z variation
          FAI_sd = sd(FAI_total, na.rm = T), 
          FAI_final = (FAI_total - FAI_mean) / FAI_sd) |> 
  select(ZBEZ, FAI_final, geometry)  |>
  st_as_sf()

quintile <- quantile(FAI_care_filtered$FAI_final, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
# quintile <- quintile + seq(0, 0.001, length.out = length(quintile))
FAI_care_filtered$quintile <- cut(FAI_care_filtered$FAI_final, breaks = quintile, labels = FALSE, include.lowest = TRUE)

# write to disk
output_path = here("FAI/Care/FAI_care_filtered.geojson")
# delete old version
unlink(output_path)
write_sf(FAI_care_filtered, output_path)

#visualization: FIGURE FOR ARTICLE
data_iso = read_sf("Data/Schindler/isochrone20_data_center.geojson")  # 20 min 
data_cir = read_sf("Data/Schindler/circle_1km_data_center.geojson")  # 1 km


ggplot() +
  geom_sf(data = data_zbez) +
  geom_sf(data = FAI_care_filtered, aes(fill= quintile),
          color = "grey20") +
  geom_sf(data = care_points, 
          aes(shape = type), 
          size = 0.4, 
          show.legend = "point") +
  theme_map() +
  scale_fill_continuous(type = "viridis",
                        direction = -1) +
  ggtitle("Social care") +
  scale_shape_discrete()  +
  geom_sf(data = data_ring, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_gürtel, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_donau, fill = "transparent", color = "lightblue", lwd = 2.5) +
  geom_sf(data = data_lake, fill = "transparent", color = "lightblue", lwd = 1.7) +
  geom_sf(data = data_donaukanal, fill = "transparent", color = "lightblue", lwd = 3.5) +
  geom_sf(data = data_borders, fill = "transparent", color = "black", lwd = 1.5) +
  theme(legend.position = "left", plot.title = element_text(size = 32, hjust = 0.15, vjust = -10, face = "bold", family = "serif"), 
        legend.text = element_text(size = 20, family = "serif"), legend.title = element_text(size = 24, face = "bold", family = "serif")) +    
  geom_text(data = data_bez, aes(x = st_coordinates(st_centroid(data_bez))[, "X"], y = st_coordinates(st_centroid(data_bez))[, "Y"], label = BEZ), size = 5, family = "serif", fontface = "bold", color = "red") +
  #  geom_sf(data = data_iso, fill = "transparent", color ="pink", lwd = 1) +
  geom_sf(data = data_cir, fill = "transparent", color = "pink", lwd = 2) +
  geom_sf(data = data_center, fill = "transparent", color = "pink", lwd = 35, size = 3) 


# save image
ggsave(file = "FAI/Overview/FAI_care_filtered.png", plot = last_plot(), width = 10, height = 8, units = "in")






# additional figures  ----------------------------------------------------------
# easier to remove geometry column and merge again with census district dataset
ggplot() +
  geom_sf(data = FAI_care, aes(fill= quintile),
          color = "grey20") +
  geom_sf(data = care_points, 
          aes(shape = type), 
          size = 0.4) +
  theme_map() +
  scale_fill_continuous(type = "viridis",
                        direction = -1) +
 # ggtitle("Access to care in Vienna") +
  scale_shape_discrete() +  
  theme(legend.position = "left", plot.title = element_text(hjust = 0.2, vjust = -30)) +
  geom_text(data = FAI_care, aes(x = st_coordinates(st_centroid(FAI_care))[, "X"], y = st_coordinates(st_centroid(FAI_care))[, "Y"], label = ZBEZ), size = 2) 



# displaying results on aggregated district level for better data display 
# how to see FAI per district?
# shortcut of FAI since no weighting done and because isochrones irrelevant in case of bez level

care_points_agg <- data_bez |> 
  mutate(count = lengths(st_intersects(data_bez, care_points)))

data_pop_agg <- data_pop |>
  st_drop_geometry() |>
  group_by(BEZ) |>
  aggregate(POP_TOTAL ~ BEZ, sum) 

FAI_care_agg <- merge(care_points_agg, data_pop_agg, by = "BEZ") |> 
  mutate( FAI_total = count/POP_TOTAL,
          FAI_mean = mean(FAI_total, na.rm = T),   # to get the FAI standardized: mean, sd and z variation
          FAI_sd = sd(FAI_total, na.rm = T), 
          FAI_final = (FAI_total - FAI_mean) / FAI_sd) |>
  st_as_sf()

quintile <- quantile(FAI_care_agg$FAI_final, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
quintile <- quintile + seq(0, 0.001, length.out = length(quintile))
FAI_care_agg$quintile <- cut(FAI_care_agg$FAI_final, breaks = quintile, labels = FALSE, include.lowest = TRUE)

# write to disk
output_path = here("FAI/Care/FAI_care_agg.geojson")
# delete old version
unlink(output_path)
write_sf(FAI_care_agg, output_path)

# plotting
ggplot()  +
  geom_sf(data = FAI_care_agg, aes(fill= quintile),
          color = "grey20") +
  geom_sf(data = care_points, 
          aes(shape = type), 
          size = 0.4)  + 
  theme_map() +
  scale_fill_continuous(type = "viridis",
                        direction = -1) + 
 # ggtitle("Access to care in Vienna") +
  scale_shape_discrete() + 
  theme(legend.position = "left") +  #, plot.title = element_text(hjust = 0.2, vjust = -35)
  geom_text(data = data_bez, aes(x = st_coordinates(st_centroid(data_bez))[, "X"], y = st_coordinates(st_centroid(data_bez))[, "Y"], label = BEZ), size = 2) 

# save image
ggsave(file = "FAI/Overview/FAI_care_agg.png", plot = last_plot(), width = 10, height = 8, units = "in")







