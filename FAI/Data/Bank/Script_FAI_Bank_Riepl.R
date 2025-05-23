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

view(bank_points)

# data --------------------------------------------------------------------
bank_points = read_sf("Data/Bank/bank_points.geojson")  
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
# FAI_bank = read_sf("FAI/Bank/FAI_bank.geojson")  # shortcut, no new calculation

# bank data of neighborhoods, saved on disc ----------------------------------------------------------

result = map(1:250, function(i){  # für jeden zbez
  
  cat(glue(i, "/", 250, "\r"))
  
  iso = data_iso[i, ]
  
  zbez = data_zbez[i, ]
  
  # find points in zbez, similar to: st_intersection(data_points, zbez)
  bank_in_zbez = bank_points[zbez, ] %>%
    mutate(inZbez = TRUE)
  
  # find points in isochrone
  bank_in_isochrone = bank_points[iso, ] %>%
    mutate(inZbez = FALSE)
  
  #  in isochrone, aber nicht in zbez
  bank_in_iso_but_no_zbez = anti_join(bank_in_isochrone, bank_in_zbez %>% st_drop_geometry(), join_by(name))
  
  # in zbez + in isochrone (aber nicht in zbez)
  bank_iso_zbez = bind_rows(bank_in_zbez, bank_in_iso_but_no_zbez)
  
  
  # um welchen zbez handelt es sich
  bank_iso_zbez[["zbez"]] = zbez[["ZBEZ"]]
  # mapview(care_iso_zbez, zcol="inZbez")
  
  # weights
  bank_iso_zbez |>
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
  select( "ZBEZ" = zbez, "Bank_Iso" = FALSE_bank, "Atm_Iso" = FALSE_atm, "Exchange_Office_Iso" = "FALSE_exchange office", 
          "Bank_Zbez" = TRUE_bank, "Atm_Zbez" = TRUE_atm,  "Exchange_Office_Zbez" = "TRUE_exchange office")
#data cleaning and FAI preparation (for weightening + per capita)
all_results_final[is.na(all_results_final)] <- 0   # to replace NAs with 0 for FAI calculation

#FAI calculation # update needed but what to do with training facilities in isochrones??
FAI_bank <- merge(all_results_final, data_pop, by="ZBEZ") |> 
  mutate( FAI_Zbez = Bank_Zbez + Atm_Zbez  + Exchange_Office_Zbez,     # no weighting of different amenities
          FAI_Iso = Bank_Iso + Atm_Iso   + Exchange_Office_Iso, # no weighting of neighboring area
          FAI_total = (FAI_Zbez + FAI_Iso) / POP_TOTAL)
FAI_bank$FAI_total[is.infinite(FAI_bank$FAI_total)] <- 0      # because one zbez population = 0  
          
FAI_bank = FAI_bank |>
  mutate( FAI_mean = mean(FAI_total, na.rm = T),   # to get the FAI standardized: mean, sd and z variation
          FAI_sd = sd(FAI_total, na.rm = T), 
          FAI_final = (FAI_total - FAI_mean) / FAI_sd) |> 
  select(ZBEZ, FAI_final, geometry)  |>
  st_as_sf()

FAI_bank[is.na(FAI_bank)] <- 0   # to replace NaNs with 0 for plotting

quintile <- quantile(FAI_bank$FAI_final, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
# quintile <- quintile + seq(0, 0.001, length.out = length(quintile))
FAI_bank$quintile <- cut(FAI_bank$FAI_final, breaks = quintile, labels = FALSE, include.lowest = TRUE)

# write to disk
output_path = here("FAI/Bank/FAI_bank.geojson")
# delete old version
unlink(output_path)
write_sf(FAI_bank, output_path)



# FIGURE FOR ARTICLE

# filtered FAI
percentile_5 <- quantile(data_pop$POP_TOTAL, probs = 0.05)
data_pop_filtered <- data_pop |>
  filter(POP_TOTAL > percentile_5)

# new FAI calculation
FAI_bank_filtered <- merge(all_results_final, data_pop_filtered, by="ZBEZ") |> 
  mutate( FAI_Zbez = Bank_Zbez + Atm_Zbez  + Exchange_Office_Zbez,     # no weighting of different amenities
          FAI_Iso = Bank_Iso + Atm_Iso   + Exchange_Office_Iso, # no weighting of neighboring area
          FAI_total = (FAI_Zbez + FAI_Iso) / POP_TOTAL,    # FAI per capita 
          FAI_mean = mean(FAI_total, na.rm = T),   # to get the FAI standardized: mean, sd and z variation
          FAI_sd = sd(FAI_total, na.rm = T), 
          FAI_final = (FAI_total - FAI_mean) / FAI_sd) |> 
  select(ZBEZ, FAI_final, geometry)  |>
  st_as_sf()

quintile <- quantile(FAI_bank_filtered$FAI_final, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
# quintile <- quintile + seq(0, 0.001, length.out = length(quintile))
FAI_bank_filtered$quintile <- cut(FAI_bank_filtered$FAI_final, breaks = quintile, labels = FALSE, include.lowest = TRUE)

# write to disk
output_path = here("FAI/Bank/FAI_bank_filtered.geojson")
# delete old version
unlink(output_path)
write_sf(FAI_bank_filtered, output_path)

#visualization
data_iso = read_sf("Data/Schindler/isochrone20_data_center.geojson")  # 20 min 
data_cir = read_sf("Data/Schindler/circle_1km_data_center.geojson")  # 1 km

ggplot() +
  geom_sf(data = data_zbez) +
  geom_sf(data = FAI_bank_filtered, aes(fill= quintile),
          color = "grey20") +
    geom_sf(data = bank_points, 
            aes(shape = type), 
            size = 0.6, 
            show.legend = "point") +
  theme_map() +
  scale_fill_continuous(type = "viridis",
                        direction = -1) +
  ggtitle("Bank") +
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
ggsave(file = "FAI/Overview/FAI_bank_filtered.png", plot = last_plot(), width = 10, height = 8, units = "in")



# additional figures ----------------------------------------------------------

# easier to remove geometry column and merge again with census district dataset


ggplot() +
  geom_sf(data = FAI_bank, aes(fill= quintile),
          color = "grey20") +
  geom_sf(data = bank_points, 
          aes(shape = type), 
          size = 0.4) +
  theme_map() +
  scale_fill_continuous(type = "viridis",
                        direction = -1) +
  ggtitle("Access to bank in Vienna") +
  scale_shape_discrete() +  
  theme(legend.position = "left", plot.title = element_text(hjust = 0.2, vjust = -30)) +
  geom_text(data = FAI_care, aes(x = st_coordinates(st_centroid(FAI_care))[, "X"], y = st_coordinates(st_centroid(FAI_care))[, "Y"], label = ZBEZ), size = 2) 



# displaying results on aggregated district level for better data display 
# how to see FAI per district?

# shortcut of FAI since no weighting done and isochrone counts irrelvant for bez analysis

bank_points_agg <- data_bez |> 
  mutate(count = lengths(st_intersects(data_bez, bank_points)))

data_pop_agg <- data_pop |>
  st_drop_geometry() |>
  group_by(BEZ) |>
  aggregate(POP_TOTAL ~ BEZ, sum) 

FAI_bank_agg <- merge(bank_points_agg, data_pop_agg, by = "BEZ") |> 
  mutate( FAI_total = count/POP_TOTAL,
          FAI_mean = mean(FAI_total, na.rm = T),   # to get the FAI standardized: mean, sd and z variation
          FAI_sd = sd(FAI_total, na.rm = T), 
          FAI_final = (FAI_total - FAI_mean) / FAI_sd) |>
  st_as_sf()

quintile <- quantile(FAI_bank_agg$FAI_final, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
FAI_bank_agg$quintile <- cut(FAI_bank_agg$FAI_final, breaks = quintile, labels = FALSE, include.lowest = TRUE)

# write to disk
output_path = here("FAI/bank/FAI_bank_agg.geojson")
# delete old version
unlink(output_path)
write_sf(FAI_bank_agg, output_path)

# plotting
ggplot()  +
  geom_sf(data = FAI_bank_agg, aes(fill= quintile),
          color = "grey20", 
          size = 0.4) +
  geom_sf(data = bank_points, 
          aes(shape = type), 
          size = 0.4)  + 
  theme_map() +
  scale_fill_continuous(type = "viridis",
                        direction = -1) + 
  #ggtitle("Access to bank in Vienna") +
  scale_shape_discrete() + 
  theme(legend.position = "left") +   #, plot.title = element_text(hjust = 0.2, vjust = -35)
  geom_text(data = data_bez, aes(x = st_coordinates(st_centroid(data_bez))[, "X"], y = st_coordinates(st_centroid(data_bez))[, "Y"], label = BEZ), size = 2) 

# save image
ggsave(file = "FAI/Overview/FAI_bank_agg.png", plot = last_plot(), width = 10, height = 8, units = "in")





