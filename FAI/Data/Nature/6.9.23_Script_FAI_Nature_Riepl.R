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
data_pop = read_sf("Data/Population/Population_Vienna.geojson")
FAI_nature = read_sf("FAI/Nature/FAI_nature.geojson")  # shortcut, no new calculation
data_dens = read_sf("Data/Population Density/pop_density.geojson")


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
    group_by(zbez, inZbez, amenity) |>
    summarise(n_amenities = n()) |>
    pivot_wider(        # make long to wide
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
  select( "ZBEZ" = zbez, "Park_Iso" = FALSE_park, "Garden_Iso" = FALSE_garden, "Green_Space_Iso" = FALSE_green_space, "Forest_Iso" = FALSE_forest, 
          "Park_Zbez" = TRUE_park, "Garden_Zbez" = TRUE_garden, "Green_Space_Zbez" = TRUE_green_space, "Forest_Zbez" = TRUE_forest)

all_results_final[is.na(all_results_final)] <- 0   # to replace NaNs with 0 for further calculations
View(all_results_final)


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

View(FAI_nature)
quantile <- quantile(FAI_nature$FAI_final, probs = seq(0, 1, by = 0.2), na.rm = TRUE)


FAI_nature$quantile <- cut(FAI_nature$FAI_final, breaks = quantile, labels = FALSE)

?cut()
View(FAI_nature)
      
# write to disk
output_path = here("FAI/Nature/FAI_nature.geojson")
# delete old version
unlink(output_path)
write_sf(FAI_nature_final, output_path)


# just checking..plotting final FAI data ----------------------------------------------------------

ggplot() +
  geom_sf(data = FAI_nature, aes(fill= FAI_final),
          color = "grey20", 
          size = 0.4) +
  geom_sf(data = nature_points, 
          aes(shape = amenity), 
          size = 0.7, 
          show.legend = "point") +
  theme_map() +
  scale_fill_continuous(type = "viridis",
                        direction = -1) +
  ggtitle("Nature") + 
  theme(legend.position = "left", plot.title = element_text(hjust = 0.3, vjust = -50)) +
  geom_text(data = data_zbez, aes(x = st_coordinates(st_centroid(data_zbez))[, "X"], y = st_coordinates(st_centroid(data_zbez))[, "Y"], label = ZBEZ), size = 2) 




# removing outliers first for better data display

# option 1: only filter FAI
FAI_percentile_5 = quantile(FAI_nature$FAI_final, probs = 0.05)
FAI_percentile_95 = quantile(FAI_nature$FAI_final, probs = 0.95)

FAI_nature_filtered <- FAI_nature |>
  filter(FAI_percentile_5 < FAI_final & FAI_final < FAI_percentile_95) 

FAI_nature_filtered

# option 2: filter FAI and data_pop

#FAI again
FAI_percentile_5 = quantile(FAI_nature$FAI_final, probs = 0.05)
FAI_percentile_95 = quantile(FAI_nature$FAI_final, probs = 0.95)

FAI_nature_filtered <- FAI_nature |>
  filter(FAI_percentile_5 < FAI_final & FAI_final < FAI_percentile_95) 

# for data_pop
pop_percentile_5 = quantile(data_pop$POP_TOTAL, probs = 0.05)
pop_percentile_95 = quantile(data_pop$POP_TOTAL, probs = 0.95)
data_pop_filtered <- data_pop |>
  filter(pop_percentile_5 < POP_TOTAL & POP_TOTAL < pop_percentile_95) 

common_zbez = intersect(FAI_nature_filtered$ZBEZ, data_pop_filtered$ZBEZ)

# filter FAI_nature based on common values
FAI_nature_filtered2 <- FAI_nature_filtered[FAI_nature_filtered$ZBEZ %in% common_zbez, ] 

View(FAI_nature_filtered2)


ggplot() +
  geom_sf(data = FAI_nature_filtered2, aes(fill= FAI_final),
          color = "grey20", 
          size = 0.4) +
  geom_sf(data = nature_points, 
          aes(shape = amenity), 
          size = 0.7, 
          show.legend = "point") +
  theme_map() +
  scale_fill_continuous(type = "viridis",
                        direction = -1) +
  ggtitle("Nature") + 
  theme(legend.position = "left", plot.title = element_text(hjust = 0.3, vjust = -50)) +
  geom_text(data = data_zbez, aes(x = st_coordinates(st_centroid(data_zbez))[, "X"], y = st_coordinates(st_centroid(data_zbez))[, "Y"], label = ZBEZ), size = 2) 

# plot nature points only of the zbez that are shown

nature_points_filtered2 = st_intersection(nature_points, FAI_nature_filtered2)
View(nature_points_filtered2)

ggplot() +
  geom_sf(data = FAI_nature_filtered2, aes(fill= FAI_final),
          color = "grey20", 
          size = 0.4) +
  geom_sf(data = nature_points_filtered2, 
          aes(shape = amenity), 
          size = 0.7, 
          show.legend = "point") +
  theme_map() +
  scale_fill_continuous(type = "viridis",
                        direction = -1) +
  ggtitle("Nature") + 
  theme(legend.position = "left", plot.title = element_text(hjust = 0.3, vjust = -50)) +
  geom_text(data = FAI_nature_filtered2, aes(x = st_coordinates(st_centroid(FAI_nature_filtered2))[, "X"], y = st_coordinates(st_centroid(FAI_nature_filtered2))[, "Y"], label = ZBEZ), size = 2) 



# displaying results on aggregated district level for better data display 
# how to see FAI per district?

# link zbez to bez. drop geometry for merging 
data1 <- FAI_nature |> st_drop_geometry()  # get rid of geometry to join
data2 <- data_zbez |> st_drop_geometry()

# merge and aggregate that bitch
step1 = merge(data1, data2, by = "ZBEZ") |>
  select(ZBEZ, BEZ, FAI_final) |>
  group_by(BEZ) |>
  aggregate(FAI_final ~ BEZ, sum) 

# add geometry back and convert into sf type
step2 = merge(step1, data_bez, by = "BEZ") |>
  st_as_sf()


# plot that bitch
ggplot()  +
  geom_sf(data = step2, aes(fill= FAI_final),
          color = "grey20", 
          size = 0.4) +
  geom_sf(data = nature_points, 
          aes(shape = amenity), 
          size = 0.4)  + 
  theme_map() +
  scale_fill_continuous(type = "viridis",
                        direction = -1) +
  scale_shape_discrete() + 
  theme(legend.position = "left", plot.title = element_text(hjust = 0.3, vjust = -50)) +
  geom_text(data = data_bez, aes(x = st_coordinates(st_centroid(data_bez))[, "X"], y = st_coordinates(st_centroid(data_bez))[, "Y"], label = BEZ), size = 2) 


# understanding figure: why so little nature in outskirts

# 10th district big but little green, 
# 22th district big with a lot of green but not accessible as it is agricultural area
  
