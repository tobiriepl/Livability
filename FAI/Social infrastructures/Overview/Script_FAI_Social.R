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
library(ggthemes) 
library(ggpattern)
library(ggdist)
library(extrafont)


# import FAI of each sector: filtered ones

FAI_culture = read_sf("FAI/Social Infrastructures/Culture/FAI_culture_filtered.geojson") |> select(ZBEZ, FAI_culture = "FAI_final") |> st_drop_geometry()
FAI_nature = read_sf("FAI/Social Infrastructures/Nature/FAI_nature_filtered.geojson") |> select(ZBEZ, FAI_nature = "FAI_final") |> st_drop_geometry()

data_zbez = read_sf("City data/Census Districts/data_zbez.geojson")
data_pop = read_sf("City data/Population/data_pop.geojson")

data_ring = read_sf("City data/Aesthetics/data_ring.geojson")
data_gürtel = read_sf("City data/Aesthetics/data_gürtel.geojson")
data_borders = read_sf("City data/Aesthetics/data_borders.geojson")
data_donau = read_sf("City data/Aesthetics/data_donau.geojson")
data_donaukanal = read_sf("City data/Aesthetics/data_donaukanal.geojson")
data_lake = read_sf("City data/Aesthetics/data_lake.geojson")

FAI_social = left_join(FAI_culture, FAI_nature, by = "ZBEZ")

FAI_social_final = FAI_social |>    #2nd standardization
  mutate(FAI_sum =  (FAI_culture + FAI_nature),
         FAI_total = (FAI_sum - mean(FAI_sum))/sd(FAI_sum))


View(FAI_social_final)

#divide into quintiles
quintile <- quantile(FAI_social_final$FAI_total, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
FAI_social_final$quintile <- cut(FAI_social_final$FAI_total, breaks = quintile, labels = FALSE, include.lowest = TRUE)


#add geometry again
FAI_social_final <- merge(FAI_social_final, data_zbez, by = "ZBEZ") |>
  st_as_sf() |>
  select(ZBEZ, BEZ, FAI_culture, FAI_nature, FAI_total, quintile, geometry)
View(FAI_social_final)



# write to disk
output_path = "FAI/Social Infrastructures/Overview/FAI_social_filtered.geojson"
# delete old version
unlink(output_path)
write_sf(FAI_social_final, output_path)

# write to disk
output_path = "FAI/All/FAI_social_filtered.geojson"
# delete old version
unlink(output_path)
write_sf(FAI_all_final, output_path)

View(FAI_social_final)



# just checking..plotting final FAI data ----------------------------------------------------------

#data_projects = read_sf("City data/Projects/data_projects.geojson")

ggplot() +
  geom_sf(data = data_zbez)  +
  geom_sf(data = FAI_social_final, aes(fill= quintile),
          color = "white", 
          size = 0.4) +
  theme_map() +
  scale_fill_continuous(type = "viridis",
                        direction = -1) +
  ggtitle("Social infrastructures") +
  scale_shape_discrete()  +
  theme_map() +
  geom_sf(data = data_ring, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_gürtel, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_donau, fill = "transparent", color = "lightblue", lwd = 2.5) +
  geom_sf(data = data_lake, fill = "transparent", color = "lightblue", lwd = 1.7) +
  geom_sf(data = data_donaukanal, fill = "transparent", color = "lightblue", lwd = 3.5) +
  geom_sf(data = data_borders, fill = "transparent", color = "black", lwd = 1.5) +
  #  geom_sf(data = data_projects, aes(size = Size, color = Status), alpha = 0.8) +
  #  scale_size_manual(values = c(10, 6, 2)) +
  #  scale_color_manual(values = c("yellow", "green", "red")) +
  theme(legend.position = "left", plot.title = element_text(size = 26, hjust = -0.05, vjust = -10, face = "bold", family = "serif"), 
        legend.text = element_text(size = 14, family = "serif"), legend.title = element_text(size = 18, face = "bold", family = "serif")) +    
  geom_text(data = data_bez, aes(x = st_coordinates(st_centroid(data_bez))[, "X"], y = st_coordinates(st_centroid(data_bez))[, "Y"], label = BEZ), size = 5, family = "serif", fontface = "bold", color = "red") +
  guides(fill = guide_legend(title = "Quintile"))
#     color = guide_legend(title = "Implementation status"),
#    size = guide_legend(title = "Project size"))

# save image
ggsave(file = "FAI/Social Infrastructures/Overview/FAI_social_filtered.png", plot = last_plot(), width = 10, height = 8, units = "in")
ggsave(file = "FAI/All/FAI_social_filtered.png", plot = last_plot(), width = 10, height = 8, units = "in")


