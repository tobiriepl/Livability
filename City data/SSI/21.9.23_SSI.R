

library(sf)
library(tidyverse)
library(ggthemes)

ssi_selim = read_csv("City data/SSI/ssidata_selim.csv")
data_zbez = read_sf("City data/Census Districts/data_zbez.geojson")
data_bez = read_sf("City data/Districts/data_bez.geojson")
data_iso = read_sf(here("City data/Isochrones/Isochrones15min.geojson")) 
data_pop = read_sf("City data/Population/data_pop.geojson")
data_ring = read_sf("City data/Aesthetics/data_ring.geojson")
data_gürtel = read_sf("City data/Aesthetics/data_gürtel.geojson")
data_borders = read_sf("City data/Aesthetics/data_borders.geojson")
data_donau = read_sf("City data/Aesthetics/data_donau.geojson")
data_donaukanal = read_sf("City data/Aesthetics/data_donaukanal.geojson")
data_lake = read_sf("City data/Aesthetics/data_lake.geojson")

data_ssi = merge(ssi_selim, data_zbez, by = "ZBEZ") |>
  select(ZBEZ, university_share, avg_income, unemprate, benefit_share, SSI, geometry = "geometry.y") |>
  mutate(university_share = as.numeric(university_share),
         avg_income = as.numeric(avg_income),
         unemprate = as.numeric(unemprate),
         benefit_share = as.numeric(benefit_share),
         SSI = as.numeric(SSI)) |>
  st_as_sf()

#divide into quintiles
quintile <- quantile(data_ssi$SSI, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
data_ssi$quintile <- cut(data_ssi$SSI, breaks = quintile, labels = FALSE, include.lowest = TRUE)


# data saving  --------------------------------------
output_path = "City data/SSI/data_ssi.geojson"   # set file path
unlink(output_path)   # delete old version
write_sf(data_ssi, output_path)


# plot 
ggplot() + 
  geom_sf(data = data_zbez) +
  geom_sf(data = data_ssi, aes(fill = quintile), 
          color = "grey20", 
          size = 0.4) + 
  theme_map() +
  scale_fill_continuous(type = "viridis",
                        direction = -1) +
  ggtitle("Social Status") +
  scale_shape_discrete()  +
  geom_sf(data = data_ring, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_gürtel, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_donau, fill = "transparent", color = "lightblue", lwd = 2.5) +
  geom_sf(data = data_lake, fill = "transparent", color = "lightblue", lwd = 1.7) +
  geom_sf(data = data_donaukanal, fill = "transparent", color = "lightblue", lwd = 3.5) +
  geom_sf(data = data_borders, fill = "transparent", color = "black", lwd = 1.5) +
  theme(legend.position = "left", plot.title = element_text(size = 32, hjust = 0.1, vjust = -6, face = "bold", family = "serif"), 
        legend.text = element_text(size = 20, family = "serif"), legend.title = element_text(size = 24, face = "bold", family = "serif")) +    
  geom_text(data = data_bez, aes(x = st_coordinates(st_centroid(data_bez))[, "X"], y = st_coordinates(st_centroid(data_bez))[, "Y"], label = BEZ), size = 5, family = "serif", fontface = "bold", color = "red")  +
  #  geom_sf(data = data_iso, fill = "transparent", color ="pink", lwd = 1) +
 # geom_sf(data = data_cir, fill = "transparent", color = "pink", lwd = 2) +
#  geom_sf(data = data_center, fill = "transparent", color = "pink", lwd = 35, size = 3) 

# save image
ggsave(file = "City data/SSI/SSI_filtered.png", plot = last_plot(), width = 10, height = 8, units = "in")



