## FLI calculation 
# load packages
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

# load data
data_zbez = read_sf("City data/Census Districts/ZAEHLBEZIRKOGDPolygon.shp")
data_bez = read_sf("City data/Districts/data_bez.geojson")
data_ring = read_sf("City data/Aesthetics/data_ring.geojson")
data_gürtel = read_sf("City data/Aesthetics/data_gürtel.geojson")
data_borders = read_sf("City data/Aesthetics/data_borders.geojson")
data_donau = read_sf("City data/Aesthetics/data_donau.geojson")
data_donaukanal = read_sf("City data/Aesthetics/data_donaukanal.geojson")
data_lake = read_sf("City data/Aesthetics/data_lake.geojson")

data_household = read_sf("FE4/Data/Disposable Income/Households/data_house.geojson")
data_pop = read_sf("City data/Population/data_pop.geojson")

data_essentials = read_sf("FAI/All/FAI_all.geojson") |> 
  select(BEZ, FAI_total, quintile, everything()) |>
  mutate(FAI_score = percent_rank(FAI_total) * 100)

data_resid = read_sf("FE4/Overview/data_FE4.geojson") |>
  mutate( FE4_score = percent_rank(resid_inc_avg_sd) * 100)

#View(data_resid)

data_FLI = merge(data_essentials,  data_resid |> st_drop_geometry(), by = "BEZ") |>
  select(!contains("quintile")) |>
  mutate(FLI = FAI_total + resid_inc_avg_sd,
         FLI_score = (FAI_score + FE4_score) / 2)     # divide by 200 to get 0 to 1 ranking and then multiply by 100, or simply divide by 2


# get quintiles
quintile <- quantile(data_FLI$FLI_score, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
# quintile <- quintile + seq(0, 0.001, length.out = length(quintile))
data_FLI$quintile <- cut(data_FLI$FLI_score, breaks = quintile, labels = FALSE, include.lowest = TRUE)

data_FLI_final = data_FLI |>
  select(BEZ, quintile, FLI_score,  FAI_score, FE4_score, everything())

#View(data_FLI_final)
output_path = here("FLI/data_FLI.geojson")
# delete old version
unlink(output_path)
write_sf(data_FLI_final, output_path)



#### FLI in Vienna 
ggplot() +
  geom_sf(data = data_FLI_final, aes(fill= quintile),
          color = "white") +
  theme_map() +
  scale_fill_continuous(type = "viridis",
                        direction = -1) +
  ggtitle("Foundational livability") +
  scale_shape_discrete()  +
  geom_sf(data = data_ring, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_gürtel, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_donau, fill = "transparent", color = "lightblue", lwd = 2.5) +
  geom_sf(data = data_lake, fill = "transparent", color = "lightblue", lwd = 1.7) +
  geom_sf(data = data_donaukanal, fill = "transparent", color = "lightblue", lwd = 3.5) +
  geom_sf(data = data_borders, fill = "transparent", color = "black", lwd = 1.5) +
  theme(legend.position = "left", plot.title = element_text(size = 32, hjust = -0.35, vjust = -10, face = "bold", family = "serif"), 
        legend.text = element_text(size = 20, family = "serif"), legend.title = element_text(size = 24, face = "bold", family = "serif"),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA)) +    
  geom_text(data = data_bez, aes(x = st_coordinates(st_centroid(data_bez))[, "X"], y = st_coordinates(st_centroid(data_bez))[, "Y"], label = BEZ), size = 5, family = "serif", fontface = "bold", color = "red") +
  guides(fill = guide_legend(title = "Quintile"))

ggsave(file = "FLI/FLI_avg.png", plot = last_plot(), width = 10, height = 8, units = "in")

