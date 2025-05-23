
# city map with key features

library(sf) 
library(osmdata)
library(tidyverse)
library(mapview) 
library(ggthemes) 
library(here)
library(glue)
library(jsonlite)
library(hereR)
library(readr)
library(ggthemes) 
library(ggpattern)
library(ggdist)
library(extrafont)
library(mapview)

#load data
data_pop = read_sf("City data/Population/data_pop.geojson")
data_ring = read_sf("City data/Aesthetics/data_ring.geojson")
data_gürtel = read_sf("City data/Aesthetics/data_gürtel.geojson")
data_borders = read_sf("City data/Aesthetics/data_borders.geojson")
data_donau = read_sf("City data/Aesthetics/data_donau.geojson")
data_donaukanal = read_sf("City data/Aesthetics/data_donaukanal.geojson")
data_lake = read_sf("City data/Aesthetics/data_lake.geojson")
data_projects = read_sf("City data/Projects/data_projects.geojson")
data_projects$Size = as.factor(data_projects$Size)
data_projects$Status = as.factor(data_projects$Status)
data_projects = data_projects |>
  select(Name, address, "Project type" = Type, "Project size" = Size, Status) 


data_regio = data_pop |> select(BEZ, geometry)
data_regio$regio <- ifelse(data_regio$BEZ %in% c("21","22"), "Transdanubia", 
                                     ifelse(data_regio$BEZ %in% c("01"), "City center",
                                            ifelse(data_regio$BEZ %in% c("02", "03","04", "05", "06", "07", "08", "09", "20"), "Inner area",  "Outer area")))
data_regio$regio = as.factor(data_regio$regio)


# data saving: regions --------------------------------------
output_path = "City data/City/data_regio.geojson"   # set file path
unlink(output_path)   # delete old version
write_sf(data_regio, output_path)


#View(data_regio)
ggplot() +
  geom_sf(data = data_regio, aes(fill = regio))  +
  theme_map() +
  scale_fill_grey() +  # Change the fill color to a spectrum of grey
  geom_sf(data = data_ring, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_gürtel, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_donau, fill = "transparent", color = "lightblue", lwd = 2.5) +
  geom_sf(data = data_lake, fill = "transparent", color = "lightblue", lwd = 1.7) +
  geom_sf(data = data_donaukanal, fill = "transparent", color = "lightblue", lwd = 3.5) +
  geom_sf(data = data_borders, fill = "transparent", color = "black", lwd = 1.5) +
  theme(legend.position = "left", plot.title = element_text(size = 26, hjust = 0.1, vjust = -10, face = "bold", family = "serif"), 
        legend.text = element_text(size = 14, family = "serif"), legend.title = element_text(size = 18, face = "bold", family = "serif"),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA)) +    
  geom_text(data = data_bez, aes(x = st_coordinates(st_centroid(data_bez))[, "X"], y = st_coordinates(st_centroid(data_bez))[, "Y"], label = BEZ), size = 5, family = "serif", fontface = "bold", color = "white") +
  labs(fill = "Regions")


# save image
ggsave(file = "City data/City/Vienna_map.png", plot = last_plot(), width = 10, height = 8, units = "in")




## for supplementary material
# projectes added

ggplot() +
  geom_sf(data = data_regio, aes(fill = regio)) +
  scale_fill_grey(name = "Region", start = 0.9, end = 0.3) +  # fill scale for regions
  theme_map() +
  geom_sf(data = data_ring, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_gürtel, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_donau, fill = "transparent", color = "lightblue", lwd = 2.5) +
  geom_sf(data = data_lake, fill = "transparent", color = "lightblue", lwd = 1.7) +
  geom_sf(data = data_donaukanal, fill = "transparent", color = "lightblue", lwd = 3.5) +
  geom_sf(data = data_borders, fill = "transparent", color = "black", lwd = 1.5) +
  geom_text(data = data_bez, aes(x = st_coordinates(st_centroid(data_bez))[,1], 
                                 y = st_coordinates(st_centroid(data_bez))[,2], label = BEZ), 
            size = 5, family = "serif", fontface = "bold", color = "white") +
  geom_sf(data = data_projects, 
          aes(size = `Project size`, color = Status),  # color for status
          shape = 21, , fill = "white", stroke = 2, alpha = 0.9) +  # white fill for bubbles
  scale_size_manual(name = "Project Size", values = c(Small = 3, Medium = 9, Large = 15)) +
  scale_color_manual(name = "Implementation Status", 
                     values = c("Currently implemented" = "yellow2", 
                                "Still planning" = "forestgreen", 
                                "Recently completed" = "darkred")) +
  theme(legend.position = "left", 
        plot.title = element_text(size = 26, hjust = 0.1, vjust = -10, face = "bold", family = "serif"), 
        legend.text = element_text(size = 14, family = "serif"), 
        legend.title = element_text(size = 18, face = "bold", family = "serif"),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA)) +
  labs(fill = "Regions", color = "Implementation Status")

# save image
ggsave(file = "City data/City/Vienna_projects.png", plot = last_plot(), width = 10, height = 8, units = "in")




