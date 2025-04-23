
# city map with key features

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
library(mapview)



data_zbez = read_sf("City data/Census Districts/data_zbez.geojson")
data_pop_filtered = read_sf("City data/Population/data_pop_filtered.geojson")
  data_pop_filtered
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


data_regio = data_pop_filtered |> select(BEZ, ZBEZ, geometry)
data_regio$regio <- ifelse(data_regio$BEZ %in% c("21","22"), "Transdanubia", 
                                     ifelse(data_regio$BEZ %in% c("01"), "City center",
                                            ifelse(data_regio$BEZ %in% c("02", "03","04", "05", "06", "07", "08", "09", "20"), "Inner area",  "Outer area")))
data_regio$regio = as.factor(data_regio$regio)


# data saving: regions --------------------------------------
output_path = "City data/City/data_regio.geojson"   # set file path
unlink(output_path)   # delete old version
write_sf(data_regio, output_path)


View(data_regio)

View(data_projects)
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
  geom_sf(data = data_projects, aes(size = Size, color = Status), alpha = 0.8) +
  scale_size_manual(values = c(10, 6, 2)) +
  scale_color_manual(values = c("yellow", "green", "red")) +
  theme(legend.position = "left", plot.title = element_text(size = 26, hjust = 0.1, vjust = -10, face = "bold", family = "serif"), 
        legend.text = element_text(size = 14, family = "serif"), legend.title = element_text(size = 18, face = "bold", family = "serif")) +    
  geom_text(data = data_bez, aes(x = st_coordinates(st_centroid(data_bez))[, "X"], y = st_coordinates(st_centroid(data_bez))[, "Y"], label = BEZ), size = 5, family = "serif", fontface = "bold", color = "white") +
  labs(regio = "City regions", size = "Project size", color = " Implementation status") +
  guides(fill = guide_legend(title = "City regions"),
         color = guide_legend(title = "Implementation status"),
         size = guide_legend(title = "Project size"))

# save image
ggsave(file = "City data/City/Vienna_map.png", plot = last_plot(), width = 10, height = 8, units = "in")
