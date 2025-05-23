


# get landmarks. code by selim (see mail)

library(sf)
library(tidyverse)
library(mapview)
library(ggplot2)
library(here)

data_ssi = read_sf("SSI/data_ssi.geojson")
data_zbez = read_sf("Data/Census Districts/ZAEHLBEZIRKOGDPolygon.shp")
data_borders <- st_read("Aesthetics/BEZIRKSGRENZEOGD.json",  options = "ENCODING=WINDOWS-1252")
data_streets <- st_read("Aesthetics/STRASSENGRAPHOGD.json",  options = "ENCODING=WINDOWS-1252")
data_rivers <- st_read("Aesthetics/FLIESSGEWOGD.json",  options = "ENCODING=WINDOWS-1252")
data_lakes <- st_read("Aesthetics/STEHENDEGEWOGD.json", options = "ENCODING=WINDOWS-1252")


# subset
data_ring <- data_streets[data_streets$FEATURENAM %in% c("Schottenring", "Burgring", "Opernring", "Kärntner Ring", "Dr.-Karl-Renner-Ring",
                                        "Schubertring", "Parkring", "Universitätsring", "Stubenring"),]
data_gürtel <- data_streets[data_streets$FEATURENAM %in% c("Neubaugürtel", "Landstraßer Gürtel", "Döblinger Gürtel", "Margaretengürtel", "Wiedner Gürtel",
                                          "Währinger Gürtel", "Mariahilfer Gürtel", "Lerchenfelder Gürtel", "Lerchenfelder Gürtel",
                                          "Gumpendorfer Gürtel", "Hernalser Gürtel", "Sechshauser Gürtel", "Gaudenzdorfer Gürtel"),]
data_donau <- data_rivers[data_rivers$NAME %in% c("Neue Donau", "Donau"),]
data_donaukanal <- data_rivers[data_rivers$NAME %in% c("Donaukanal"),]
data_lake <- data_lakes[data_lakes$NAME %in% c("Neue Donau", "Donau", "Alte Donau"),]


vienna_map <- ggplot() +
 # geom_sf(data = data_ssi,  aes(fill = SSI), color = "white", size = 0.1, show.legend = T) +
  # scale_fill_viridis_c() +
  geom_sf(data = data_borders, fill = "transparent", color = "black", lwd = 1) +
  geom_sf(data = data_ring, fill = "transparent", color = "black", lwd = 2) +
  geom_sf(data = data_gürtel, fill = "transparent", color = "black", lwd = 2) +
  geom_sf(data = data_donau, fill = "transparent", color = "lightblue", lwd = 2.2) +
  geom_sf(data = data_lake, fill = "transparent", color = "lightblue", lwd = 1.5) +
  geom_sf(data = data_donaukanal, fill = "transparent", color = "lightblue", lwd = 1.5) 
  # +theme_bw()
  # +new_retro()

vienna_map 

data_aes = bind_rows(data_borders, data_donau, data_lake, data_donaukanal, data_ring, data_gürtel)



# data saving  --------------------------------------

#data_aes
output_path = "Aesthetics/data_aes.geojson"
unlink(output_path)
write_sf(data_aes, output_path)

#data_borders
output_path = "Aesthetics/data_borders.geojson"
unlink(output_path)
write_sf(data_borders, output_path)

#data_donau
output_path = "Aesthetics/data_donau.geojson"
unlink(output_path)
write_sf(data_donau, output_path)

#data_lake
output_path = "Aesthetics/data_lake.geojson"
unlink(output_path)
write_sf(data_lake, output_path)

#data_donaukanal
output_path = "Aesthetics/data_donaukanal.geojson"
unlink(output_path)
write_sf(data_donaukanal, output_path)

#data_ring
output_path = "Aesthetics/data_ring.geojson"
unlink(output_path)
write_sf(data_ring, output_path)

#data_gürtel
output_path = "Aesthetics/data_gürtel.geojson"
unlink(output_path)
write_sf(data_gürtel, output_path)




