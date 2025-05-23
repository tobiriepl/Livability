## R Code: mapping the Foundational Infrastructure in Vienna

library(sf) 
library(osmdata)
library(tidyverse)
library(mapview) 
library(jsonlite)
library(readr)


library(dbscan)

# read raw data   --------------------------------------------------------------
census_districts_vienna = read_sf("Data/Census Districts/ZAEHLBEZIRKOGDPolygon.shp")  

# approach 1: take complete nature dataset and assign amenity type through names

# data background on nature: siehe https://www.wien.gv.at/umweltschutz/umweltgut/oeffentlich.html  
# Bewertungskategorien: Als "öffentlich zugänglich" wurden dabei Flächen gewertet, welche allgemein und unentgeltlich zugänglich sind, unabhängig von ihrer Widmung oder von Eigentumsverhältnissen. Nicht berücksichtigt wurden daher (öffentliche) Freibäder und Grünflächen von Wohnhausanlagen, auch wenn diese in städtischem Besitz sind (Gemeindebauten). Baumscheiben und Baumreihen im Straßenraum blieben generell unberücksichtigt. Aufgrund der sehr eingeschränkten Nutzbarkeit für Erholungszwecke wurden auch Friedhöfe nicht berücksichtigt. Für Feld- und Weinbaulandschaften wurde vom Projektteam entschieden, nur die vom motorisierten Individualverkehr freien Wege sowie einen 15 Meter breiten Streifen neben diesen zu berücksichtigen. Dies bildet nach Meinung der Abteilung Stadt Wien – Umweltschutz (MA 22) im Unterschied zu einer vollflächige Berücksichtigung die Qualität des Angebotes für Erholungsnutzende besser ab. Ähnlich wurde mit Wasserflächen verfahren: Von öffentlich zugänglichen Grünflächen am Ufer wurden 15 Meter Wasserfläche in die Flächenaufstellung einbezogen. Ebenso wurde der 15 Meter breite Streifen in Naturschutzgebieten mit Wegegebot (Lainzer Tiergarten und Nationalpark Donauauen) verwendet.
# Flächentypen:Die erfassten Flächen wurden aufgrund ihrer Nutzbarkeit aus Sicht der Erholungsfunktion sowie ihrer Gestalt in Typen kategorisiert. Als Kategorien wurden gewählt:Wald- und Wiesenlandschaft, Feldlandschaft, Weinbaulandschaft, Parklandschaft, Abstandsgrün, Ruderalflächen (vom Menschen unbeabsichtigt geschaffene Vegetation), Urbanes Grün. 
# Zusammengehörende, aber physisch voneinander getrennte Teilflächen (wie beispielsweise die auf dem Plan durch Brücken und Querstraßen getrennten Teile der Donauinsel), wurden zusammengefasst.


nature_official = read_sf("Data/Nature/OEFFGRUENFLOGD.json")  

nature_official_clean <- st_centroid(nature_official) |>
  select(name = "T_TEXT") |>
  mutate(amenity = "green_space",
         amenity = case_when(str_detect(tolower(name), "park") ~ "park", TRUE ~ amenity), 
         amenity = case_when(str_detect(tolower(name), "garten") ~ "garden", TRUE ~ amenity),
         amenity = case_when(str_detect(tolower(name), "wald") ~ "forest", TRUE ~ amenity))

# some data exploration: wienerwald <- nature_official_clean |> filter(name == "Wienerwald")

# all wienerwald data in two zbez: mapview(wienerwald) + mapview(data_zbez)

# nature_data_unique <- nature_official_clean[!duplicated(nature_official_clean$name), ]  # not removing duplicates since forest can be in several districts

mapview(nature_official_clean)
View(nature_data_unique)

# approach2: bind rows of all datasets (e.g., dog area, garden, parks, nature)
# but issue with duplicated not fixed yet while nature includes even green areas none of the other includes (e.g., wiener wald)
# parkanlagen

parks_official = read_sf("Data/Nature/PARKINFOOGD.json")
gardens_official = read_sf("Data/Nature/HISTGARTENOGD.json")
dog_areas_official = read_sf("Data/Nature/HUNDEZONEOGD.json")

View(dog_areas_official)

parks_official_clean <- parks_official |>
  mutate(amenity = "park") |>
  select(name = "ANL_NAME", amenity, geometry)

gardens_official_clean <- st_centroid(gardens_official) |>
  mutate(amenity = "garden") |>
  select(name = "BEZEICHNUNG", amenity, geometry)

dog_areas_official_clean <- dog_areas_official |>
  mutate(amenity = "dog_area") |>
  select(name = "PARK", amenity, geometry)

nature2_data_merged <- bind_rows(parks_official_clean, gardens_official_clean, dog_areas_official_clean, nature_data_unique) |>
  filter(!grepl("GA |PA ", name))
nature2_data_unique <- nature2_data_merged[!duplicated(nature2_data_merged$name), ]


# Compute clusters using DBSCAN
dbscan_result <- dbscan(nature2_data_unique, eps = 0.005, minPts = 2)

# Add cluster labels to the sf object
sf_data$cluster <- dbscan_result$cluster


View(nature2_data_merged)
View(nature2_data_unique)

mapview(dog_areas_official_clean, col.region = "red") + mapview(gardens_official_clean, col.region = "yellow") + mapview(parks_official_clean, col.region = "blue") + mapview(nature_data_unique, col.region = "green")
View(nature2_data)
mapview(nature2_data_unique)







# final data preparation and saving --------------------------------------

nature_data_final <- st_join(nature_data_unique, census_districts_vienna, join = st_within) |>   
  filter(!is.na(ZBEZ)) |>    # to cut providers not in vienna (they have no ZBEZ data)
  select(name, amenity, ZBEZ, geometry) |>
  group_by(ZBEZ) 

mapview(nature_data_final)
View(nature_data_final)


# write to disk
output_path = here("Data/Nature/nature_points.geojson")

# delete old version
unlink(output_path)
write_sf(nature_data_final, output_path)








