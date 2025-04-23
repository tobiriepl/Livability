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

FAI_healthcare = read_sf("FAI/Essentials/Healthcare/FAI_healthcare_filtered.geojson") |> select(ZBEZ, FAI_healthcare = "FAI_final") |> st_drop_geometry()
FAI_care = read_sf("FAI/Essentials/Care/FAI_care_filtered.geojson") |> select(ZBEZ, FAI_care = "FAI_final") |> st_drop_geometry()
FAI_education = read_sf("FAI/Essentials/Education/FAI_education_filtered.geojson") |> select(ZBEZ, FAI_education = "FAI_final") |> st_drop_geometry()
FAI_housing = read_sf("FAI/Essentials/Housing/FAI_housing_filtered.geojson") |> select(ZBEZ, FAI_housing = "FAI_final") |> st_drop_geometry()
FAI_admin = read_sf("FAI/Essentials/Public admin/FAI_admin_filtered.geojson") |> select(ZBEZ, FAI_admin = "FAI_final") |> st_drop_geometry()
FAI_laworder = read_sf("FAI/Essentials/Law&Order/FAI_laworder_filtered.geojson") |> select(ZBEZ, FAI_laworder = "FAI_final") |> st_drop_geometry()

FAI_utilities = read_sf("FAI/Essentials/Utilities/FAI_utilities_filtered.geojson") |> select(ZBEZ, FAI_utilities = "FAI_final") |> st_drop_geometry()
FAI_transport = read_sf("FAI/Essentials/Transport/FAI_transport_filtered.geojson") |> select(ZBEZ, FAI_transport = "FAI_final") |> st_drop_geometry()
FAI_food = read_sf("FAI/Essentials/Food/FAI_food_filtered.geojson") |> select(ZBEZ, FAI_food = "FAI_final") |> st_drop_geometry()
FAI_post = read_sf("FAI/Essentials/Post/FAI_post_filtered.geojson") |> select(ZBEZ, FAI_post = "FAI_final") |> st_drop_geometry()
FAI_bank = read_sf("FAI/Essentials/Bank/FAI_bank_filtered.geojson") |> select(ZBEZ, FAI_bank = "FAI_final") |> st_drop_geometry()

FAI_culture = read_sf("FAI/Social Infrastructures/Culture/FAI_culture_filtered.geojson") |> select(ZBEZ, FAI_culture = "FAI_final") |> st_drop_geometry()
FAI_nature = read_sf("FAI/Social Infrastructures/Nature/FAI_nature_filtered.geojson") |> select(ZBEZ, FAI_nature = "FAI_final") |> st_drop_geometry()


FAI_all <- left_join(FAI_healthcare, FAI_care, by = "ZBEZ")
FAI_all <- left_join(FAI_all, FAI_education, by = "ZBEZ")
FAI_all <- left_join(FAI_all, FAI_housing, by = "ZBEZ")
FAI_all <- left_join(FAI_all, FAI_admin, by = "ZBEZ")
FAI_all <- left_join(FAI_all, FAI_laworder, by = "ZBEZ")

FAI_all <- left_join(FAI_all, FAI_utilities, by = "ZBEZ")
FAI_all <- left_join(FAI_all, FAI_food, by = "ZBEZ")
FAI_all <- left_join(FAI_all, FAI_transport, by = "ZBEZ")
FAI_all <- left_join(FAI_all, FAI_post, by = "ZBEZ")
FAI_all <- left_join(FAI_all, FAI_bank, by = "ZBEZ")

FAI_all <- left_join(FAI_all, FAI_culture, by = "ZBEZ")
FAI_all <- left_join(FAI_all, FAI_nature, by = "ZBEZ")

FAI_all_final = FAI_all |>    #2nd standardization
  mutate(FAI_sum =  FAI_healthcare + FAI_care +  FAI_education +  FAI_housing + FAI_admin + FAI_laworder + 
           FAI_utilities + FAI_food + FAI_transport + FAI_post + FAI_bank + 
           FAI_culture + FAI_nature,
         FAI_total = (FAI_sum - mean(FAI_sum)/sd(FAI_sum)))

#divide into quintiles
quintile <- quantile(FAI_all_final$FAI_total, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
FAI_all_final$quintile <- cut(FAI_all_final$FAI_total, breaks = quintile, labels = FALSE, include.lowest = TRUE)


#add geometry again
FAI_all_final <- merge(FAI_all_final, data_zbez, by = "ZBEZ") |>
  st_as_sf() |>
  select(ZBEZ, BEZ, FAI_healthcare, FAI_care, FAI_education,  FAI_housing, FAI_admin, FAI_laworder, 
         FAI_utilities, FAI_food, FAI_transport, FAI_post, FAI_bank , 
         FAI_culture, FAI_nature, 
         FAI_total, quintile, geometry)


# write to disk
output_path = "FAI/All/FAI_all_filtered.geojson"
# delete old version
unlink(output_path)
write_sf(FAI_all_final, output_path)

View(FAI_all_final)


# just checking..plotting final FAI data ----------------------------------------------------------

ggplot() +
  geom_sf(data= data_zbez) +
  geom_sf(data = FAI_all_final, aes(fill= quintile),
          color = "white") +
  theme_map() +
  scale_fill_continuous(type = "viridis",
                        direction = -1) +
  ggtitle("Availability of essential services
and social infrastructures") +
  scale_shape_discrete()  +
  geom_sf(data = data_ring, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_gürtel, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_donau, fill = "transparent", color = "lightblue", lwd = 2.5) +
  geom_sf(data = data_lake, fill = "transparent", color = "lightblue", lwd = 1.7) +
  geom_sf(data = data_donaukanal, fill = "transparent", color = "lightblue", lwd = 3.5) +
  geom_sf(data = data_borders, fill = "transparent", color = "black", lwd = 1.5) +
  theme(legend.position = "left", plot.title = element_text(size = 32, hjust = -0.3, vjust = -3, face = "bold", family = "serif"),
        legend.text = element_text(size = 20, family = "serif"), legend.title = element_text(size = 24, face = "bold", family = "serif")) +    
  geom_text(data = data_bez, aes(x = st_coordinates(st_centroid(data_bez))[, "X"], y = st_coordinates(st_centroid(data_bez))[, "Y"], label = BEZ), size = 5, family = "serif", fontface = "bold", color = "red")  



# save image
ggsave(file = "FAI/All/FAI_all_filtered.png", plot = last_plot(), width = 10, height = 8, units = "in")


