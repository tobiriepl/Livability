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

FAI_essentials_final = FAI_all |>    
  mutate(FAI_sum =  (FAI_healthcare + FAI_care +  FAI_education +  FAI_housing + FAI_admin + FAI_laworder + 
           FAI_utilities + FAI_food + FAI_transport + FAI_post + FAI_bank),  
         FAI_total =  (FAI_sum - mean(FAI_sum))/sd(FAI_sum))
View(FAI_essentials_final)




#divide into quintiles
quintile <- quantile(FAI_essentials_final$FAI_total, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
FAI_essentials_final$quintile <- cut(FAI_essentials_final$FAI_total, breaks = quintile, labels = FALSE, include.lowest = TRUE)


#add geometry again
FAI_essentials_final <- merge(FAI_essentials_final, data_zbez, by = "ZBEZ") |>
  st_as_sf() |>
  select(ZBEZ, BEZ, FAI_healthcare, FAI_care, FAI_education,  FAI_housing, FAI_admin, FAI_laworder, 
         FAI_utilities, FAI_food, FAI_transport, FAI_post, FAI_bank , 
         FAI_total, quintile, geometry)
View(FAI_essentials_final)


# write to disk
output_path = "FAI/Essentials/Overview/FAI_essentials_filtered.geojson"
# delete old version
unlink(output_path)
write_sf(FAI_essentials_final, output_path)

# write to disk
output_path = "FAI/All/FAI_essentials_filtered.geojson"
# delete old version
unlink(output_path)
write_sf(FAI_essentials_final, output_path)

View(FAI_essentials_final)




# just checking..plotting final FAI data ----------------------------------------------------------

data_typo = read_sf("Data/City/data_typo.geojson")
data_projects = read_sf("Data/Projects/data_projects.geojson")

ggplot() +
  geom_sf(data = data_zbez)  +
  geom_sf(data = FAI_essentials_final, aes(fill= quintile),
          color = "white", 
          size = 0.4) +
  theme_map() +
  scale_fill_continuous(type = "viridis",
                        direction = -1) +
  ggtitle("Essential services") +
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
  theme(legend.position = "left", plot.title = element_text(size = 26, hjust = 0.1, vjust = -10, face = "bold", family = "serif"), 
        legend.text = element_text(size = 14, family = "serif"), legend.title = element_text(size = 18, face = "bold", family = "serif")) +    
  geom_text(data = data_bez, aes(x = st_coordinates(st_centroid(data_bez))[, "X"], y = st_coordinates(st_centroid(data_bez))[, "Y"], label = BEZ), size = 5, family = "serif", fontface = "bold", color = "red") +
  guides(fill = guide_legend(title = "Quintile"))
    #     color = guide_legend(title = "Implementation status"),
     #    size = guide_legend(title = "Project size"))

# save image
ggsave(file = "FAI/Essentials/Overview/FAI_essentials_filtered.png", plot = last_plot(), width = 10, height = 8, units = "in")
ggsave(file = "FAI/All/FAI_essentials_filtered.png", plot = last_plot(), width = 10, height = 8, units = "in")



# to copy stuff if projects display wanted
ggplot() +
  geom_sf(data = data_zbez)  +
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
  labs( typo = "City regions", size = "Project size", color = " Implementation status") +
  guides(fill = guide_legend(title = "City regions"),
         color = guide_legend(title = "Implementation status"),
         size = guide_legend(title = "Project size"))

