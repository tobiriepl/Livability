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

FAI_healthcare = read_sf("FAI/Essentials/Healthcare/FAI_healthcare.geojson") |> select(BEZ, FAI_healthcare = "FAI_final") |> st_drop_geometry()

FAI_care = read_sf("FAI/Essentials/Care/FAI_care.geojson") |> select(BEZ, FAI_care = "FAI_final") |> st_drop_geometry()
FAI_education = read_sf("FAI/Essentials/Education/FAI_education.geojson") |> select(BEZ, FAI_education = "FAI_final") |> st_drop_geometry()
FAI_housing = read_sf("FAI/Essentials/Housing/FAI_housing.geojson") |> select(BEZ, FAI_housing = "FAI_final") |> st_drop_geometry()
FAI_admin = read_sf("FAI/Essentials/Public admin/FAI_admin.geojson") |> select(BEZ, FAI_admin = "FAI_final") |> st_drop_geometry()
FAI_laworder = read_sf("FAI/Essentials/Law&Order/FAI_laworder.geojson") |> select(BEZ, FAI_laworder = "FAI_final") |> st_drop_geometry()

FAI_utilities = read_sf("FAI/Essentials/Utilities/FAI_utilities.geojson") |> select(BEZ, FAI_utilities = "FAI_final") |> st_drop_geometry()
FAI_transport = read_sf("FAI/Essentials/Transport/FAI_transport.geojson") |> select(BEZ, FAI_transport = "FAI_final") |> st_drop_geometry()
FAI_food = read_sf("FAI/Essentials/Food/FAI_food.geojson") |> select(BEZ, FAI_food = "FAI_final") |> st_drop_geometry()
FAI_post = read_sf("FAI/Essentials/Post/FAI_post.geojson") |> select(BEZ, FAI_post = "FAI_final") |> st_drop_geometry()
FAI_bank = read_sf("FAI/Essentials/Bank/FAI_bank.geojson") |> select(BEZ, FAI_bank = "FAI_final") |> st_drop_geometry()

FAI_culture = read_sf("FAI/Social Infrastructures/Culture/FAI_culture.geojson") |> select(BEZ, FAI_culture = "FAI_final") |> st_drop_geometry()
FAI_nature = read_sf("FAI/Social Infrastructures/Nature/FAI_nature.geojson") |> select(BEZ, FAI_nature = "FAI_final") |> st_drop_geometry()

#View(FAI_food)

FAI_all <- left_join(FAI_healthcare, FAI_care, by = "BEZ")
FAI_all <- left_join(FAI_all, FAI_education, by = "BEZ")
FAI_all <- left_join(FAI_all, FAI_housing, by = "BEZ")
FAI_all <- left_join(FAI_all, FAI_admin, by = "BEZ")
FAI_all <- left_join(FAI_all, FAI_laworder, by = "BEZ")

FAI_all <- left_join(FAI_all, FAI_utilities, by = "BEZ")
FAI_all <- left_join(FAI_all, FAI_food, by = "BEZ")
FAI_all <- left_join(FAI_all, FAI_transport, by = "BEZ")
FAI_all <- left_join(FAI_all, FAI_post, by = "BEZ")
FAI_all <- left_join(FAI_all, FAI_bank, by = "BEZ")

FAI_all <- left_join(FAI_all, FAI_culture, by = "BEZ")
FAI_all <- left_join(FAI_all, FAI_nature, by = "BEZ")


#View(FAI_all)
FAI_all_final = FAI_all |>    #2nd standardization
  mutate(FAI_sum =  FAI_healthcare + FAI_care +  FAI_education +  FAI_housing + FAI_admin + FAI_laworder + 
           FAI_utilities + FAI_food + FAI_transport + FAI_post + FAI_bank + 
           FAI_culture + FAI_nature,
         FAI_total = (FAI_sum - mean(FAI_sum)/sd(FAI_sum)))

#divide into quintiles
quintile <- quantile(FAI_all_final$FAI_total, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
FAI_all_final$quintile <- cut(FAI_all_final$FAI_total, breaks = quintile, labels = FALSE, include.lowest = TRUE)


#add geometry again
FAI_all_final <- merge(FAI_all_final, data_bez , by = "BEZ") |>
  st_as_sf() |>
  select(BEZ, FAI_healthcare, FAI_care, FAI_education,  FAI_housing, FAI_admin, FAI_laworder, 
         FAI_utilities, FAI_food, FAI_transport, FAI_post, FAI_bank , 
         FAI_culture, FAI_nature, 
         FAI_total, quintile, geometry)

#View(FAI_all_final)


#View(data_pop)
# write to disk
output_path = "FAI/All/FAI_all.geojson"
# delete old version
unlink(output_path)
write_sf(FAI_all_final, output_path)


# plot

ggplot() +
  geom_sf(data= data_bez) +
  geom_sf(data = FAI_all_final, aes(fill= quintile),
          color = "white") +
  theme_map() +
  scale_fill_continuous(name = "Quintile", type = "viridis", direction = -1) +

  ggtitle("Availability of essential services") +
  scale_shape_discrete()  +
  geom_sf(data = data_ring, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_gürtel, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_donau, fill = "transparent", color = "lightblue", lwd = 2.5) +
  geom_sf(data = data_lake, fill = "transparent", color = "lightblue", lwd = 1.7) +
  geom_sf(data = data_donaukanal, fill = "transparent", color = "lightblue", lwd = 3.5) +
  geom_sf(data = data_borders, fill = "transparent", color = "black", lwd = 1.5) +
  theme(legend.position = "left", legend.background = element_blank(),
        plot.title = element_text(size = 32, hjust = -0.3, vjust = -1, face = "bold", family = "serif"),
        legend.text = element_text(size = 20, family = "serif"), legend.title = element_text(size = 24, face = "bold", family = "serif", vjust = 1.5)) +    
  geom_text(data = data_bez, aes(x = st_coordinates(st_centroid(data_bez))[, "X"], y = st_coordinates(st_centroid(data_bez))[, "Y"], label = BEZ), size = 5, family = "serif", fontface = "bold", color = "red")  


# save image
ggsave(file = "FAI/All/FAI_all.png", plot = last_plot(), width = 10, height = 8, units = "in")





### Supplementary Material: get sectoral FAIs

## healthcare
quintile <- quantile(FAI_all_final$FAI_healthcare, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
FAI_all_final$quintile <- cut(FAI_all_final$FAI_healthcare, breaks = quintile, labels = FALSE, include.lowest = TRUE)

ggplot() +
  geom_sf(data = FAI_all_final, aes(fill= quintile),
          color = "white") +
  theme_map() +
  scale_fill_continuous(name = "Quintile", type = "viridis", direction = -1) +
  
  ggtitle("Healthcare") +
  scale_shape_discrete()  +
  geom_sf(data = data_ring, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_gürtel, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_donau, fill = "transparent", color = "lightblue", lwd = 2.5) +
  geom_sf(data = data_lake, fill = "transparent", color = "lightblue", lwd = 1.7) +
  geom_sf(data = data_donaukanal, fill = "transparent", color = "lightblue", lwd = 3.5) +
  geom_sf(data = data_borders, fill = "transparent", color = "black", lwd = 1.5) +
  theme(legend.position = "left", legend.background = element_blank(),
        plot.title = element_text(size = 32, vjust = -1, face = "bold", family = "serif"),
        legend.text = element_text(size = 20, family = "serif"), legend.title = element_text(size = 24, face = "bold", family = "serif", vjust = 1.5)) +    
  geom_text(data = data_bez, aes(x = st_coordinates(st_centroid(data_bez))[, "X"], y = st_coordinates(st_centroid(data_bez))[, "Y"], label = BEZ), size = 5, family = "serif", fontface = "bold", color = "red")  


# save image
ggsave(file = "FAI/All/FAI_healthcare.png", plot = last_plot(), width = 10, height = 8, units = "in")


## social care
quintile <- quantile(FAI_all_final$FAI_care, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
FAI_all_final$quintile <- cut(FAI_all_final$FAI_care, breaks = quintile, labels = FALSE, include.lowest = TRUE)

ggplot() +
  geom_sf(data = FAI_all_final, aes(fill= quintile),
          color = "white") +
  theme_map() +
  scale_fill_continuous(name = "Quintile", type = "viridis", direction = -1) +
  
  ggtitle("Social care") +
  scale_shape_discrete()  +
  geom_sf(data = data_ring, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_gürtel, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_donau, fill = "transparent", color = "lightblue", lwd = 2.5) +
  geom_sf(data = data_lake, fill = "transparent", color = "lightblue", lwd = 1.7) +
  geom_sf(data = data_donaukanal, fill = "transparent", color = "lightblue", lwd = 3.5) +
  geom_sf(data = data_borders, fill = "transparent", color = "black", lwd = 1.5) +
  theme(legend.position = "left", legend.background = element_blank(),
        plot.title = element_text(size = 32, vjust = -1, face = "bold", family = "serif"),
        legend.text = element_text(size = 20, family = "serif"), legend.title = element_text(size = 24, face = "bold", family = "serif", vjust = 1.5)) +    
  geom_text(data = data_bez, aes(x = st_coordinates(st_centroid(data_bez))[, "X"], y = st_coordinates(st_centroid(data_bez))[, "Y"], label = BEZ), size = 5, family = "serif", fontface = "bold", color = "red")  


# save image
ggsave(file = "FAI/All/FAI_care.png", plot = last_plot(), width = 10, height = 8, units = "in")



## education
quintile <- quantile(FAI_all_final$FAI_education, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
FAI_all_final$quintile <- cut(FAI_all_final$FAI_education, breaks = quintile, labels = FALSE, include.lowest = TRUE)

ggplot() +
  geom_sf(data = FAI_all_final, aes(fill= quintile),
          color = "white") +
  theme_map() +
  scale_fill_continuous(name = "Quintile", type = "viridis", direction = -1) +
  
  ggtitle("Education") +
  scale_shape_discrete()  +
  geom_sf(data = data_ring, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_gürtel, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_donau, fill = "transparent", color = "lightblue", lwd = 2.5) +
  geom_sf(data = data_lake, fill = "transparent", color = "lightblue", lwd = 1.7) +
  geom_sf(data = data_donaukanal, fill = "transparent", color = "lightblue", lwd = 3.5) +
  geom_sf(data = data_borders, fill = "transparent", color = "black", lwd = 1.5) +
  theme(legend.position = "left", legend.background = element_blank(),
        plot.title = element_text(size = 32, vjust = -1, face = "bold", family = "serif"),
        legend.text = element_text(size = 20, family = "serif"), legend.title = element_text(size = 24, face = "bold", family = "serif", vjust = 1.5)) +    
  geom_text(data = data_bez, aes(x = st_coordinates(st_centroid(data_bez))[, "X"], y = st_coordinates(st_centroid(data_bez))[, "Y"], label = BEZ), size = 5, family = "serif", fontface = "bold", color = "red")  


# save image
ggsave(file = "FAI/All/FAI_education.png", plot = last_plot(), width = 10, height = 8, units = "in")




## housing
quintile <- quantile(FAI_all_final$FAI_housing, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
FAI_all_final$quintile <- cut(FAI_all_final$FAI_housing, breaks = quintile, labels = FALSE, include.lowest = TRUE)

ggplot() +
  geom_sf(data = FAI_all_final, aes(fill= quintile),
          color = "white") +
  theme_map() +
  scale_fill_continuous(name = "Quintile", type = "viridis", direction = -1) +
  
  ggtitle("Housing") +
  scale_shape_discrete()  +
  geom_sf(data = data_ring, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_gürtel, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_donau, fill = "transparent", color = "lightblue", lwd = 2.5) +
  geom_sf(data = data_lake, fill = "transparent", color = "lightblue", lwd = 1.7) +
  geom_sf(data = data_donaukanal, fill = "transparent", color = "lightblue", lwd = 3.5) +
  geom_sf(data = data_borders, fill = "transparent", color = "black", lwd = 1.5) +
  theme(legend.position = "left", legend.background = element_blank(),
        plot.title = element_text(size = 32, vjust = -1, face = "bold", family = "serif"),
        legend.text = element_text(size = 20, family = "serif"), legend.title = element_text(size = 24, face = "bold", family = "serif", vjust = 1.5)) +    
  geom_text(data = data_bez, aes(x = st_coordinates(st_centroid(data_bez))[, "X"], y = st_coordinates(st_centroid(data_bez))[, "Y"], label = BEZ), size = 5, family = "serif", fontface = "bold", color = "red")  


# save image
ggsave(file = "FAI/All/FAI_housing.png", plot = last_plot(), width = 10, height = 8, units = "in")




## public admin
# Get unique, sorted values
unique_vals <- sort(unique(FAI_all_final$FAI_admin))

# Create breaks based on equally sized quantiles of unique values
breaks <- quantile(unique_vals, probs = seq(0, 1, 0.2), na.rm = TRUE)

# Ensure breaks are unique to avoid errors in cut()
breaks <- unique(breaks)

FAI_all_final$quintile <- cut(FAI_all_final$FAI_admin,
                              breaks = breaks,
                              include.lowest = TRUE,
                              labels = FALSE)
ggplot() +
  geom_sf(data = FAI_all_final, aes(fill= quintile),
          color = "white") +
  theme_map() +
  scale_fill_continuous(name = "Quintile", type = "viridis", direction = -1) +
  
  ggtitle("Public admin") +
  scale_shape_discrete()  +
  geom_sf(data = data_ring, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_gürtel, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_donau, fill = "transparent", color = "lightblue", lwd = 2.5) +
  geom_sf(data = data_lake, fill = "transparent", color = "lightblue", lwd = 1.7) +
  geom_sf(data = data_donaukanal, fill = "transparent", color = "lightblue", lwd = 3.5) +
  geom_sf(data = data_borders, fill = "transparent", color = "black", lwd = 1.5) +
  theme(legend.position = "left", legend.background = element_blank(),
        plot.title = element_text(size = 32, vjust = -1, face = "bold", family = "serif"),
        legend.text = element_text(size = 20, family = "serif"), legend.title = element_text(size = 24, face = "bold", family = "serif", vjust = 1.5)) +    
  geom_text(data = data_bez, aes(x = st_coordinates(st_centroid(data_bez))[, "X"], y = st_coordinates(st_centroid(data_bez))[, "Y"], label = BEZ), size = 5, family = "serif", fontface = "bold", color = "red")  


# save image
ggsave(file = "FAI/All/FAI_admin.png", plot = last_plot(), width = 10, height = 8, units = "in")




## law and order
# get cuts
unique_vals <- sort(unique(FAI_all_final$FAI_laworder))
breaks <- quantile(unique_vals, probs = seq(0, 1, 0.2), na.rm = TRUE)
breaks <- unique(breaks)

FAI_all_final$quintile <- cut(FAI_all_final$FAI_laworder,
                              breaks = breaks,
                              include.lowest = TRUE,
                              labels = FALSE)


ggplot() +
  geom_sf(data = FAI_all_final, aes(fill= quintile),
          color = "white") +
  theme_map() +
  scale_fill_continuous(name = "Quintile", type = "viridis", direction = -1) +
  
  ggtitle("Law & order") +
  scale_shape_discrete()  +
  geom_sf(data = data_ring, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_gürtel, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_donau, fill = "transparent", color = "lightblue", lwd = 2.5) +
  geom_sf(data = data_lake, fill = "transparent", color = "lightblue", lwd = 1.7) +
  geom_sf(data = data_donaukanal, fill = "transparent", color = "lightblue", lwd = 3.5) +
  geom_sf(data = data_borders, fill = "transparent", color = "black", lwd = 1.5) +
  theme(legend.position = "left", legend.background = element_blank(),
        plot.title = element_text(size = 32, vjust = -1, face = "bold", family = "serif"),
        legend.text = element_text(size = 20, family = "serif"), legend.title = element_text(size = 24, face = "bold", family = "serif", vjust = 1.5)) +    
  geom_text(data = data_bez, aes(x = st_coordinates(st_centroid(data_bez))[, "X"], y = st_coordinates(st_centroid(data_bez))[, "Y"], label = BEZ), size = 5, family = "serif", fontface = "bold", color = "red")  


# save image
ggsave(file = "FAI/All/FAI_laworder.png", plot = last_plot(), width = 10, height = 8, units = "in")





## utilities
quintile <- quantile(FAI_all_final$FAI_utilities, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
FAI_all_final$quintile <- cut(FAI_all_final$FAI_utilities, breaks = quintile, labels = FALSE, include.lowest = TRUE)

ggplot() +
  geom_sf(data = FAI_all_final, aes(fill= quintile),
          color = "white") +
  theme_map() +
  scale_fill_continuous(name = "Quintile", type = "viridis", direction = -1) +
  
  ggtitle("Utilities") +
  scale_shape_discrete()  +
  geom_sf(data = data_ring, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_gürtel, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_donau, fill = "transparent", color = "lightblue", lwd = 2.5) +
  geom_sf(data = data_lake, fill = "transparent", color = "lightblue", lwd = 1.7) +
  geom_sf(data = data_donaukanal, fill = "transparent", color = "lightblue", lwd = 3.5) +
  geom_sf(data = data_borders, fill = "transparent", color = "black", lwd = 1.5) +
  theme(legend.position = "left", legend.background = element_blank(),
        plot.title = element_text(size = 32, vjust = -1, face = "bold", family = "serif"),
        legend.text = element_text(size = 20, family = "serif"), legend.title = element_text(size = 24, face = "bold", family = "serif", vjust = 1.5)) +    
  geom_text(data = data_bez, aes(x = st_coordinates(st_centroid(data_bez))[, "X"], y = st_coordinates(st_centroid(data_bez))[, "Y"], label = BEZ), size = 5, family = "serif", fontface = "bold", color = "red")  


# save image
ggsave(file = "FAI/All/FAI_utilities.png", plot = last_plot(), width = 10, height = 8, units = "in")




## food
quintile <- quantile(FAI_all_final$FAI_food, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
FAI_all_final$quintile <- cut(FAI_all_final$FAI_food, breaks = quintile, labels = FALSE, include.lowest = TRUE)

ggplot() +
  geom_sf(data = FAI_all_final, aes(fill= quintile),
          color = "white") +
  theme_map() +
  scale_fill_continuous(name = "Quintile", type = "viridis", direction = -1) +
  
  ggtitle("Food") +
  scale_shape_discrete()  +
  geom_sf(data = data_ring, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_gürtel, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_donau, fill = "transparent", color = "lightblue", lwd = 2.5) +
  geom_sf(data = data_lake, fill = "transparent", color = "lightblue", lwd = 1.7) +
  geom_sf(data = data_donaukanal, fill = "transparent", color = "lightblue", lwd = 3.5) +
  geom_sf(data = data_borders, fill = "transparent", color = "black", lwd = 1.5) +
  theme(legend.position = "left", legend.background = element_blank(),
        plot.title = element_text(size = 32, vjust = -1, face = "bold", family = "serif"),
        legend.text = element_text(size = 20, family = "serif"), legend.title = element_text(size = 24, face = "bold", family = "serif", vjust = 1.5)) +    
  geom_text(data = data_bez, aes(x = st_coordinates(st_centroid(data_bez))[, "X"], y = st_coordinates(st_centroid(data_bez))[, "Y"], label = BEZ), size = 5, family = "serif", fontface = "bold", color = "red")  


# save image
ggsave(file = "FAI/All/FAI_food.png", plot = last_plot(), width = 10, height = 8, units = "in")



## transport
quintile <- quantile(FAI_all_final$FAI_transport, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
FAI_all_final$quintile <- cut(FAI_all_final$FAI_transport, breaks = quintile, labels = FALSE, include.lowest = TRUE)

ggplot() +
  geom_sf(data = FAI_all_final, aes(fill= quintile),
          color = "white") +
  theme_map() +
  scale_fill_continuous(name = "Quintile", type = "viridis", direction = -1) +
  
  ggtitle("Transport") +
  scale_shape_discrete()  +
  geom_sf(data = data_ring, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_gürtel, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_donau, fill = "transparent", color = "lightblue", lwd = 2.5) +
  geom_sf(data = data_lake, fill = "transparent", color = "lightblue", lwd = 1.7) +
  geom_sf(data = data_donaukanal, fill = "transparent", color = "lightblue", lwd = 3.5) +
  geom_sf(data = data_borders, fill = "transparent", color = "black", lwd = 1.5) +
  theme(legend.position = "left", legend.background = element_blank(),
        plot.title = element_text(size = 32, vjust = -1, face = "bold", family = "serif"),
        legend.text = element_text(size = 20, family = "serif"), legend.title = element_text(size = 24, face = "bold", family = "serif", vjust = 1.5)) +    
  geom_text(data = data_bez, aes(x = st_coordinates(st_centroid(data_bez))[, "X"], y = st_coordinates(st_centroid(data_bez))[, "Y"], label = BEZ), size = 5, family = "serif", fontface = "bold", color = "red")  


# save image
ggsave(file = "FAI/All/FAI_transport.png", plot = last_plot(), width = 10, height = 8, units = "in")



## post
unique_vals <- sort(unique(FAI_all_final$FAI_post))
breaks <- quantile(unique_vals, probs = seq(0, 1, 0.2), na.rm = TRUE)
breaks <- unique(breaks)

FAI_all_final$quintile <- cut(FAI_all_final$FAI_post,
                              breaks = breaks,
                              include.lowest = TRUE,
                              labels = FALSE)
ggplot() +
  geom_sf(data = FAI_all_final, aes(fill= quintile),
          color = "white") +
  theme_map() +
  scale_fill_continuous(name = "Quintile", type = "viridis", direction = -1) +
  
  ggtitle("Postal services") +
  scale_shape_discrete()  +
  geom_sf(data = data_ring, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_gürtel, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_donau, fill = "transparent", color = "lightblue", lwd = 2.5) +
  geom_sf(data = data_lake, fill = "transparent", color = "lightblue", lwd = 1.7) +
  geom_sf(data = data_donaukanal, fill = "transparent", color = "lightblue", lwd = 3.5) +
  geom_sf(data = data_borders, fill = "transparent", color = "black", lwd = 1.5) +
  theme(legend.position = "left", legend.background = element_blank(),
        plot.title = element_text(size = 32, vjust = -1, face = "bold", family = "serif"),
        legend.text = element_text(size = 20, family = "serif"), legend.title = element_text(size = 24, face = "bold", family = "serif", vjust = 1.5)) +    
  geom_text(data = data_bez, aes(x = st_coordinates(st_centroid(data_bez))[, "X"], y = st_coordinates(st_centroid(data_bez))[, "Y"], label = BEZ), size = 5, family = "serif", fontface = "bold", color = "red")  


# save image
ggsave(file = "FAI/All/FAI_post.png", plot = last_plot(), width = 10, height = 8, units = "in")




## bank
unique_vals <- sort(unique(FAI_all_final$FAI_bank))
breaks <- quantile(unique_vals, probs = seq(0, 1, 0.2), na.rm = TRUE)
breaks <- unique(breaks)

FAI_all_final$quintile <- cut(FAI_all_final$FAI_bank,
                              breaks = breaks,
                              include.lowest = TRUE,
                              labels = FALSE)
ggplot() +
  geom_sf(data = FAI_all_final, aes(fill= quintile),
          color = "white") +
  theme_map() +
  scale_fill_continuous(name = "Quintile", type = "viridis", direction = -1) +
  
  ggtitle("Bank") +
  scale_shape_discrete()  +
  geom_sf(data = data_ring, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_gürtel, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_donau, fill = "transparent", color = "lightblue", lwd = 2.5) +
  geom_sf(data = data_lake, fill = "transparent", color = "lightblue", lwd = 1.7) +
  geom_sf(data = data_donaukanal, fill = "transparent", color = "lightblue", lwd = 3.5) +
  geom_sf(data = data_borders, fill = "transparent", color = "black", lwd = 1.5) +
  theme(legend.position = "left", legend.background = element_blank(),
        plot.title = element_text(size = 32, vjust = -1, face = "bold", family = "serif"),
        legend.text = element_text(size = 20, family = "serif"), legend.title = element_text(size = 24, face = "bold", family = "serif", vjust = 1.5)) +    
  geom_text(data = data_bez, aes(x = st_coordinates(st_centroid(data_bez))[, "X"], y = st_coordinates(st_centroid(data_bez))[, "Y"], label = BEZ), size = 5, family = "serif", fontface = "bold", color = "red")  


# save image
ggsave(file = "FAI/All/FAI_bank.png", plot = last_plot(), width = 10, height = 8, units = "in")




## culture
quintile <- unique(quantile(FAI_all_final$FAI_culture, probs = seq(0, 1, by = 0.2), na.rm = TRUE))
FAI_all_final$quintile <- cut(FAI_all_final$FAI_culture, breaks = quintile, labels = FALSE, include.lowest = TRUE)

ggplot() +
  geom_sf(data = FAI_all_final, aes(fill= quintile),
          color = "white") +
  theme_map() +
  scale_fill_continuous(name = "Quintile", type = "viridis", direction = -1) +
  
  ggtitle("Culture") +
  scale_shape_discrete()  +
  geom_sf(data = data_ring, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_gürtel, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_donau, fill = "transparent", color = "lightblue", lwd = 2.5) +
  geom_sf(data = data_lake, fill = "transparent", color = "lightblue", lwd = 1.7) +
  geom_sf(data = data_donaukanal, fill = "transparent", color = "lightblue", lwd = 3.5) +
  geom_sf(data = data_borders, fill = "transparent", color = "black", lwd = 1.5) +
  theme(legend.position = "left", legend.background = element_blank(),
        plot.title = element_text(size = 32, vjust = -1, face = "bold", family = "serif"),
        legend.text = element_text(size = 20, family = "serif"), legend.title = element_text(size = 24, face = "bold", family = "serif", vjust = 1.5)) +    
  geom_text(data = data_bez, aes(x = st_coordinates(st_centroid(data_bez))[, "X"], y = st_coordinates(st_centroid(data_bez))[, "Y"], label = BEZ), size = 5, family = "serif", fontface = "bold", color = "red")  


# save image
ggsave(file = "FAI/All/FAI_culture.png", plot = last_plot(), width = 10, height = 8, units = "in")





## nature
quintile <- quantile(FAI_all_final$FAI_nature, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
FAI_all_final$quintile <- cut(FAI_all_final$FAI_nature, breaks = quintile, labels = FALSE, include.lowest = TRUE)

ggplot() +
  geom_sf(data = FAI_all_final, aes(fill= quintile),
          color = "white") +
  theme_map() +
  scale_fill_continuous(name = "Quintile", type = "viridis", direction = -1) +
  
  ggtitle("Nature") +
  scale_shape_discrete()  +
  geom_sf(data = data_ring, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_gürtel, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_donau, fill = "transparent", color = "lightblue", lwd = 2.5) +
  geom_sf(data = data_lake, fill = "transparent", color = "lightblue", lwd = 1.7) +
  geom_sf(data = data_donaukanal, fill = "transparent", color = "lightblue", lwd = 3.5) +
  geom_sf(data = data_borders, fill = "transparent", color = "black", lwd = 1.5) +
  theme(legend.position = "left", legend.background = element_blank(),
        plot.title = element_text(size = 32, vjust = -1, face = "bold", family = "serif"),
        legend.text = element_text(size = 20, family = "serif"), legend.title = element_text(size = 24, face = "bold", family = "serif", vjust = 1.5)) +    
  geom_text(data = data_bez, aes(x = st_coordinates(st_centroid(data_bez))[, "X"], y = st_coordinates(st_centroid(data_bez))[, "Y"], label = BEZ), size = 5, family = "serif", fontface = "bold", color = "red")  


# save image
ggsave(file = "FAI/All/FAI_nature.png", plot = last_plot(), width = 10, height = 8, units = "in")

