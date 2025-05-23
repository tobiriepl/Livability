## R Code: mapping the Foundational Infrastructure in Vienna

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
library(ggthemes)   # to get theme map from tom


# data --------------------------------------------------------------------
post_points = read_sf("FAI/Data/Post/post_points.geojson")  
data_zbez = read_sf("City data/Census Districts/ZAEHLBEZIRKOGDPolygon.shp")
data_bez = read_sf("City data/Districts/data_bez.geojson")
data_iso = read_sf(here("City data/Isochrones/Isochrones15min.geojson")) 
data_pop = read_sf("City data/Population/data_pop.geojson") |>
  group_by(BEZ) |>
  summarize(POP_TOTAL = sum(POP_TOTAL, na.rm = TRUE))
data_ring = read_sf("City data/Aesthetics/data_ring.geojson")
data_gürtel = read_sf("City data/Aesthetics/data_gürtel.geojson")
data_borders = read_sf("City data/Aesthetics/data_borders.geojson")
data_donau = read_sf("City data/Aesthetics/data_donau.geojson")
data_donaukanal = read_sf("City data/Aesthetics/data_donaukanal.geojson")
data_lake = read_sf("City data/Aesthetics/data_lake.geojson")

# post data of neighborhoods, saved on disc ----------------------------------------------------------

data_post = merge(data_zbez, post_points |> st_drop_geometry(), by = "ZBEZ") |>
  select(BEZ, name, type, geometry)

#View(data_post)

result = map(1:23, function(i){  # für jeden zbez
  
  cat(glue(i, "/", 23, "\r"))
  
  iso = data_iso[i, ]
  
  bez = data_bez[i, ]
  
  # find points in zbez, similar to: st_intersection(data_points, zbez)
  post_in_bez = data_post[bez, ] %>%
    mutate(inBez = TRUE)
  
  # find points in isochrone
  post_in_isochrone = data_post[iso, ] %>%
    mutate(inBez = FALSE)
  
  #  in isochrone, aber nicht in zbez
  post_in_iso_but_no_bez = anti_join(post_in_isochrone, post_in_bez %>% st_drop_geometry(), join_by(name))
  
  # in bez + in isochrone (aber nicht in bez)
  post_iso_bez = bind_rows(post_in_bez, post_in_iso_but_no_bez)
  
  
  # um welchen zbez handelt es sich
  post_iso_bez[["bez"]] = bez[["BEZ"]]
  
  # weights
  post_iso_bez |>
    st_drop_geometry() |>
    group_by(bez, inBez, type) |>
    summarise(
      n_amenities = n()
    ) |>
    pivot_wider(        # make long to wide
      names_from = c("inBez", "type"),
      values_from = "n_amenities"
    ) -> final
  
  return(final)
  
})

# alle Zählbezirke zusammenschreiben
all_results = result |> bind_rows() |>
  arrange(bez)  



# append the ZBEZ number of the missing ones
# the ones filtered out above, will have 0s in all columns
all_results_final = right_join(all_results, data_bez, join_by(bez == BEZ)) |>
  select( "BEZ" = bez, "Post_office_Iso" = "FALSE_post office", 
          "Post_office_Bez" = "TRUE_post office") 

#data cleaning and FAI preparation (for weightening + per capita)
all_results_final[is.na(all_results_final)] <- 0   # to replace NAs with 0 for FAI calculation



#  FAI calculation
FAI_post <- merge(all_results_final, data_pop, by="BEZ") |> 
  distinct(BEZ, .keep_all = TRUE) |>
  
  mutate( FAI_Bez = Post_office_Bez,     # no weighting of different amenities
          FAI_Iso = Post_office_Iso, # no weighting of neighboring area
          FAI_total = (FAI_Bez + FAI_Iso) / POP_TOTAL,    # FAI per capita 
          FAI_mean = mean(FAI_total, na.rm = T),   # to get the FAI standardized: mean, sd and z variation
          FAI_sd = sd(FAI_total, na.rm = T), 
          FAI_final = (FAI_total - FAI_mean) / FAI_sd) |> 
  select(BEZ, FAI_final, geometry)  |>
  st_as_sf()

quintile <- quantile(FAI_post$FAI_final, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
quintile <- quintile + seq(0, 0.001, length.out = length(quintile))
FAI_post$quintile <- cut(FAI_post$FAI_final, breaks = quintile, labels = FALSE, include.lowest = TRUE)

# write to disk
output_path = here("FAI/Essentials/Post/FAI_post.geojson")
# delete old version
unlink(output_path)
write_sf(FAI_post, output_path)



