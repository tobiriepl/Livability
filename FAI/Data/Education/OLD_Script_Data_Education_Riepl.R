## R Code: mapping the Foundational Infrastructure in Vienna

library(sf) # mittlerweile standard paket für vektor geodaten in R    # spatial formation package 
library(osmdata)
library(tidyverse)
library(mapview) # interkative, schnelle karten
library(ggthemes) # pretty ggplots

library(here)
library(glue)
# library(rajudas) 
library(jsonlite)
library(hereR)
library(readr)



#read raw data
census_districts_vienna = read_sf("Data/Census Districts/ZAEHLBEZIRKOGDPolygon.shp")   #  census district data from city of Vienna
  
schools_official = read_sf("Data/Education/Schools/SCHULEOGDPoint.shp")  # official data on schools from the City of Vienna
schools_official_clean <- schools_official |>
  select(name = "NAME") |>
  mutate(amenity = "school")


#get ohter education data from osm since no more official data: library, university
# overpass query = API (application programming interface) von OSM
vienna_bb = osmdata::opq(st_bbox(census_districts_vienna))   # bb bounding box 

library_osm = vienna_bb |>
  osmdata::add_osm_feature(key="amenity", value="library") |>
  osmdata_sf()  

library_osm = library_osm$osm_points  # get osm data points, data clean, no spatial cleaning needed for libraries
library_osm_clean <- library_osm |>
  filter(!is.na(name)) |>
  mutate(amenity = "library") |>
  select(name, amenity, geometry)

add_education_osm = vienna_bb |>
  osmdata::add_osm_feature(key="amenity", value=c("college", "language_school", "training")) |>   #music school excluded
  osmdata_sf()  

add_education_osm = add_education_osm$osm_points  # get osm data points, data clean, no spatial cleaning needed for libraries
add_education_osm_clean <- add_education_osm |>
  filter(!is.na(name)) |>
  select(name, amenity, geometry)  # polygons not required since no entry

View(add_education_osm_clean)
university_osm = vienna_bb |>
  osmdata::add_osm_feature(key="amenity", value="university") |>
  osmdata_sf()  

university_osm_points <- university_osm$osm_points |>    # do first cleaning of data
  filter(!is.na(name)) |>
  mutate(amenity = "university") |>
  select(name, amenity, geometry)


# get rid of overlapping university points (e.g., same building) 

university_osm_polygons = university_osm$osm_polygons |>    # creates polygons for centroids
  filter(!is.na(name)) |>
  mutate(amenity = "university") |>
  select(name, amenity, geometry)

university_osm_centroids = st_centroid(university_osm_polygons) 



# punkte auf dem rand eines polygons
points_that_touch = st_touches(university_osm_points, university_osm_polygons)
points_that_are_within =  st_within(university_osm_points, university_osm_polygons)

 # make binary version
touches_binary = lengths(points_that_touch) > 0
within_binary = lengths(points_that_are_within) > 0

university_osm_points[["touches_polygon"]] = touches_binary
university_osm_points[["within_polygon"]] = within_binary

# wenn einer von beiden wahr ist, dann punkt rauswerfen
university_osm_points <- university_osm_points %>%
  mutate(
      punkt_behalten = case_when(
      !touches_polygon & !within_polygon  ~ TRUE,    ## why not working??
      .default =  FALSE
    )
  ) 

# delete points
university_osm_points_clean = university_osm_points %>%
  filter(punkt_behalten)


# how to get rid off polygons within polygons
# polygons_that_are_within = st_within(university_osm_polygons, university_osm_polygons)
# within_within_binary = lengths(polygons_that_are_within) > 0    
# university_osm_polygons[["within_within_polygon"]] = within_within_binary

# university_osm_polygons <- university_osm_polygons %>%
# mutate(
#  punkt_behalten = case_when(
#    !within_within_polygon ~ TRUE,    ## why not working??
#    .default =  FALSE
#  )
# ) 

#university_osm_polygons_clean = university_osm_polygons |>
# filter(punkt_behalten)


# data preparation for visualization --------------------------------------

# university data zusammenfügen
university_osm_clean <- bind_rows(university_osm_points_clean, university_osm_centroids)

# merge data and remove duplicates --------------------------------------------------------------
education_data_merged <- bind_rows(schools_official_clean, library_osm_clean, university_osm_clean, add_education_osm_clean) |>
    select(name, amenity, geometry)

education_data_unique <- education_data_clean[!duplicated(education_data_clean$name), ]

#education points 
education_data_final <- st_join(education_data_unique, census_districts_vienna, join = st_within) |>   
  filter(!is.na(ZBEZ)) |>    # to cut providers not in vienna (they have no ZBEZ data)
  select(name, amenity, ZBEZ, geometry) |>
  group_by(ZBEZ) 

mapview(education_data_final)
write_sf(education_data_final, "Data/education/education_points.geojson")






