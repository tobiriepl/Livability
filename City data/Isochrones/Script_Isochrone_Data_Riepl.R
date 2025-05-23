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


#read census district data for vienna and centroids
census_districts_vienna = read_sf("Data/Census Districts/ZAEHLBEZIRKOGDPolygon.shp")   #  census district data from city of Vienna
  

# get neighborhood isochrones ------------------------------------------------------------
apiKey = readLines(here("Data/API/api.txt"))   # alternative: apiKey = "0kOQyCByMkYR55j-46uA1tTpuryTSjnT4lI3vj7TvwY"

# urls ------------------------------------------------------
base_here_url = glue("https://isoline.route.ls.hereapi.com/routing/7.2/calculateisoline.json?apiKey={apiKey}")

data_iso = read_sf(here("Data/Isochrones/Isochrones.geojson")) 

mapview(data_iso)

# get 15min isochrones for census district centroids---------------------------------------------------------------
cents = st_centroid(census_districts_vienna)
isochrones = c(15)

# set api key
hereR::set_key(apiKey)

# setup array for sroting isochrons

isochrone_calcuation = map(1:nrow(cents), function(j){
  
  cat(glue("{j} / 250"), "\r")
  
  row = cents[j, ] # j is the row index
  
  times = isochrones *60 # time in seconds
  
  res = hereR::isoline(        # use hereR package to send API request
    row,
    datetime = Sys.time(),
    range = times,
    range_type = "time",
    transport_mode = "pedestrian"
  )
  
  res[["zbez"]] = row$ZBEZNR
  
  # sleep to not get banned from API
  Sys.sleep(1)
  
#   #save on disc done later because faster: write_sf(res, "Data/Isochrones/test.geojson")
  
  return(res)
  
})

View(culture_points)

# save isochrone calculation 
# saveRDS(isochrone_calcuation, file = "Data/Isochrones/Isochrones15min_list.rds")


# bind and save isochrone dataset with write_sf(res, path) 
all_isochrones = bind_rows(isochrone_calcuation) 

#  mapview(all_isochrones)


# write to disk
output_path = here("Data/Isochrones/Isochrones30min.geojson")

# delete old version
unlink(output_path)
write_sf(all_isochrones, output_path)

mapview(all_isochrones)

# to clean isochrone data  ---------------------------------------------------------------

check <- st_read("Data/Isochrones/test.geojson")

mapview(check)

isochrones_20min <- st_read("Data/Isochrones/Isochrones20min.geojson")
isochrones_20min_final <- isochrones_20min |> 
  rownames_to_column(var = "ZBEZ") |>
  select(ZBEZ, )
View(isochrones_20min_final)




