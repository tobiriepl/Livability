## R Code: mapping the Foundational Infrastructure in Vienna

library(sf) # mittlerweile standard paket für vektor geodaten in R    # spatial formation package 
library(tidyverse)
library(mapview) # interkative, schnelle karten



#read raw population data and census district data
# population
population_vienna_raw = read_csv2("Data/Population/Population_Vienna.csv")

View(population_vienna_raw)
population_vienna_final <- population_vienna_raw |>
  filter(REF_YEAR == 2022)  |>
  mutate(POP_TOTAL = AUT + FOR) |>
  aggregate(POP_TOTAL~SUB_DISTRICT_CODE, sum) |>
  mutate(SUB_DISTRICT_CODE = as.character(SUB_DISTRICT_CODE)) 
View(population_vienna_final)
colnames(population_vienna_final) <- c("ZBEZ", "POP_TOTAL")

#if I was to remove 0 values in order to end up with 250 rows manually
#but not needed because merge function will do it
#population_vienna_final <-  population_vienna_final |>
#  mutate(across(everything(), ~if_else(row_number() == 18, ., replace(., . == 0, NA)))) |>
#  drop_na()

population_vienna_final$ZBEZ <- substring(population_vienna_final$ZBEZ, 2)


#census districts
census_districts_vienna = read_sf("Data/Census Districts/ZAEHLBEZIRKOGDPolygon.shp")   #  census district data from city of Vienna
 
View(population_vienna_final)

data_pop <- merge(population_vienna_final, census_districts_vienna, by = "ZBEZ") |>
  select(ZBEZ, BEZ, POP_TOTAL, geometry)

View(data_pop)


# write to disk
output_path = here("Data/Population/data_pop.geojson")

# delete old version
unlink(output_path)
write_sf(data_pop, output_path)


percentile_5 <- quantile(data_pop$POP_TOTAL, probs = 0.05)
data_pop_filtered <- data_pop |>
  filter(POP_TOTAL > percentile_5)


# write to disk
output_path = here("Data/Population/data_pop_filtered.geojson")

# delete old version
unlink(output_path)
write_sf(data_pop_filtered, output_path)
