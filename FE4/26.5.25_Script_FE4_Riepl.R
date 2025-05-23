

## FE4 metric

# load packages
library(sf) 
library(osmdata)
library(tidyverse)
library(mapview) 
library(ggthemes) 
library(here)
library(glue)
library(jsonlite)
library(hereR)
library(readr)
library(ggthemes)  
library(openxlsx)
library(readxl)
library(stringr)
library(tidygeocoder)



# load data
data_zbez = read_sf("City data/Census Districts/ZAEHLBEZIRKOGDPolygon.shp")
data_bez = read_sf("City data/Districts/data_bez.geojson")
data_ring = read_sf("City data/Aesthetics/data_ring.geojson")
data_gürtel = read_sf("City data/Aesthetics/data_gürtel.geojson")
data_borders = read_sf("City data/Aesthetics/data_borders.geojson")
data_donau = read_sf("City data/Aesthetics/data_donau.geojson")
data_donaukanal = read_sf("City data/Aesthetics/data_donaukanal.geojson")
data_lake = read_sf("City data/Aesthetics/data_lake.geojson")

data_household = read_sf("FE4/Data/Disposable Income/Households/data_house.geojson")
data_pop = read_sf("City data/Population/data_pop.geojson")
percentile_5 <- quantile(data_pop$POP_TOTAL, probs = 0.05)
data_pop_filtered <- data_pop |>
  filter(POP_TOTAL > percentile_5)



# FE4 calculation: disposable income - housing costs - utility costs - food costs - transportation costs. 
# three ways of calculating residual income 

# FIRST OPTION (article option). calculating income and costs for each different household size and then averaging the results by using the share of households per census district
data_hsize_raw = read_excel("FE4/Data/Housing/Registerzählung/vie_404_adjusted.xlsx")
data_htype_raw = read_csv("FE4/Data/Housing/Wohnungsgebietstypen/wohngebietstypen-2016.csv")

# disposable income
data_dispo_hh = read_sf("FE4/Data/Disposable Income/Income/data_income_hh_avg.geojson") |>
  select(BEZ, "dispo_hh_avg" = hh_income_avg, geometry )

output_path = here("FE4/Disposable Income/data_dispo_hh.geojson")
unlink(output_path) # delete old version
write_sf(data_dispo, output_path)



#housing 
data_hprice_raw = read_excel("FE4/Data/Housing/Mietpreise/Mietpreise2022_2023_Immobilienscout24_adjusted.xlsx")
data_hprice_final = data_hprice_raw |>
  select(BEZ, "price" = "2023")
data_hprice_final$BEZ = c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23")
data_hprice_final$price = as.numeric(data_hprice_final$price)
write.xlsx(data_hprice_final, "FE4/Data/Housing/data_hprice.xlsx")
data_hprice = data_hprice_final

data_hspace_raw = read_csv2("FE4/Data/Housing/Wohnfläche/Wohnfläche_adjusted.csv")
colnames(data_hspace_raw) <- c("BEZ", "1981","1991", "2001", "2011", "2021a", "1981", "1991", "2001", "2021", "2021b")
data_hspace_raw <- data_hspace_raw[-c(1:3), ]
data_hspace_final = data_hspace_raw |>
  select(BEZ, "average_space_h" ="2021a", "average_space_pp" ="2021b") |>
  slice(-1,-25,-26,-27) 
data_hspace_final$BEZ = c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23")

write.xlsx(data_hspace_final, "FE4/Data/Housing/data_hspace.xlsx")
data_hspace = data_hspace_final
#View(data_hspace_final)
#View(data_household)

#View(data_rent_pre)
data_rent_pre = merge(data_hprice |> st_drop_geometry(), 
                      data_household |> st_drop_geometry() |>
                        group_by(BEZ) |>
                        summarize(household_avg = mean(house_avg, na.rm = TRUE)) , by = "BEZ") 

data_rent = merge(data_rent_pre, data_hspace, by = "BEZ") |>
  mutate(housing_avg = price * average_space_pp * household_avg) |>  # house_avg: how many people per household
  select(BEZ, housing_avg)
  
 
#View(data_rent)
output_path = here("FE4/Housing/data_rent_hh.geojson")
unlink(output_path) # delete old version
write_sf(data_rent, output_path)



# utilities: 
# zbez in baujahr einteilen
data_hyear_processed = data_hsize_raw |>
  select("BEZ" = DISTRICT_CODE, "ZBEZ" = SUB_DISTRICT_CODE, "Prior to 1919" = OBJ_BAUP_1, "1919-1944" = OBJ_BAUP_2, "1945-1960"=OBJ_BAUP_3, "1961-1970"=OBJ_BAUP_4,"1971-1980" = OBJ_BAUP_5, "1981-1990" = OBJ_BAUP_6, "1991-2000"=OBJ_BAUP_7, "2001-2005"=OBJ_BAUP_8, "2006-now"=OBJ_BAUP_9) |>
  group_by(ZBEZ, BEZ)

#View(data_hyear_processed)

#data_hyear_processed

data_hyear_processed$BEZ <- paste(substr(data_hyear_processed$BEZ, start = 2, stop = 3))
data_hyear_processed$ZBEZ <- paste(substr(data_hyear_processed$ZBEZ, start = 2, stop = 5))

data_hyear_merged <- data_hyear_processed |>
  group_by(ZBEZ, BEZ) |>
  summarize(`Before 1960` = sum(`Prior to 1919`, `1919-1944`, `1945-1960`),
            `1961-1980`= sum(`1961-1970`, `1971-1980`), 
            `1981-2005`= sum(`1981-1990`, `1991-2000`, `2001-2005`),
            `After 2006` = `2006-now`)

data_hyear_merged$class <- ""

# Iterate through each row
for (i in 1:nrow(data_hyear_merged)) {
  max_index <- which.max(data_hyear_merged[i, -1])  # Get the column index with the maximum value excluding the 'zbez' column
  data_hyear_merged$class[i] <- names(data_hyear_merged)[max_index + 1]  #  # Assign the classification based on the column index and Add 1 to skip 'zbez' column
}
data_hyear_merged$class = as.factor(data_hyear_merged$class)

data_hyear_assigned <- data_hyear_merged |>
  mutate(efficiency = case_when(
    class == "Before 1960" ~ (250/12),   # angaben von michi (kWh/m² per month). durch 12 um auf monatswerte zu kommen
    class == "1961-1980" ~ (150/12),     # 
    class == "1981-2005" ~ (100/12),
    class == "After 2006" ~ (50/12),
    TRUE ~ NA_integer_ ))  |>    # get for bez and mean
  group_by(BEZ) |>
  summarize(efficiency_avg = mean(efficiency, na.rm = TRUE))



data_utiliti  =  merge(data_hyear_assigned, data_hspace_final, by = "BEZ")      
data_utilities = merge(data_utiliti, data_household, by = "BEZ") |>
  select(BEZ, house_avg, efficiency_avg, average_space_h, average_space_pp, geometry)   |>
  group_by(BEZ) |>
  mutate(household_avg = mean(house_avg, na.rm = TRUE)) |>
  ungroup() |>
  distinct(BEZ, .keep_all = TRUE) |>
  mutate(
    elec_cost = case_when(     
      household_avg < 1.5 ~ (2225/12) * 0.271,  #  average electricty price per kWh: 0.271 (Statistik Austria)   
      household_avg >= 1.5 & household_avg < 2.5 ~ (3574/12) * 0.271,   # this value from https://www.wien.gv.at/spezial/energiebericht/energie-von-der-gewinnung-bis-zur-nutzung/energieverbrauch-eines-wiener-haushalts/   
      household_avg >= 2.5 ~ (4914/12) * 0.271),                    # other values based on city data but using a electricty consumption ratio from e-control (e.g., how much 1 person household consumes electricty, how much a 2-person household)
  
     gas_cost = efficiency_avg  * 0.165 * average_space_h,     #  average gas price per kWh: 0.165 (Statistik Austria) https://www.statistik.at/en/statistics/energy-and-environment/energy/energy-prices-taxes
                                                   # avg floor space households
         
     int_cost = 30)     |>                # internet cost based on: https://www.drei.at/de/shop/tarife/privat/internet-tarife/tarife-fuer-zuhause/
   mutate(utilities_cost = elec_cost + gas_cost + int_cost) |>
   select(BEZ, utilities_cost, elec_cost, gas_cost,  int_cost)
  
#View(data_utilities)
output_path = here("FE4/Utilities/data_utilities_hh.geojson")
unlink(output_path) # delete old version
write_sf(data_utilities, output_path)


# transport
data_transport = data_dispo_hh |>
  mutate(transport_cost = dispo_hh_avg *0.139) |>
  select(BEZ, transport_cost)

#View(data_transport)

output_path = here("FE4/Transport/data_transport_hh.geojson")
unlink(output_path) # delete old version
write_sf(data_transport, output_path)

# food
data_food = data_dispo_hh |>
  mutate(food_cost = dispo_hh_avg * 0.118) |>
  select(BEZ, food_cost)

output_path = here("FE4/Food/data_food_hh.geojson")
unlink(output_path) # delete old version
write_sf(data_food, output_path)


# RESIDUAL INCOME 
data_resid_pre = merge(data_dispo_hh, data_rent |> st_drop_geometry(), by = "BEZ") 

data_resid_pr = merge(data_resid_pre, data_utilities |> st_drop_geometry(), by = "BEZ") 
data_resid_p = merge(data_resid_pr, data_transport |> st_drop_geometry(), by = "BEZ") 
data_resid = merge(data_resid_p, data_food |> st_drop_geometry() , by = "BEZ") |>
  mutate(resid_inc_avg = dispo_hh_avg -  housing_avg - utilities_cost - transport_cost - food_cost,
         total_cost = housing_avg + utilities_cost + transport_cost + food_cost) |>
  mutate(resid_inc_avg_mean = mean(resid_inc_avg, na.rm = T),   # to get the FAI standardized: mean, sd and z variation
         resid_inc_avg_sd = sd(resid_inc_avg, na.rm = T), 
         resid_inc_avg_final = (resid_inc_avg - resid_inc_avg_mean) / resid_inc_avg_sd) |>
  select(BEZ, resid_inc_avg, "resid_inc_avg_sd" = resid_inc_avg_final, "dispo_inc" = dispo_hh_avg, total_cost, "housing_cost" = housing_avg, utilities_cost, transport_cost, food_cost)

#View(data_resid)

data_fe4 = merge(data_bez, data_resid |> st_drop_geometry(),  by = "BEZ") |>
  select(-name, -DISTRICT_CODE)

#View(data_fe4)

#View(data_fe4)
quintile <- quantile(data_fe4$resid_inc_avg, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
# quintile <- quintile + seq(0, 0.001, length.out = length(quintile))
data_fe4$quintile <- cut(data_fe4$resid_inc_avg, breaks = quintile, labels = FALSE, include.lowest = TRUE)


# write to disk
output_path = here("FE4/Overview/data_FE4.geojson")
# delete old version
unlink(output_path)
write_sf(data_fe4, output_path)


#View(data_fe4)

#visualization 

ggplot() +
  geom_sf(data = data_fe4, aes(fill= quintile),
          color = "white") +
  theme_map() +
  scale_fill_continuous(type = "viridis",
                        direction = -1) +
  ggtitle("Residual income") +
  scale_shape_discrete()  +
  geom_sf(data = data_ring, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_gürtel, fill = "transparent", color = "black", lwd = 3) +
  geom_sf(data = data_donau, fill = "transparent", color = "lightblue", lwd = 2.5) +
  geom_sf(data = data_lake, fill = "transparent", color = "lightblue", lwd = 1.7) +
  geom_sf(data = data_donaukanal, fill = "transparent", color = "lightblue", lwd = 3.5) +
  geom_sf(data = data_borders, fill = "transparent", color = "black", lwd = 1.5) +
  theme(legend.position = "left", plot.title = element_text(size = 32, hjust = -0.03, vjust = -10, face = "bold", family = "serif"), 
        legend.text = element_text(size = 20, family = "serif"), legend.title = element_text(size = 24, face = "bold", family = "serif"),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA)) +    
  geom_text(data = data_bez, aes(x = st_coordinates(st_centroid(data_bez))[, "X"], y = st_coordinates(st_centroid(data_bez))[, "Y"], label = BEZ), size = 5, family = "serif", fontface = "bold", color = "red") +
  guides(fill = guide_legend(title = "Quintile")) 
 # geom_sf(data = data_zbez, fill = NA, color = "grey", size = 0.2)


#View(data_FE4_A)
ggsave(file = "FE4/Overview/FE4_Avg_HH.png", plot = last_plot(), width = 10, height = 8, units = "in")


# calculations for article (using first option to estimate residual income)
# Residual Income table 

FE4_table = data_fe4 |>
  group_by(BEZ) |>    # get summary statistic per bez 
  st_drop_geometry() |>
  
  mutate( 
    total_cost =   housing_cost + utilities_cost + transport_cost +  food_cost, 
    share_housing_dispo = housing_cost / dispo_inc,
    share_housing_cost = housing_cost / total_cost) |>
  select(BEZ, quintile, resid_inc_avg, dispo_inc, total_cost, everything())



# write to disk
output_path = here("FE4/Overview/FE4_table.csv")
# delete old version
unlink(output_path)
write_csv(FE4_table, output_path)
write.xlsx(FE4_table, "FE4/Overview/FE4_table.xlsx")


#View(FE4_table)
















