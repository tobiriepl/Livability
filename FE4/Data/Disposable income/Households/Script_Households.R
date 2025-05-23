
library(tidyverse)
library(readxl)
library(mapview)


data_house_raw = read_csv("Data/Households/vie_zbez_hh_size_2012f.csv")

colnames(data_house_raw) <- data_house_raw[1, ]
data_house_raw <- data_house_raw[-1, ] |>
  filter(REF_YEAR == "2021") 

colnames(data_house_raw) <- c("NUTS", "BEZ", "ZBEZ", "REF_YEAR", "REF_DATE", "H_i", "H_ii", "H_iv", "H_vi", "ANST")

View(data_house_raw)

data_house_clean <- data_house_raw |>
  mutate( H_i = as.numeric(H_i),
          H_ii = as.numeric(H_ii),
          H_iv = as.numeric(H_iv),
          H_vi = as.numeric(H_vi)) 

data_house_clean$BEZ <- paste(substr(data_house_clean$BEZ, start = 2, stop = 3),
                              substr(data_house_clean$BEZ, start = 6, stop = 7), sep = "")

data_house_clean$ZBEZ <- paste(substr(data_house_clean$ZBEZ, start = 2, stop = 7))

data_house_clean$sum <- apply(data_house_clean[, 6:9], MARGIN = 1, FUN = sum) 


data_house_clean = data_house_clean |>
  mutate( house_avg = (1 * H_i + 2 * H_ii + 4 * H_iv + 6 * H_vi) / sum,
          hh1_share = H_i / sum,
          hh2_share = H_ii / sum, 
          hh4_share = H_iv / sum,
          hh6_share = H_vi / sum) |>
  select(BEZ, ZBEZ, REF_YEAR, H_i, hh1_share, H_ii, hh2_share, H_iv, hh4_share, H_vi, hh6_share, sum, house_avg)



View(data_house_clean)

#RELATIVES HAUSHALTSEINKOMMEN BASIEREND AUF 1 HAUSHALT
#1.912420961    2 Person   #d.h 2 person hh hat fast 2 mal so viel einkommen
#1.183654467   3 Person  #dh 3 personen haben 1.18 mal so viel einkommen wie 2 person hh
#1.10298438
#1.04516345



View(data_house_clean)

# write to disk
output_path = here("Data/Households/data_house.geojson")

# delete old version
unlink(output_path)
write_sf(data_house_clean, output_path)

