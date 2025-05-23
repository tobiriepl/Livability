
# SSI data cleaning 

#load packages

library(tidyverse)
library(readxl)
library(mapview)

#load data

data_zbez = read_sf("Data/Census Districts/ZAEHLBEZIRKOGDPolygon.shp")


# INCOME
# Die „Integrierte Lohn- und Einkommensteuerstatistik“ stellt die Einkommen aller Arbeitnehmer:innen, selbständig Erwerbstätigen sowie der Pensionist:innen dar, 
# soweit sie steuerlich erfasst sind. Zudem werden die Transferleistungen ausgewiesen (Arbeitslosengeld, Notstandshilfe, Kinderbetreuungsgeld, Pflegegeld, 
# Familienbeihilfe und diverse Beihilfen). 
# Es ergibt sich somit ein umfassendes Bild über die Einkommensverteilung sowie die Steuerleistung des jeweiligen Jahres.
#Die Einkommen der Selbständigen in der Einkommensteuerveranlagung bzw. -statistik enthalten keine Sozialversicherungsbeiträge – um die Einkommen vergleichbar zu machen, wird das Brutto der Nichtselbständigen bzw. Pensionsbezieher:innen um diese SV-Beiträge reduziert, daher handelt es sich beim Gesamteinkommen um das sog. „adaptierte Brutto“ plus Transferleistungen.

#data import and cleaning
income_raw = read_excel("FE4/Data/Disposable Income/Income/Income_Vienna.xlsx")
income_clean <- income_raw |>
  select(ZSP= "Statistik der integrierten Lohn- und Einkommensteuer 2020", observations = "...2", sum = "...3", mean = "...4", median = "...5") |>
  slice(-1,-2) |>
  filter(startsWith(ZSP, "9"))  # vienna's ZSPs start with 9, ZSP = Zählsprengel

income_clean$ZSP <- paste(substr(income_clean$ZSP, start = 2, stop = 3),
                          substr(income_clean$ZSP, start = 6, stop = 7), sep = "") 
#View(income_clean)
income_final = income_clean |>
  mutate(mean = as.numeric(mean)) |>
  aggregate(mean~ZSP, mean) |>
  select(ZBEZ = "ZSP", mean_income = "mean") 

# adjust income data from 2020 to year 2024
income_development_raw = read_csv("FE4/Data/Disposable income/Income/vie-bez-biz-ecn-inc-sex-2002f.csv")
colnames(income_development_raw) <- as.character(income_development_raw[1, ])
income_development_raw <- income_development_raw[-1, ] |>
  select(BEZ = "DISTRICT_CODE", year = "REF_YEAR", total_income = "INC_TOT_VALUE") |>
  group_by(BEZ) |>
  mutate(year = as.numeric(year),
         total_income = as.numeric(total_income))

income_development_raw$BEZ <- paste(substr(income_development_raw$BEZ, start = 2, stop = 3))

View(income_development_final)
income_development_final = income_development_raw |> 
  pivot_wider(names_from = year, values_from = total_income) |>
  mutate(average_increase = (`2021`  - `2020`) / `2020`) |> 
  filter(BEZ != "00") |>
  mutate(BEZ = as.character(BEZ))
  
  
income_merged = right_join(income_final, data_zbez, join_by(ZBEZ == ZBEZ)) |>
  select(ZBEZ, BEZ, mean_income, geometry) 

income_final_adjusted = merge(income_merged, income_development_final, by = "BEZ")  |>
  mutate(mean_income_adj = mean_income * (1 + average_increase) * (1 + average_increase) * (1 + average_increase) * (1 + average_increase)) |>  # 2020 income updated to 2023
  select(ZBEZ, mean_income, mean_income_adj, geometry)


#View(income_development_final)
# write to disk
output_path = here("FE4/Data/Disposable income/Income/data_income_adjusted.geojson")    # original income dataset only already adjusted
unlink(output_path)
write_sf(income_final_adjusted, output_path)   



