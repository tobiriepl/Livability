
# SSI data cleaning 

#load packages

library(tidyverse)
library(readxl)
library(mapview)

#load data

data_zbez = read_sf("City data/Census Districts/ZAEHLBEZIRKOGDPolygon.shp")


# UNEMPLOYMENT
unemployment_raw = read_excel("City data/SSI/Unemployment_Vienna.xlsx") 
unemployment_clean <- unemployment_raw |>
  select(ZBEZ = "Zählbezirk", non_employable = "Nicht-Erwerbspersonen", unemployed = "arbeitslos", employed = "erwerbstätig")

unemployment_clean$ZBEZ <- paste(substr(unemployment_clean$ZBEZ, start = 2, stop = 3),
                                 substr(unemployment_clean$ZBEZ, start = 6, stop = 7), sep = "")


# merge unemployment data with data_bez adding geospatial information to the data
unemyploment_merged = right_join(data_zbez, unemployment_clean, join_by(ZBEZ == ZBEZ)) |>
  select(BEZ, ZBEZ, non_employable, employed, unemployed, geometry)

View(unemyploment_merged)

# write to disk
output_path = here("City data/SSI/data_unemp.geojson")

# delete old version
unlink(output_path)
write_sf(unemyploment_merged, output_path)


# GRADUATION

graduation_raw = read_excel("City data/SSI/Graduation_Vienna.xlsx") 

graduation_clean <- graduation_raw |>
  select(ZBEZ = "Zählbezirk", compulsory_school = "Pflichtschule (inkl. kein Abschluss)", apprenctiship = "Lehrabschluss", secondary_school = "Mittlere und Höhere Schule", university = "Hochschule und Akademie") 
  
graduation_clean$total <- apply(graduation_clean[, 2:5], MARGIN = 1, FUN = sum)   # margin indicates that function is applied row-wise, margin 2 is column-wise

graduation_clean$ZBEZ <- paste(substr(graduation_clean$ZBEZ, start = 2, stop = 3),
                               substr(graduation_clean$ZBEZ, start = 6, stop = 7), sep = "")

graduation_clean = graduation_clean |>
  mutate(university_share = university / total)
View(graduation_clean)


# merge unemployment data with data_bez adding geospatial information to the data
graduation_merged = right_join(data_zbez, graduation_clean, join_by(ZBEZ == ZBEZ)) |>
  select(BEZ, ZBEZ, compulsory_school, apprenctiship, secondary_school, university, total, university_share, geometry) 

View(graduation_merged)

# write to disk
output_path = here("City data/SSI/data_grad.geojson")

# delete old version
unlink(output_path)
write_sf(graduation_merged, output_path)



# INCOME
# Die „Integrierte Lohn- und Einkommensteuerstatistik“ stellt die Einkommen aller Arbeitnehmer:innen, selbständig Erwerbstätigen sowie der Pensionist:innen dar, 
# soweit sie steuerlich erfasst sind. Zudem werden die Transferleistungen ausgewiesen (Arbeitslosengeld, Notstandshilfe, Kinderbetreuungsgeld, Pflegegeld, 
# Familienbeihilfe und diverse Beihilfen). 
# Es ergibt sich somit ein umfassendes Bild über die Einkommensverteilung sowie die Steuerleistung des jeweiligen Jahres.
#Die Einkommen der Selbständigen in der Einkommensteuerveranlagung bzw. -statistik enthalten keine Sozialversicherungsbeiträge – um die Einkommen vergleichbar zu machen, wird das Brutto der Nichtselbständigen bzw. Pensionsbezieher:innen um diese SV-Beiträge reduziert, daher handelt es sich beim Gesamteinkommen um das sog. „adaptierte Brutto“ plus Transferleistungen.

income_raw = read_excel("City data/SSI/Income_Vienna.xlsx")
income_clean <- income_raw |>
  select(ZSP= "Statistik der integrierten Lohn- und Einkommensteuer 2020", observations = "...2", sum = "...3", mean = "...4", median = "...5") |>
  slice(-1,-2) |>
  filter(startsWith(ZSP, "9"))  # vienna's ZSPs start with 9, ZSP = Zählsprengel

income_final = income_clean
income_final$ZSP <- paste(substr(income_final$ZSP, start = 2, stop = 3),
                          substr(income_final$ZSP, start = 6, stop = 7), sep = "") 
income_final = income_final |>
  mutate(mean = as.numeric(mean)) |>
  aggregate(mean~ZSP, mean) |>
  select(ZBEZ = "ZSP", avg_income = "mean")

income_merged = right_join(income_final, data_zbez, join_by(ZBEZ == ZBEZ)) |>
  select(ZBEZ, avg_income, geometry)
# View(income_merged)

# write to disk
output_path = here("City data/SSI/data_inc.geojson")

# delete old version
unlink(output_path)
write_sf(income_merged, output_path)

