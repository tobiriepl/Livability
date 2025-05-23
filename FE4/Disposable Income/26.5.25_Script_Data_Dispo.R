## diposable household income

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

# load data
data_zbez = read_sf("City data/Census Districts/ZAEHLBEZIRKOGDPolygon.shp")
data_bez = read_sf("City data/Districts/data_bez.geojson")
data_ring = read_sf("City data/Aesthetics/data_ring.geojson")
data_gürtel = read_sf("City data/Aesthetics/data_gürtel.geojson")
data_borders = read_sf("City data/Aesthetics/data_borders.geojson")
data_donau = read_sf("City data/Aesthetics/data_donau.geojson")
data_donaukanal = read_sf("City data/Aesthetics/data_donaukanal.geojson")
data_lake = read_sf("City data/Aesthetics/data_lake.geojson")

data_household = read_sf("FE4/Data/Disposable income/Households/data_house.geojson")
data_pop = read_sf("City data/Population/data_pop.geojson")

data_income_raw = read_sf("FE4/Data/Disposable income/Income/data_income_adjusted.geojson")  # adjusted data means data from 2020 is adjusted to yearly increases over the last years
                                                              # average income data of employee for each zbez
# View(data_household)
## calculation of net income after tax
# Die „Integrierte Lohn- und Einkommensteuerstatistik“ stellt die Einkommen aller Arbeitnehmer:innen, selbständig Erwerbstätigen sowie der Pensionist:innen dar, 
# soweit sie steuerlich erfasst sind. Zudem werden die Transferleistungen ausgewiesen (Arbeitslosengeld, Notstandshilfe, Kinderbetreuungsgeld, Pflegegeld, Familienbeihilfe und diverse Beihilfen). 
# Es ergibt sich somit ein umfassendes Bild über die Einkommensverteilung sowie die Steuerleistung des jeweiligen Jahres.
# Die Einkommen der Selbständigen in der Einkommensteuerveranlagung bzw. -statistik enthalten keine Sozialversicherungsbeiträge – um die Einkommen vergleichbar zu machen, wird das Brutto der Nichtselbständigen bzw. Pensionsbezieher:innen um diese SV-Beiträge reduziert, daher handelt es sich beim Gesamteinkommen um das sog. „adaptierte Brutto“ plus Transferleistungen.

calculate_net_income <- function(income) {
  if (is.na(income)) return(NA)
  
  original_income <- income  # Save this to subtract from later
  tax <- 0
  
  # Bracket 7: Over 1,000,000 EUR – 55%
  if (income > 1e6) {
    tax <- tax + (income - 1e6) * 0.55
    income <- 1e6
  }
  # Bracket 6: 90,001 – 1,000,000 EUR – 50%
  if (income > 90000) {
    tax <- tax + (income - 90000) * 0.50
    income <- 90000
  }
  # Bracket 5: 60,001 – 90,000 EUR – 48%
  if (income > 60000) {
    tax <- tax + (income - 60000) * 0.48
    income <- 60000
  }
  # Bracket 4: 31,001 – 60,000 EUR – 42%
  if (income > 31000) {
    tax <- tax + (income - 31000) * 0.42
    income <- 31000
  }
  # Bracket 3: 18,001 – 31,000 EUR – 35%
  if (income > 18000) {
    tax <- tax + (income - 18000) * 0.35
    income <- 18000
  }
  # Bracket 2: 11,001 – 18,000 EUR – 20%
  if (income > 11000) {
    tax <- tax + (income - 11000) * 0.20
    income <- 11000
  }
  
  # Bracket 1: 0 – 11,000 EUR – 0% (no tax)
  
  # Corrected: subtract tax from original income
  net_income <- original_income - tax
  return(net_income)
}

# apply
data_income_raw$net <- sapply(data_income_raw$mean_income_adj, calculate_net_income)


data_income_clean = merge(data_income_raw, data_pop_filtered |> st_drop_geometry(), by="ZBEZ") |>
  mutate(monthly_income_mean = net/12) |>   #monthly_income_mean uses adjusted income data
  select(-POP_TOTAL)    # average income of employee for each zbez with more than 1000 inhabitants

quintile <- quantile(data_income_clean$net, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
data_income_clean$quintile <- cut(data_income_clean$net, breaks = quintile, labels = FALSE, include.lowest = TRUE)


# calculation of household income: income avg * income distribution
data_income_hh = merge(data_income_clean, data_household |> select(-BEZ) |> st_drop_geometry(), by = "ZBEZ") |>
  
  mutate(hh_income = monthly_income_mean * house_avg) |>
  group_by(BEZ) |>
  summarize(hh_income_avg = mean(hh_income, na.rm = TRUE))

#View(data_income_hh)
output_path = here("FE4/Data/Disposable income/Income/data_income_hh_avg.geojson")
unlink(output_path) # delete old version
write_sf(data_income_hh, output_path)



