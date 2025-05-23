

## cleaning scraped data by tatjana

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



test = load("FE4/Data/Housing/Scraping/Archiv/willhaben_rawdata_2021_05_17.RData")
test = save.res 

pattern1 <- "gasse|Gasse|straße|strasse|Straße|Strasse|Promenade|promenade"
pattern2 =  "01. Bezirk, |02.Bezirk, |03. Bezirk, "
  
  
test_filtered <- test |>
  filter(str_detect(Gemeinde, "Wien"))  |>
  drop_na()  |>
  mutate( Preis = gsub("\\.", "", Preis), # Remove all "."
          Preis = gsub(",", ".", Preis))   |>    # Replace "," with "." 
  mutate(Preis = as.numeric(Preis),
         Flaeche = as.numeric(Flaeche)) |>
  mutate("P/m2" = as.numeric(Preis)/as.numeric(Flaeche))  |>
  filter(str_detect(Gemeinde, pattern1),
         nchar(Gemeinde) >= 35)      # remove all rows where no street is mentioned (e.g., by filtering rows where Gemeinde has more than 35 characters)

test_test = test_filtered |>
  mutate(Gemeinde = gsub("01. Bezirk, Innere Stadt, ", "", Gemeinde),
         Gemeinde = gsub("02. Bezirk, Leopoldstadt, ", "", Gemeinde),
         Gemeinde = gsub("03. Bezirk, Landstraße,", "", Gemeinde),
         Gemeinde = gsub("04. Bezirk, Wieden,", "", Gemeinde),
         Gemeinde = gsub("05. Bezirk, Margareten,", "", Gemeinde),
         Gemeinde = gsub("06. Bezirk, Mariahilf,", "", Gemeinde),
         Gemeinde = gsub("07. Bezirk, Neubau,", "", Gemeinde),
         Gemeinde = gsub("08. Bezirk, Josefstadt,", "", Gemeinde),
         Gemeinde = gsub("09. Bezirk, Alsergrund,", "", Gemeinde),
         Gemeinde = gsub("10. Bezirk, Favoriten,", "", Gemeinde),
         Gemeinde = gsub("11. Bezirk, Simmering,", "", Gemeinde),
         Gemeinde = gsub("12. Bezirk, Meidling,", "", Gemeinde),
         Gemeinde = gsub("13. Bezirk, Hietzing,", "", Gemeinde),
         Gemeinde = gsub("14. Bezirk, Penzing,", "", Gemeinde),
         Gemeinde = gsub("15. Bezirk, Rudolfsheim-Fünfhaus,", "", Gemeinde),
         Gemeinde = gsub("16. Bezirk, Ottakring,", "", Gemeinde),
         Gemeinde = gsub("17. Bezirk, Hernals,", "", Gemeinde),
         Gemeinde = gsub("18. Bezirk, Währing,", "", Gemeinde),
         Gemeinde = gsub("19. Bezirk, Döbling,", "", Gemeinde),
         Gemeinde = gsub("20. Bezirk, Brigittenau,", "", Gemeinde),
         Gemeinde = gsub("21. Bezirk, Floridsdorf,", "", Gemeinde),
         Gemeinde = gsub("22. Bezirk, Donaustadt,", "", Gemeinde),
         Gemeinde = gsub("23. Bezirk, Liesing,", "", Gemeinde))

#head(test_test, 20)
#View(test_test)

test_geo = test_test |> slice(1:20)
test_geo_filtered = geo(test_geo$Gemeinde) |> drop_na()

test_sf = st_as_sf(test_geo_filtered, coords = c("long", "lat"), crs = 4326) 

#View(test_sf)
#mapview(test_sf)



