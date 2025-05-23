
# script to map current urban Still planning projects in vienna

library(tidygeocoder)
library(sf)
library(osmdata)
library(tidyverse)
library(mapview) # interkative, schnelle karten
library(ggthemes) # pretty ggplots

library(here)
library(glue)
library(jsonlite)
library(hereR)
library(readr)

# get geospatial data for each project

test <- read_sf("City data/Projects/data_projects.geojson")

#View(test)
# Recently completed projects
address_leopold_ungarn_platz <- "Leopold-Ungar-Platz, 1190 Wien"
geocoded_address_leopold_ungarn_platz <- geo(address_leopold_ungarn_platz) 
data_leopold_ungarn_platz <- st_as_sf(geocoded_address_leopold_ungarn_platz, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Leopold-Ungar-Platz" ,
         Status = "Recently completed",
         Type = "public space",
         Size = "Small")

address_ottokar_fischer_gasse <- "Ottokar-Fischer-Gasse 1, 1100 Wien"
geocoded_address_ottokar_fischer_gasse <- geo(address_ottokar_fischer_gasse) 
data_ottokar_fischer_gasse <- st_as_sf(geocoded_address_ottokar_fischer_gasse, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Biotope City Wienerberg" ,
          Status = "Recently completed",
         Type = "green space",
         Size = "Medium")

address_bruno_marek_allee <- "Bruno-Marek-Allee 24, 1020 Wien"   # nordbahnviertel
geocoded_address_bruno_marek_allee <- geo(address_bruno_marek_allee) 
data_bruno_marek_allee <- st_as_sf(geocoded_address_bruno_marek_allee, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Wohnallee mit Bildungscampus" ,
         Status = "Recently completed",
         Type = "housing",
         Size = "Medium")

address_praterstern <- "Praterstern, 1020 Wien"   
geocoded_address_praterstern <- geo(address_praterstern) 
data_praterstern <- st_as_sf(geocoded_address_praterstern, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Praterstern wird zur Oase" ,
         Status = "Recently completed",
         Type = "green space",
         Size = "Small")

address_westbahnhof_ikea <- "Europaplatz 1, 1150 Wien"   
geocoded_address_westbahnhof_ikea <- geo(address_westbahnhof_ikea) 
data_westbahnhof_ikea <- st_as_sf(geocoded_address_westbahnhof_ikea, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Westbahnhof IKEA - Verkehrsberuhigung im IKEA-Umfeld" ,
         Status = "Recently completed",
         Type = "green space",
         Size = "Small")

address_bildungscampus_gasometer <- "Rappachgasse 44, 1110 Wien"   
geocoded_address_bildungscampus_gasometer <- geo(address_bildungscampus_gasometer)
data_bildungscampus_gasometer <- st_as_sf(geocoded_address_bildungscampus_gasometer, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Neuer Bildungscampus im Gasometerumfeld" ,
         Status = "Currently implemented",
         Type = "education",
         Size = "Medium")    

address_rösslergasse <- "Rösslergasse 10, 1230 Wien"   
geocoded_address_rösslergasse <- geo(address_rösslergasse) 
data_rösslergasse <- st_as_sf(geocoded_address_rösslergasse, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Rösslergasse: Das Tor zu In der Wiesen"  ,
         Status = "Recently completed",
         Type = "housing",
         Size = "Medium")   

#donau_kanal
coordinates <- data.frame(long = 16.3774794, lat = 48.2123232)
data_zielgebiet_donaukanal <- st_as_sf(coordinates, coords = c("long", "lat"), crs = 4326)
data_zielgebiet_donaukanal <- data_zielgebiet_donaukanal |>
  mutate(Name = "Zielgebiet Donaukanal",
         address = "Schiffstation, Donaukanal, 1010 Wien",
         Status = "Recently completed",
         Type = "green space",
         Size = "Large")

#address_donaukanal <- "Motto am Fluss, Franz-Josefs-Kai, 1020 Wien"   
#geocoded_address_donaukanal <- geo(address_donaukanal) 
#data_donaukanal <- st_as_sf(c("48.2123232", "16.3774794"), coords = c("long", "lat"), crs = 4326) |>
#  mutate(Name = "Zielgebiet Donaukanal" ,
##         Status = "Recently completed",
#         Type = "green space",
#         Size = "Large")

address_bildungscampus_berrengasse <- "Scheedgasse 2, 1220 Wien"   
geocoded_address_bildungscampus_berrengasse <- geo(address_bildungscampus_berrengasse) 
data_bildungscampus_berrengasse <- st_as_sf(geocoded_address_bildungscampus_berrengasse, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Bildungscampus Berresgasse" ,
         Status = "Recently completed",
         Type = "education",
         Size = "Medium")

address_bildungscampus_attemgasse <- " Attemsgasse 22, 1220 Wien"   
geocoded_address_bildungscampus_attemgasse <- geo(address_bildungscampus_attemgasse) 
data_bildungscampus_attemgasse <- st_as_sf(geocoded_address_bildungscampus_attemgasse, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Bildungscampus Attemsgasse" ,
         Status = "Recently completed",
         Type = "education",
         Size = "Small")

address_wohnquartier_spallartgasse <- "Spallartgasse 21, 1140 Wien"   
geocoded_address_wohnquartier_spallartgasse <- geo(address_wohnquartier_spallartgasse) 
data_wohnquartier_spallartgasse <- st_as_sf(geocoded_address_wohnquartier_spallartgasse, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Spallartgasse 21 - Wohnquartier statt ehemaliger Kaserne" ,
         Status = "Recently completed",
         Type = "housing",
         Size = "Medium")

address_begegnungszone_rotenturmstrasse <- "Rotenturmstraße 14, 1010 Wien"   
geocoded_address_begegnungszone_rotenturmstrasse <- geo(address_begegnungszone_rotenturmstrasse) 
data_begegnungszone_rotenturmstrasse <- st_as_sf(geocoded_address_begegnungszone_rotenturmstrasse, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Rotenturmstraße Neu" ,
         Status = "Recently completed",
         Type = "culture",
         Size = "Small")

address_bildungscampus_atzgersdorf <- "Breitenfurter Straße 170, 1230 Wien"   
geocoded_address_bildungscampus_atzgersdorf <- geo(address_bildungscampus_atzgersdorf) 
data_bildungscampus_atzgersdorf <- st_as_sf(geocoded_address_bildungscampus_atzgersdorf, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Bildungscampus Atzgersdorf" ,
         Status = "Recently completed",
         Type = "education",
         Size = "Small")

address_bildungscampus_aron_menczer <- "Otto-Preminger-Straße 1, 1030 Wien"   
geocoded_address_bildungscampus_aron_menczer <- geo(address_bildungscampus_aron_menczer) 
data_bildungscampus_aron_menczer <- st_as_sf(geocoded_address_bildungscampus_aron_menczer, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Bildungscampus Aron Menczer" ,
         Status = "Recently completed",
         Type = "education",
         Size = "Small")

address_wohnquartier_unilever_gründe <- "Breitenfurter Straße 239, 1230 Wien"   
geocoded_address_wohnquartier_unilever_gründe <- geo(address_wohnquartier_unilever_gründe) 
data_wohnquartier_unilever_gründe <- st_as_sf(geocoded_address_wohnquartier_unilever_gründe, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Unilever-Gründe-Wohnquartier statt Industriegebiet" ,
         Status = "Recently completed",
         Type = "housing",
         Size = "Small")

address_sonnwendviertel <- "Helmut-Zilk-Park, 1100 Wien"   
geocoded_address_sonnwendviertel <- geo(address_sonnwendviertel) 
data_sonnwendviertel <- st_as_sf(geocoded_address_sonnwendviertel, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Sonnwendviertel" ,
         Status = "Recently completed",
         Type = "housing",
         Size = "Large")



# Currently implemented projects

address_naschmarkt_neugestaltung <- "Naschmarkt 510, 1060 Wien"   
geocoded_address_naschmarkt_neugestaltung <- geo(address_naschmarkt_neugestaltung) 
data_naschmarkt_neugestaltung <- st_as_sf(geocoded_address_naschmarkt_neugestaltung, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Naschmarkt-Parkplatz: Hitzeinsel wird Grünoase" ,
         Status = "Currently implemented",
         Type = "nature",
         Size = "Small")

address_vienna_business_district_ost <- "Maria-Jacobi-Gasse 2, 1030 Wien"
geocoded_address_vienna_business_district_ost <- geo(address_vienna_business_district_ost)
data_vienna_business_district_ost <- st_as_sf(geocoded_address_vienna_business_district_ost, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Vienna Business District Ost" ,
         Status = "Currently implemented",
         Type = "business",
         Size = "Medium")

address_vienna_business_district_süd <- "Perfektastraße 87, 1230 Wien"   
geocoded_address_vienna_business_district_süd <- geo(address_vienna_business_district_süd)
data_vienna_business_district_süd <- st_as_sf(geocoded_address_vienna_business_district_süd, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Vienna Business District Süd" ,
         Status = "Currently implemented",
         Type = "business",
         Size = "Medium")


address_vienna_business_district_nord <- "Puchgasse 1, 1220 Wien"   
geocoded_address_vienna_business_district_nord <- geo(address_vienna_business_district_nord)
data_vienna_business_district_nord <- st_as_sf(geocoded_address_vienna_business_district_nord, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Vienna Business District Nord" ,
         Status = "Currently implemented",
         Type = "business",
         Size = "Medium")

address_ostbahn <- "An der Ostbahn 46, 1100 Wien"   
geocoded_address_ostbahn <- geo(address_ostbahn)
data_ostbahn <- st_as_sf(geocoded_address_ostbahn, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Tangentenpark an der Ostbahn" ,
         Status = "Currently implemented",
         Type = "nature",
         Size = "Small")

address_wohnquartier_pilzgasse <- "Pilzgasse 33, 1210 Wien"   
geocoded_address_wohnquartier_pilzgasse <- geo(address_wohnquartier_pilzgasse)
data_wohnquartier_pilzgasse <- st_as_sf(geocoded_address_wohnquartier_pilzgasse, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Pilzgasse 33" ,
         Status = "Currently implemented",
         Type = "housing",
         Size = "Small")

address_Bafep21_Freytaggasse <- "Patrizigasse 2, 1210 Wien"   
geocoded_address_Bafep21_Freytaggasse <- geo(address_Bafep21_Freytaggasse)
data_Bafep21_Freytaggasse <- st_as_sf(geocoded_address_Bafep21_Freytaggasse, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Bafep21 - Freytaggasse" ,
         Status = "Currently implemented",
         Type = "education",
         Size = "Small")

address_entwicklungsgebiet_raffenstättergasse <- "Raffenstättergasse, 1220 Wien"   
geocoded_address_entwicklungsgebiet_raffenstättergasse <- geo(address_entwicklungsgebiet_raffenstättergasse)
data_entwicklungsgebiet_raffenstättergasse <- st_as_sf(geocoded_address_entwicklungsgebiet_raffenstättergasse, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Entwicklungsgebiet Raffenstättergasse" ,
         Status = "Currently implemented",
         Type = "housing",
         Size = "Medium")

address_wohnquartier_kurbadstraße <- "Kurbadstraße 8, 1100 Wien"   
geocoded_address_wohnquartier_kurbadstraße <- geo(address_wohnquartier_kurbadstraße)
data_wohnquartier_kurbadstraße <- st_as_sf(geocoded_address_wohnquartier_kurbadstraße, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Wohnquartier Kurbadstraße" ,
         Status = "Currently implemented",
         Type = "housing",
         Size = "Medium")
 
address_stadtrand_siebenhirten <- "Basler Gasse 86, 1230 Wien"   
geocoded_address_stadtrand_siebenhirten <- geo(address_stadtrand_siebenhirten)
data_stadtrand_siebenhirten <- st_as_sf(geocoded_address_stadtrand_siebenhirten, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Stadtrand Vörsendorf/Siebenhierten" ,
         Status = "Currently implemented",
         Type = "transport",
         Size = "Medium")


address_neu_leopoldau <- "Wohlfahrtsweg 2, 1210 Wien"   
geocoded_address_neu_leopoldau <- geo(address_neu_leopoldau)
data_neu_leopoldau <- st_as_sf(geocoded_address_neu_leopoldau, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Neu Leopoldau" ,
         Status = "Currently implemented",
         Type = "housing",
         Size = "Medium")

address_wohnquartier_amseebogen <- "Lina-Bo-Bardi-Platz 1, 1220 Wien"   
geocoded_address_wohnquartier_amseebogen <- geo(address_wohnquartier_amseebogen)
data_wohnquartier_amseebogen <- st_as_sf(geocoded_address_wohnquartier_amseebogen, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Quartier Am Seeobogen" ,
         Status = "Currently implemented",
         Type = "housing",
         Size = "Medium")

address_donau_city <- "Bruno-Kreisky-Platz, 1220 Wien"   
geocoded_address_donau_city <- geo(address_donau_city)
data_donau_city <- st_as_sf(geocoded_address_donau_city, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Donau City" ,
         Status = "Currently implemented",
         Type = "business",
         Size = "Large")


address_wohnhausanlage_dresdnerstraße <- "Dresdner Straße 84-90, 1200 Wien"     # probably too Small to keep it? maybe only focus on wohnquartiers
geocoded_address_wohnhausanlage_dresdnerstraße <- geo(address_wohnhausanlage_dresdnerstraße)
data_wohnhausanlage_dresdnerstraße <- st_as_sf(geocoded_address_wohnhausanlage_dresdnerstraße, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Wohnhausanlage Dresdner Straße 84-90" ,
         Status = "Currently implemented",
         Type = "housing",
         Size = "Small")

address_zentrum_kagran <- "Wagramer Strasse 94, 1220 Wien"     
geocoded_address_zentrum_kagran <- geo(address_zentrum_kagran)
data_zentrum_kagran <- st_as_sf(geocoded_address_zentrum_kagran, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Zentrum Kagran" ,
         Status = "Currently implemented",
         Type = "culture",
         Size = "Medium")

address_entwicklungsgebiet_althangrund <- "Augasse 2-6, 1090 Wien"     
geocoded_address_entwicklungsgebiet_althangrund <- geo(address_entwicklungsgebiet_althangrund)
data_entwicklungsgebiet_althangrund <- st_as_sf(geocoded_address_entwicklungsgebiet_althangrund, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Stadtentwicklungsgebiet Althangrund" ,
         Status = "Currently implemented",
         Type = "culture",
         Size = "Medium")

address_gründerzeit_westgürtel <- "Währinger Gürtel 41, 1180 Wien"     
geocoded_address_gründerzeit_westgürtel <- geo(address_gründerzeit_westgürtel)
data_gründerzeit_westgürtel <- st_as_sf(geocoded_address_gründerzeit_westgürtel, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Zielgebiet Gründerzeitviertel/Westgürtel" ,
         Status = "Currently implemented",
         Type = "nature",
         Size = "Large")   # zielgebiet

address_schulcampus_brigittenau <- "Leystraße 34, 1200 Wien"     
geocoded_address_schulcampus_brigittenau <- geo(address_schulcampus_brigittenau)
data_schulcampus_brigittenau <- st_as_sf(geocoded_address_schulcampus_brigittenau, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Leystraße 34 - Neuer Campus in der Brigittenau" ,
         Status = "Currently implemented",
         Type = "education",
         Size = "Small")

address_semmelweisklinik_areal <- "Hockegasse 37, 1180 Wien"     
geocoded_address_semmelweisklinik_areal <- geo(address_semmelweisklinik_areal)
data_semmelweisklinik_areal <- st_as_sf(geocoded_address_semmelweisklinik_areal, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Semmelweis-Areal" ,
         Status = "Currently implemented",
         Type = "nature",
         Size = "Small")

address_asparngründe <- "Otto-Preminger-Straße 15, 1030 Wien"     
geocoded_address_asparngründe <- geo(address_asparngründe)
data_asparngründe <- st_as_sf(geocoded_address_asparngründe, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Aspanggründe" ,
         Status = "Currently implemented",
         Type = "education",
         Size = "Medium")

address_quartier_althan <- "Julius-Tandler-Platz 3, 1090 Wien"     
geocoded_address_quartier_althan <- geo(address_quartier_althan)
data_quartier_althan <- st_as_sf(geocoded_address_quartier_althan, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Althan Quartier" ,
         Status = "Currently implemented",
         Type = "food",
         Size = "Small")

address_ottowagner_areal <- "Baumgartner Höhe 1, 1140 Wien"     
geocoded_address_ottowagner_areal <- geo(address_ottowagner_areal)
data_ottowagner_areal <- st_as_sf(geocoded_address_ottowagner_areal, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Otto-Wagner-Areal" ,
         Status = "Currently implemented",
         Type = "culture",
         Size = "Small")

address_stadtquariter_viertelzwei <- "Am Grünen Prater 2, 1020 Wien"     
geocoded_address_stadtquariter_viertelzwei <- geo(address_stadtquariter_viertelzwei)
data_stadtquariter_viertelzwei <- st_as_sf(geocoded_address_stadtquariter_viertelzwei, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Viertel Zwei Plus - Neues Stadtquartier an der U2" ,
         Status = "Currently implemented",
         Type = "housing",
         Size = "Medium")

address_zielgebiet_donaustadt <- "Donaustadt, 1220 Wien"     
geocoded_address_zielgebiet_donaustadt <- geo(address_zielgebiet_donaustadt)
data_zielgebiet_donaustadt <- st_as_sf(geocoded_address_zielgebiet_donaustadt, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Zielgebiet U2 Donaustadt" ,
         Status = "Currently implemented",
         Type = "transport",
         Size = "Large")    # zielgebiet = Large?

address_zielgebiet_floridsdorf <- "Pius-Parsch-Platz, 1210 Wien"     
geocoded_address_zielgebiet_floridsdorf <- geo(address_zielgebiet_floridsdorf)
data_zielgebiet_floridsdorf <- st_as_sf(geocoded_address_zielgebiet_floridsdorf, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Zielgebiet Floridsdorf" ,
         Status = "Currently implemented",
         Type = "transport",
         Size = "Large")

address_quartier_süßbrunnerwest <- "Süßenbrunner Strasse 62, 1220 Wien"     
geocoded_address_quartier_süßbrunnerwest <- geo(address_quartier_süßbrunnerwest)
data_quartier_süßbrunnerwest <- st_as_sf(geocoded_address_quartier_süßbrunnerwest, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Quartier Süßenbrunner West" ,
         Status = "Currently implemented",
         Type = "housing",
         Size = "Medium")

address_kulturzentrum_sagfabrik <- "Breitenfurter Straße 176, 1230 Wien"     
geocoded_address_kulturzentrum_sagfabrik <- geo(address_kulturzentrum_sagfabrik)
data_kulturzentrum_sagfabrik <- st_as_sf(geocoded_address_kulturzentrum_sagfabrik, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Kulturzentrum in Sagfabrik Atzgersdorf" ,
         Status = "Currently implemented",
         Type = "culture",
         Size = "Medium")

address_südraum_favoriten <- "Favoritenstraße 239, 1100 Wien"     
geocoded_address_südraum_favoriten <- geo(address_südraum_favoriten)
data_südraum_favoriten <- st_as_sf(geocoded_address_südraum_favoriten, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Südraum Favoriten" ,
         Status = "Currently implemented",
         Type = "transport",
         Size = "Large")

address_nordwestbahnstraße <- "Nordwestbahnstraße 53, 1200 Wien"     
geocoded_address_nordwestbahnstraße <- geo(address_nordwestbahnstraße)
data_nordwestbahnstraße <- st_as_sf(geocoded_address_nordwestbahnstraße, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Nordwestbahnstraße 53" ,
         Status = "Currently implemented",
         Type = "housing",
         Size = "Small")

address_supergrätzl_favoriten <- "Pernerstorfergasse 47, 1100 Wien"     
geocoded_address_supergrätzl_favoriten <- geo(address_supergrätzl_favoriten)
data_supergrätzl_favoriten <- st_as_sf(geocoded_address_supergrätzl_favoriten, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Supergrätzl Favoriten" ,
         Status = "Currently implemented",
         Type = "transport",
         Size = "Medium")

address_liesing_mitte <- "Brunner Strasse 52, 1230 Wien"     
geocoded_address_liesing_mitte <- geo(address_liesing_mitte)
data_liesing_mitte <- st_as_sf(geocoded_address_liesing_mitte, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Liesing Mitte" ,
         Status = "Currently implemented",
         Type = "transport",
         Size = "Medium")

address_sporthalle_jenschikweg <- "Jenschikweg 12, 1170 Wien"     
geocoded_address_sporthalle_jenschikweg <- geo(address_sporthalle_jenschikweg)
data_sporthalle_jenschikweg <- st_as_sf(geocoded_address_sporthalle_jenschikweg, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Sporthalle Jenschikweg" ,
         Status = "Currently implemented",
         Type = "culture",
         Size = "Small")


address_urban_gardening_inderwiesenost <- "In der Wiesen, 1230 Wien"     
geocoded_address_urban_gardening_inderwiesenost <- geo(address_urban_gardening_inderwiesenost)
data_urban_gardening_inderwiesenost <- st_as_sf(geocoded_address_urban_gardening_inderwiesenost, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "In der Wiesen Ost - Europas größte Urban Gardening Siedlung" ,
         Status = "Currently implemented",
         Type = "housing",
         Size = "Medium")

address_volksschule_grinzing <- "Grinzinger Straße 88, 1190 Wien"     
geocoded_address_volksschule_grinzing <- geo(address_volksschule_grinzing)
data_volksschule_grinzing <- st_as_sf(geocoded_address_volksschule_grinzing, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Volksschule Grinzinger Straße wird ausgebaut" ,
         Status = "Currently implemented",
         Type = "education",
         Size = "Small")

address_gemeindebau_epidelauerstraße <- "Eipeldauer Straße 21-25, 1220 Wien"     
geocoded_address_gemeindebau_epidelauerstraße <- geo(address_gemeindebau_epidelauerstraße)
data_gemeindebau_epidelauerstraße <- st_as_sf(geocoded_address_gemeindebau_epidelauerstraße, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Eipeldauer Straße - Gemeindebau NEU" ,
         Status = "Currently implemented",
         Type = "housing",
         Size = "Small")

address_wohnquartier_wildgarten <- "Emil-Behring-Weg 5, 1120 Wien"     
geocoded_address_wohnquartier_wildgarten <- geo(address_wohnquartier_wildgarten)
data_wohnquartier_wildgarten <- st_as_sf(geocoded_address_wohnquartier_wildgarten, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Wildgarten - Wohnen am Rosenhügel" ,
         Status = "Currently implemented",
         Type = "housing",
         Size = "Medium")

address_wohnquartier_viola_park2 <- "Czeikestraße 2, 1100 Wien"     
geocoded_address_wohnquartier_viola_park2 <- geo(address_wohnquartier_viola_park2)
data_wohnquartier_viola_park2 <- st_as_sf(geocoded_address_wohnquartier_viola_park2, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Viola Park II - Neues Quartier in Favoriten" ,
         Status = "Currently implemented",
         Type = "housing",
         Size = "Medium")

address_wohnquartier_34takt <- "Berresgasse, 1220 Wien"     
geocoded_address_wohnquartier_34takt <- geo(address_wohnquartier_34takt)
data_wohnquartier_34takt <- st_as_sf(geocoded_address_wohnquartier_34takt, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Das 3/4terl - Gemeinsam im Takt (Berresgasse)" ,
         Status = "Currently implemented",
         Type = "housing",
         Size = "Medium")

address_sophienspital <- "Apollogasse 17, 1070 Wien"     
geocoded_address_sophienspital <- geo(address_sophienspital)
data_sophienspital <- st_as_sf(geocoded_address_sophienspital, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Sophienspital" ,
         Status = "Currently implemented",
         Type = "culture",
         Size = "Small")

address_wohnhaus_effenbergplatz <- "Effenbergplatz, 1220 Wien"     
geocoded_address_wohnhaus_effenbergplatz <- geo(address_wohnhaus_effenbergplatz)
data_wohnhaus_effenbergplatz <- st_as_sf(geocoded_address_wohnhaus_effenbergplatz, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Effenbergplatz" ,
         Status = "Currently implemented",
         Type = "housing",
         Size = "Small")

address_berufsschule_seestadt <- "Edith-Piaf-Straße 5, 1220 Wien"     
geocoded_address_berufsschule_seestadt <- geo(address_berufsschule_seestadt)
data_berufsschule_seestadt <- st_as_sf(geocoded_address_berufsschule_seestadt, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Zentralberufschulgebäude Seestadt Aspern" ,
         Status = "Currently implemented",
         Type = "education",
         Size = "Small")

address_carre_atzgersdorf <- "Scherbangasse 1, 1230 Wien"     
geocoded_address_carre_atzgersdorf <- geo(address_carre_atzgersdorf)
data_carre_atzgersdorf <- st_as_sf(geocoded_address_carre_atzgersdorf, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Carré Atzgersdorf" ,
         Status = "Currently implemented",
         Type = "housing",
         Size = "Medium")

address_wohnquartier_breitenleerstraßesüd <- "Breitenleer Straße 224, 1220 Wien"     
geocoded_address_wohnquartier_breitenleerstraßesüd <- geo(address_wohnquartier_breitenleerstraßesüd)
data_wohnquartier_breitenleerstraßesüd <- st_as_sf(geocoded_address_wohnquartier_breitenleerstraßesüd, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Breitenleer Straße Süd - Neues Quartier in der Donaustadt" ,
         Status = "Currently implemented",
         Type = "housing",
         Size = "Medium")

address_wohnquartier_attemsgasse <- "Attemsgasse 33-44, 1220 Wien"     
geocoded_address_wohnquartier_attemsgasse <- geo(address_wohnquartier_attemsgasse)
data_wohnquartier_attemsgasse <- st_as_sf(geocoded_address_wohnquartier_attemsgasse, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Wohnquartier Attemsgasse Ost" ,
         Status = "Currently implemented",
         Type = "housing",
         Size = "Small")

address_theresianum <- "Favoritenstraße 15, 1040 Wien"     
geocoded_address_theresianum <- geo(address_theresianum)
data_theresianum <- st_as_sf(geocoded_address_theresianum, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "ORF/Theresianum: Neue Nutzungsmöglichkeiten" ,
         Status = "Currently implemented",
         Type = "nature",
         Size = "Small")

address_wohnen_anderkuhtrift <- "An der Kuhtrift, 1100 Wien"     
geocoded_address_wohnen_anderkuhtrift <- geo(address_wohnen_anderkuhtrift)
data_wohnen_anderkuhtrift <- st_as_sf(geocoded_address_wohnen_anderkuhtrift, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "An der Kuhtrift - Wohnen und Arbeiten im Süden Favoritens" ,
         Status = "Currently implemented",
         Type = "housing",
         Size = "Small")

address_wohnbebauung_amrain <- "Am Rain, 1220 Wien"     
geocoded_address_wohnbebauung_amrain <- geo(address_wohnbebauung_amrain)
data_wohnbebauung_amrain <- st_as_sf(geocoded_address_wohnbebauung_amrain, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Am Rain - Wohnbebaung und Grünzug" ,
         Status = "Currently implemented",
         Type = "housing",
         Size = "Small")

address_stadtquartier_amlangenfelde <- "Am langen Felde, 1220 Wien"     
geocoded_address_stadtquartier_amlangenfelde <- geo(address_stadtquartier_amlangenfelde)
data_stadtquartier_amlangenfelde <- st_as_sf(geocoded_address_stadtquartier_amlangenfelde, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Neues Stadtquartier Am lange Felde" ,
         Status = "Currently implemented",
         Type = "housing",
         Size = "Medium")

address_entwicklungsprojekt_kendlerstraße <- "Kendlerstraße 35-37, 1140 Wien"     
geocoded_address_entwicklungsprojekt_kendlerstraße <- geo(address_entwicklungsprojekt_kendlerstraße)
data_entwicklungsprojekt_kendlerstraße <- st_as_sf(geocoded_address_entwicklungsprojekt_kendlerstraße, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Stadtentwicklungsprojekt Kenlderstraße 35-37" ,
         Status = "Currently implemented",
         Type = "nature",
         Size = "Small")

address_entwicklungsprojekt_oberedonaustraße <- "Obere Donaustraße 23-29, 1020 Wien"     
geocoded_address_entwicklungsprojekt_oberedonaustraße <- geo(address_entwicklungsprojekt_oberedonaustraße)
data_entwicklungsprojekt_oberedonaustraße <- st_as_sf(geocoded_address_entwicklungsprojekt_oberedonaustraße, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Obere Donaustraße 23-29" ,
         Status = "Currently implemented",
         Type = "housing",
         Size = "Medium")

address_busterminal_handelskai <- "Engerthstraße 269, 1020 Wien"     
geocoded_address_busterminal_handelskai <- geo(address_busterminal_handelskai)
data_busterminal_handelskai <- st_as_sf(geocoded_address_busterminal_handelskai, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Internationaler Fernbus-Terminal am Handelskai" ,
         Status = "Currently implemented",
         Type = "transport",
         Size = "Medium")

address_aspern_seestadt <- "aspern Seestadt, 1220 Wien"     
geocoded_address_aspern_seestadt <- geo(address_aspern_seestadt)
data_aspern_seestadt <- st_as_sf(geocoded_address_aspern_seestadt, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "aspern Die Seestadt Wiens" ,
         Status = "Currently implemented",
         Type = "housing",
         Size = "Large")

address_nordbahnviertel <- "Nordbahnviertel, 1020 Wien"     
geocoded_address_nordbahnviertel <- geo(address_nordbahnviertel)
data_nordbahnviertel <- st_as_sf(geocoded_address_nordbahnviertel, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Nordbahnviertel - Einse neues Grätzl mit viel Grün" ,
         Status = "Currently implemented",
         Type = "housing",
         Size = "Large")

address_wohnquartier_kempelenpark <- "Kempelengasse 16, 1100 Wien"     
geocoded_address_wohnquartier_kempelenpark <- geo(address_wohnquartier_kempelenpark)
data_wohnquartier_kempelenpark <- st_as_sf(geocoded_address_wohnquartier_kempelenpark, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Kempelenpark - Neues Quartier für Favoriten" ,
         Status = "Currently implemented",
         Type = "housing",
         Size = "Medium")

address_wohnquartier_anderschanze <- "An der Schanze, 1210 Wien"     
geocoded_address_wohnquartier_anderschanze <- geo(address_wohnquartier_anderschanze)
data_wohnquartier_anderschanze <- st_as_sf(geocoded_address_wohnquartier_anderschanze, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Quartier An der Schanze" ,
         Status = "Currently implemented",
         Type = "housing",
         Size = "Medium")

address_entwicklungsprojekt_neueslandgut <- "Neues Landgut, 1100 Wien"     
geocoded_address_entwicklungsprojekt_neueslandgut <- geo(address_entwicklungsprojekt_neueslandgut)
data_entwicklungsprojekt_neueslandgut <- st_as_sf(geocoded_address_entwicklungsprojekt_neueslandgut, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Stadtentwicklungsgebiet Neues Landgut" ,
         Status = "Currently implemented",
         Type = "nature",
         Size = "Small")


# adopted projects

address_schulneubau_laimäckergasse <- "Laimäckergasse 17, 1100 Wien"     
geocoded_address_schulneubau_laimäckergasse <- geo(address_schulneubau_laimäckergasse)
data_schulneubau_laimäckergasse <- st_as_sf(geocoded_address_schulneubau_laimäckergasse, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Sanierung und Schulneubau Laimäckergasse" ,
         Status = "Currently implemented",
         Type = "education",
         Size = "Small")

address_auva_meidling <- "Köglergasse 2A, 1120 Wien"     
geocoded_address_auva_meidling <- geo(address_auva_meidling)
data_auva_meidling <- st_as_sf(geocoded_address_auva_meidling, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "AUVA - Rehabilationszentrum" ,
         Status = "Currently implemented",
         Type = "healthcare",
         Size = "Small")

address_entwicklungsprojekt_ambrosweg <- "Ambrosweg 11, 1230 Wien"     
geocoded_address_entwicklungsprojekt_ambrosweg <- geo(address_entwicklungsprojekt_ambrosweg)
data_entwicklungsprojekt_ambrosweg <- st_as_sf(geocoded_address_entwicklungsprojekt_ambrosweg, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Ambrosweg" ,
         Status = "Currently implemented",
         Type = "nature",
         Size = "Small")

address_eislaufen_heumarkt <- "Am Heumarkt, 1030 Wien"     
geocoded_address_eislaufen_heumarkt <- geo(address_eislaufen_heumarkt)
data_eislaufen_heumarkt <- st_as_sf(geocoded_address_eislaufen_heumarkt, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Wiener Eislaufverein" ,
         Status = "Currently implemented",
         Type = "culture",
         Size = "Small")

address_ortskern_nussdorf_heiligenstadt <- "Heiligenstadt, 1190 Wien"     
geocoded_address_ortskern_nussdorf_heiligenstadt <- geo(address_ortskern_nussdorf_heiligenstadt)
data_ortskern_nussdorf_heiligenstadt <- st_as_sf(geocoded_address_ortskern_nussdorf_heiligenstadt, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Ortskern Nussdorf und Heiligenstadt" ,
         Status = "Currently implemented",
         Type = "culture",
         Size = "Small")

address_arbeitsstätte_karlschäferstraße <- "Karl-Schäfer-Straße, 1210 Wien"     
geocoded_address_arbeitsstätte_karlschäferstraße <- geo(address_arbeitsstätte_karlschäferstraße)
data_arbeitsstätte_karlschäferstraße <- st_as_sf(geocoded_address_arbeitsstätte_karlschäferstraße, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Arbeits-und Produktionsstätte Karl-Schäfer-Straße" ,
         Status = "Currently implemented",
         Type = "business",
         Size = "Small")

address_stadtrand_aspern_hausfeld <- "Aspern Hausfeld, 1220 Wien"     
geocoded_address_stadtrand_aspern_hausfeld <- geo(address_stadtrand_aspern_hausfeld)
data_stadtrand_aspern_hausfeld <- st_as_sf(geocoded_address_stadtrand_aspern_hausfeld, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Stadtrandsieldung Aspern Hausfeld" ,
         Status = "Currently implemented",
         Type = "housing",
         Size = "Small")

address_betriebsgebiet_stachegasse <- "Stachegasse 13, 1120 Wien"     
geocoded_address_betriebsgebiet_stachegasse <- geo(address_betriebsgebiet_stachegasse)
data_betriebsgebiet_stachegasse <- st_as_sf(geocoded_address_betriebsgebiet_stachegasse, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Betriebsgebiet Stachegasse" ,
         Status = "Currently implemented",
         Type = "business",
         Size = "Medium")

address_ortskern_oberlaa <- "Oberlaa, 1100 Wien"     
geocoded_address_ortskern_oberlaa <- geo(address_ortskern_oberlaa)
data_ortskern_oberlaa <- st_as_sf(geocoded_address_ortskern_oberlaa, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Ortskern Oberlaa und Umgebung" ,
         Status = "Currently implemented",
         Type = "culture",
         Size = "Small")

address_entwicklungsprojekt_obereshausfeld <- "An den alten Schanzen, 1220 Wien"     
geocoded_address_entwicklungsprojekt_obereshausfeld <- geo(address_entwicklungsprojekt_obereshausfeld)
data_entwicklungsprojekt_obereshausfeld <- st_as_sf(geocoded_address_entwicklungsprojekt_obereshausfeld, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Stadtentwicklungsgebiet Oberes Hausfeld" ,
         Status = "Currently implemented",
         Type = "housing",
         Size = "Medium")


# dedication procedure

address_treibstoff_seybelgasse <- "Seybelgasse 1, 1230 Wien"     
geocoded_address_treibstoff_seybelgasse <- geo(address_treibstoff_seybelgasse)
data_treibstoff_seybelgasse <- st_as_sf(geocoded_address_treibstoff_seybelgasse, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Seybelgasse - Treibstoffversorgung für Berufsrettung" ,
         Status = "Still planning",
         Type = "transport",
         Size = "Small")

address_sunken_city <- "Sunken City, Wien"     
geocoded_address_sunken_city <- geo(address_sunken_city)
data_sunken_city <- st_as_sf(geocoded_address_sunken_city, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Sunken City - Neugestaltung" ,
         Status = "Still planning",
         Type = "nature",
         Size = "Small")

address_höpflerbad <- "Endresstraße 24-26, 1230 Wien"     
geocoded_address_höpflerbad <- geo(address_höpflerbad)
data_höpflerbad <- st_as_sf(geocoded_address_höpflerbad, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Höpflerbad: Standorterweiterung" ,
         Status = "Still planning",
         Type = "culture",
         Size = "Small")

address_karolineperingasse <- "Karoline-Perin-Gasse, 1220 Wien"     
geocoded_address_karolineperingasse <- geo(address_karolineperingasse)
data_karolineperingasse <- st_as_sf(geocoded_address_karolineperingasse, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Karoline-Perin-Gasse" ,
         Status = "Still planning",
         Type = "transport",
         Size = "Small")

address_schutzzone_währing <- "Währinger Gürtel, 1180 Wien"     
geocoded_address_schutzzone_währing <- geo(address_schutzzone_währing)
data_schutzzone_währing <- st_as_sf(geocoded_address_schutzzone_währing, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Schutzzone Währinger Gürtel" ,
         Status = "Still planning",
         Type = "nature",
         Size = "Small")

address_kinkplatz <- "Kinkplatz, 1140 Wien"     
geocoded_address_kinkplatz <- geo(address_kinkplatz)
data_kinkplatz <- st_as_sf(geocoded_address_kinkplatz, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Kinkplatz" ,
         Status = "Still planning",
         Type = "culture",
         Size = "Small")

address_feuerwehr_breitenlee <- "Breitenleer Straße 268, 1220 Wien"     
geocoded_address_feuerwehr_breitenlee <- geo(address_feuerwehr_breitenlee)
data_feuerwehr_breitenlee <- st_as_sf(geocoded_address_feuerwehr_breitenlee, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Feuerwehr Breitenlee" ,
         Status = "Still planning",
         Type = "law&order",
         Size = "Small")

address_aspern_seestadt_zentralbereich <- "Nelson-Mandela-Platz, 1220 Wien"     
geocoded_address_aspern_seestadt_zentralbereich <- geo(address_aspern_seestadt_zentralbereich)
data_aspern_seestadt_zentralbereich <- st_as_sf(geocoded_address_aspern_seestadt_zentralbereich, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Aspern Seestadt Zentralbereich" ,
         Status = "Still planning",
         Type = "nature",
         Size = "Medium")

address_porzellanmanufaktur_augarten <- "Obere Augartenstraße 1, 1020 Wien"     
geocoded_address_porzellanmanufaktur_augarten <- geo(address_porzellanmanufaktur_augarten)
data_porzellanmanufaktur_augarten <- st_as_sf(geocoded_address_porzellanmanufaktur_augarten, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Porzellanmanufaktur Augarten" ,
         Status = "Still planning",
         Type = "culture",
         Size = "Small")

address_blockentwicklung_lacknergasse <- "Lacknergasse 45, 1170 Wien"     
geocoded_address_blockentwicklung_lacknergasse <- geo(address_blockentwicklung_lacknergasse)
data_blockentwicklung_lacknergasse <- st_as_sf(geocoded_address_blockentwicklung_lacknergasse, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Lacknergasse - Zukunftsfähige Blockentwicklung" ,
         Status = "Still planning",
         Type = "nature",
         Size ="Small")

address_entwicklungsprojekt_dornbacherstraße <- "Dornbacher Straße, 1170 Wien"     
geocoded_address_entwicklungsprojekt_dornbacherstraße <- geo(address_entwicklungsprojekt_dornbacherstraße)
data_entwicklungsprojekt_dornbacherstraße <- st_as_sf(geocoded_address_entwicklungsprojekt_dornbacherstraße, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Modernisierung Dornbacher Straße" ,
         Status = "Still planning",
         Type = "nature",
         Size = "Small")


address_fh_höchstädtplatz <- "Höchstädtplatz 6, 1200 Wien"     
geocoded_address_fh_höchstädtplatz <- geo(address_fh_höchstädtplatz)
data_fh_höchstädtplatz <- st_as_sf(geocoded_address_fh_höchstädtplatz, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "FH Höchststädtplatz - Modernisierung und Ausbau" ,
         Status = "Still planning",
         Type = "education",
         Size = "Small")

address_wohnbauprojekt_paltramplatz <- "Paltramplatz, 1100 Wien"     
geocoded_address_wohnbauprojekt_paltramplatz <- geo(address_wohnbauprojekt_paltramplatz)
data_wohnbauprojekt_paltramplatz <- st_as_sf(geocoded_address_wohnbauprojekt_paltramplatz, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Wohnbauprojekt Paltramplatz" ,
         Status = "Still planning",
         Type = "housing",
         Size = "Small")

address_wohnquartier_tanbruckgasse <- "Tanbruckgasse, 1120 Wien"     
geocoded_address_wohnquartier_tanbruckgasse <- geo(address_wohnquartier_tanbruckgasse)
data_wohnquartier_tanbruckgasse <- st_as_sf(geocoded_address_wohnquartier_tanbruckgasse, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Modernisierung Tanbruckgasse" ,
         Status = "Still planning",
         Type = "housing",
         Size = "Small")

address_wohnquartier_kainzgasse <- "Kainzgasse, 1170 Wien"     
geocoded_address_wohnquartier_kainzgasse <- geo(address_wohnquartier_kainzgasse)
data_wohnquartier_kainzgasse <- st_as_sf(geocoded_address_wohnquartier_kainzgasse, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Kaingasse - Ausweisung einer Schutzgasse" ,
         Status = "Still planning",
         Type = "housing",
         Size = "Small")

address_hörndlwald <- "Hörndlwald, 1130 Wien"     
geocoded_address_hörndlwald <- geo(address_hörndlwald)
data_hörndlwald <- st_as_sf(geocoded_address_hörndlwald, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Hörndlwald" ,
         Status = "Still planning",
         Type = "nature",
         Size = "Small")

address_entwicklungsprojekt_schrödingerplatz <- "Schrödingerplatz, 1220 Wien"     
geocoded_address_entwicklungsprojekt_schrödingerplatz <- geo(address_entwicklungsprojekt_schrödingerplatz)
data_entwicklungsprojekt_schrödingerplatz <- st_as_sf(geocoded_address_entwicklungsprojekt_schrödingerplatz, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Schrödingerplatz" ,
         Status = "Still planning",
         Type = "education",
         Size = "Small")

address_klinik_hitzing <- "Wolkersbergenstraße 1, 1130 Wien"     
geocoded_address_klinik_hitzing <- geo(address_klinik_hitzing)
data_klinik_hitzing <- st_as_sf(geocoded_address_klinik_hitzing, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Klinik Hitzin Neu" ,
         Status = "Still planning",
         Type = "healthcare",
         Size = "Small")

address_wohnquartier_hirschstettnerstraße <- "Hirschstettner Straße 27-33, 1220 Wien"     
geocoded_address_wohnquartier_hirschstettnerstraße <- geo(address_wohnquartier_hirschstettnerstraße)
data_wohnquartier_hirschstettnerstraße <- st_as_sf(geocoded_address_wohnquartier_hirschstettnerstraße, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Hirstettner Straße - Wohnquartier" ,
         Status = "Still planning",
         Type = "housing",
         Size = "Small")

address_biotop_wildquell <- "Walter-Jurmann-Gasse 8, 1230 Wien"     
geocoded_address_biotop_wildquell <- geo(address_biotop_wildquell)
data_biotop_wildquell <- st_as_sf(geocoded_address_biotop_wildquell, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Biotop Wildquell" ,
         Status = "Still planning",
         Type = "housing",
         Size = "Medium")

address_siedlung_werndlgasse <- "Werndlgasse, 1210 Wien"     
geocoded_address_siedlung_werndlgasse <- geo(address_siedlung_werndlgasse)
data_siedlung_werndlgasse <- st_as_sf(geocoded_address_siedlung_werndlgasse, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Sanierung und Aufwertung der Sieldung Werndlgasse" ,
         Status = "Still planning",
         Type = "housing",
         Size = "Small")

address_bricolage_city_gasometer <- "Gasometer, 1030 Wien"     
geocoded_address_bricolage_city_gasometer <- geo(address_bricolage_city_gasometer)
data_bricolage_city_gasometer <- st_as_sf(geocoded_address_bricolage_city_gasometer, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Gasometervorfeld 2.0 - Bricolage City" ,
         Status = "Still planning",
         Type = "housing",
         Size = "Medium")

address_timber_marina_tower <- "Wehlistraße 334, 1020 Wien"     
geocoded_address_timber_marina_tower <- geo(address_timber_marina_tower)
data_timber_marina_tower <- st_as_sf(geocoded_address_timber_marina_tower, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Timber Marina Tower (vormals Donaumarina Tower)" ,
         Status = "Still planning",
         Type = "business",
         Size = "Small")

address_quartier_waterfront <- "Waterfront, 1020 Wien"     
geocoded_address_quartier_waterfront <- geo(address_quartier_waterfront)
data_quartier_waterfront <- st_as_sf(geocoded_address_quartier_waterfront, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Waterfront - Ein neues Quartier entsteht" ,
         Status = "Still planning",
         Type = "housing",
         Size = "Medium")

address_wohnquartier_erzherzogkarlstraßesüd <- "Genochplatz 6, 1220 Wien, 1220 Wien"     
geocoded_address_wohnquartier_erzherzogkarlstraßesüd <- geo(address_wohnquartier_erzherzogkarlstraßesüd)
data_wohnquartier_erzherzogkarlstraßesüd <- st_as_sf(geocoded_address_wohnquartier_erzherzogkarlstraßesüd, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Erzherzog-Karl-Straße Süd - Lebenswertes Wohnquartier " ,
         Status = "Still planning",
         Type = "housing",
         Size = "Medium")

address_stadtentwicklungsgebiet_nordwestbahnhof <- "Frachtenbahnhof Wien Nordwestbahnhof, 1200 Wien"     
geocoded_address_stadtentwicklungsgebiet_nordwestbahnhof <- geo(address_stadtentwicklungsgebiet_nordwestbahnhof)
data_stadtentwicklungsgebiet_nordwestbahnhof <- st_as_sf(geocoded_address_stadtentwicklungsgebiet_nordwestbahnhof, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Stadtentwicklungsgebiet Nordwestbahnhof" ,
         Status = "Still planning",
         Type = "housing",
         Size = "Large")    #doppelt verbucht?


# concept development
address_funsport_leopoldau <- "Venediger Au 11, 1020 Wien"     
geocoded_address_funsport_leopoldau <- geo(address_funsport_leopoldau)
data_funsport_leopoldau <- st_as_sf(geocoded_address_funsport_leopoldau, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Sport & Fun Halle - Standortwechsel" ,
         Status = "Still planning",
         Type = "culture",
         Size = "Small")

address_volksschule_selmalagerlöfgasse <- "Selma-Lagerlöf-Gasse 20, 1100 Wien"     
geocoded_address_volksschule_selmalagerlöfgasse <- geo(address_volksschule_selmalagerlöfgasse)
data_volksschule_selmalagerlöfgasse <- st_as_sf(geocoded_address_volksschule_selmalagerlöfgasse, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Volksschule Selma-Lagerlöf-Gasse" ,
         Status = "Still planning",
         Type = "education",
         Size = "Small")

address_klinik_ottakring <- "Montleartstraße 37, 1160 Wien"     
geocoded_address_klinik_ottakring <- geo(address_klinik_ottakring)
data_klinik_ottakring <- st_as_sf(geocoded_address_klinik_ottakring, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Klinik Ottakring - Modernisierung bis 2040" ,
         Status = "Still planning",
         Type = "healthcare",
         Size = "Small")

address_bürokomplex_schöpsstraße <- "Schöpsstraße 2, 1030 Wien"     
geocoded_address_bürokomplex_schöpsstraße <- geo(address_bürokomplex_schöpsstraße)
data_bürokomplex_schöpsstraße <- st_as_sf(geocoded_address_bürokomplex_schöpsstraße, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Schöpsstraße - Erweiterung der bestehenden Bebauung" ,
         Status = "Still planning",
         Type = "business",
         Size = "Small")

address_stadtteilentwicklung_kaiserebersdorf <- "Simmeringer Hauptstraße 140, 1110 Wien"     
geocoded_address_stadtteilentwicklung_kaiserebersdorf <- geo(address_stadtteilentwicklung_kaiserebersdorf)
data_stadtteilentwicklung_kaiserebersdorf <- st_as_sf(geocoded_address_stadtteilentwicklung_kaiserebersdorf, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Stadtteilentwicklungskonzept Kaiserebersdorf" ,
         Status = "Still planning",
         Type = "housing",
         Size = "Medium")

address_stadtteilentwicklung_arsenal <- "Arsenalstraße 1, 1030 Wien"     
geocoded_address_stadtteilentwicklung_arsenal <- geo(address_stadtteilentwicklung_arsenal)
data_stadtteilentwicklung_arsenal <- st_as_sf(geocoded_address_stadtteilentwicklung_arsenal, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Stadtteilentwicklungskonzept Arsenal" ,
         Status = "Still planning",
         Type = "culture",
         Size = "Medium")

address_stadtteilentwicklung_mitte15 <- "Felberstraße 36, 1150 Wien"     
geocoded_address_stadtteilentwicklung_mitte15 <- geo(address_stadtteilentwicklung_mitte15)
data_stadtteilentwicklung_mitte15 <- st_as_sf(geocoded_address_stadtteilentwicklung_mitte15, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Stadtteilentwicklungskonzept Mitte 15" ,
         Status = "Still planning",
         Type = "culture",
         Size = "Medium")

address_stadtteilentwicklung_rothneusiedl <- "Himberger Straße 50, 1100 Wien"     
geocoded_address_stadtteilentwicklung_rothneusiedl <- geo(address_stadtteilentwicklung_rothneusiedl)
data_stadtteilentwicklung_rothneusiedl <- st_as_sf(geocoded_address_stadtteilentwicklung_rothneusiedl, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Rothneusiedl - neues Wohnquartier" ,
         Status = "Still planning",
         Type = "food",
         Size = "Medium")

address_matzleinsdorfer_platz <- "Matzleinsdorfer Platz, 1100 Wien"     
geocoded_address_matzleinsdorfer_platz <- geo(address_matzleinsdorfer_platz)
data_matzleinsdorfer_platz <- st_as_sf(geocoded_address_matzleinsdorfer_platz, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Matzleinsdorfer Platz Süd - Modernisierung" ,
         Status = "Still planning",
         Type = "nature",
         Size = "Small")

address_entwicklungsareal_stmarx <- "St. Marx, 1030 Wien"     
geocoded_address_entwicklungsareal_stmarx <- geo(address_entwicklungsareal_stmarx)
data_entwicklungsareal_stmarx <- st_as_sf(geocoded_address_entwicklungsareal_stmarx, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Entwicklungsareal St. Marx" ,
         Status = "Still planning",
         Type = "culture",
         Size = "Medium")

address_wh_arena <- "Neu Marx, 1030 Wien"     
geocoded_address_wh_arena <- geo(address_wh_arena)
data_wh_arena <- st_as_sf(geocoded_address_wh_arena, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "WH Arena - Neue Multifunktionsarena für Wien" ,
         Status = "Still planning",
         Type = "culture",
         Size = "Small")

address_zielgebiet_donaufeld <- "Donaufeld, 1210 Wien"     
geocoded_address_zielgebiet_donaufeld <- geo(address_zielgebiet_donaufeld)
data_zielgebiet_donaufeld <- st_as_sf(geocoded_address_zielgebiet_donaufeld, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Zielgebiet Donaufeld: Neuer Stadtteil mit großen Grünanteil" ,
         Status = "Still planning",
         Type = "housing",
         Size = "Large")

address_zielgebiet_muthgasse <- "Muthgasse, 1190 Wien"     
geocoded_address_zielgebiet_muthgasse <- geo(address_zielgebiet_muthgasse)
data_zielgebiet_muthgasse <- st_as_sf(geocoded_address_zielgebiet_muthgasse, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Zielgebiet Muthgasse" ,
         Status = "Still planning",
         Type = "housing",
         Size = "Large")

address_zielgebiet_donauraum <- "Meiereistraße 7, 1020 Wien"     
geocoded_address_zielgebiet_donauraum <- geo(address_zielgebiet_donauraum)
data_zielgebiet_donauraum <- st_as_sf(geocoded_address_zielgebiet_donauraum, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Zielgebiet Donauraum" ,
         Status = "Still planning",
         Type = "nature",
         Size = "Large")

address_stadtenwicklungskonzept_hausfeld <- "Hausfeld, 1220 Wien"     
geocoded_address_stadtenwicklungskonzept_hausfeld <- geo(address_stadtenwicklungskonzept_hausfeld)
data_stadtenwicklungskonzept_hausfeld <- st_as_sf(geocoded_address_stadtenwicklungskonzept_hausfeld, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Stadtteilentwicklungskonzept Hausfeld" ,
         Status = "Still planning",
         Type = "housing",
         Size = "Medium")

address_stadtenwicklungskonzept_floridsdorf <- "Brünnerstraße, 1210 Wien"     
geocoded_address_stadtenwicklungskonzept_floridsdorf <- geo(address_stadtenwicklungskonzept_floridsdorf)
data_stadtenwicklungskonzept_floridsdorf <- st_as_sf(geocoded_address_stadtenwicklungskonzept_floridsdorf, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Unser Floridsdorf - Stadtteilentwicklungskonzept Brünner Straße" ,
         Status = "Still planning",
         Type = "housing",
         Size = "Medium")

address_stadtentwicklungsgebiet_amheidjöchl <- "Am Heidjöchl, 1220 Wien"     
geocoded_address_stadtentwicklungsgebiet_amheidjöchl <- geo(address_stadtentwicklungsgebiet_amheidjöchl)
data_stadtentwicklungsgebiet_amheidjöchl <- st_as_sf(geocoded_address_stadtentwicklungsgebiet_amheidjöchl, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Stadtentwicklungsgebiet Am Heidjöchl" ,
         Status = "Still planning",
         Type = "housing",
         Size = "Medium")

address_albrechtskaserne <- "Vorgartenstraße 225, 1020 Wien"     
geocoded_address_albrechtskaserne <- geo(address_albrechtskaserne)
data_albrechtskaserne <- st_as_sf(geocoded_address_albrechtskaserne, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Albrechtskaserne" ,
         Status = "Still planning",
         Type = "law&order",
         Size = "Small")

address_postsportareal <- "Postsportzentrum, 1170 Wien"     
geocoded_address_postsportareal <- geo(address_postsportareal)
data_postsportareal <- st_as_sf(geocoded_address_postsportareal, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Postsportareal - Modernisierung der Sportstätte" ,
         Status = "Still planning",
         Type = "nature",
         Size = "Small")


# grundlagenerhebung
address_borkowskigasse_boku <- "Borkowskigasse, 1190 Wien"     
geocoded_address_borkowskigasse_boku <- geo(address_borkowskigasse_boku)
data_borkowskigasse_boku <- st_as_sf(geocoded_address_borkowskigasse_boku, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Borkowskigasse - BOKU" ,
         Status = "Still planning",
         Type = "education",
         Size = "Small")

address_hebbelplatz <- "Hebbelplatz, 1100 Wien"     
geocoded_address_hebbelplatz <- geo(address_hebbelplatz)
data_hebbelplatz <- st_as_sf(geocoded_address_hebbelplatz, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Hebbelplatz - Nutzung unbebauter Fläche" ,
         Status = "Still planning",
         Type = "housing",
         Size = "Small")

address_stadtentwicklungsgebiet_alteslandgut <- "Altes Landgut, 1100 Wien"     
geocoded_address_stadtentwicklungsgebiet_alteslandgut <- geo(address_stadtentwicklungsgebiet_alteslandgut)
data_stadtentwicklungsgebiet_alteslandgut <- st_as_sf(geocoded_address_stadtentwicklungsgebiet_alteslandgut, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Stadtentwicklungsgebiet Ales Landgut" ,
         Status = "Still planning",
         Type = "housing",
         Size = "Medium")

address_stadtteilentwicklungskonzept_anundunterdertangente <- "Erdbergstraße 200A, 1030 Wien"     
geocoded_address_stadtteilentwicklungskonzept_anundunterdertangente <- geo(address_stadtteilentwicklungskonzept_anundunterdertangente)
data_stadtteilentwicklungskonzept_anundunterdertangente <- st_as_sf(geocoded_address_stadtteilentwicklungskonzept_anundunterdertangente, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Stadtteilentwicklungskonzept An und unter der Tangente" ,
         Status = "Still planning",
         Type = "nature",
         Size = "Medium")

address_kleinerschafberg <- "Korngasse, 1170 Wien"     
geocoded_address_kleinerschafberg <- geo(address_kleinerschafberg)
data_kleinerschafberg <- st_as_sf(geocoded_address_kleinerschafberg, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Kleiner Schafberg" ,
         Status = "Still planning",
         Type = "nature",
         Size = "Medium")

address_ortskern_hirschstetten <- "Hirschstettner Platz, 1220 Wien"     
geocoded_address_ortskern_hirschstetten <- geo(address_ortskern_hirschstetten)
data_ortskern_hirschstetten <- st_as_sf(geocoded_address_ortskern_hirschstetten, coords = c("long", "lat"), crs = 4326) |>
  mutate(Name = "Ortskern Hischstetten" ,
         Status = "Still planning",
         Type = "housing",
         Size = "Small")



data_projects = bind_rows(data_leopold_ungarn_platz, data_ottokar_fischer_gasse, data_bruno_marek_allee, data_praterstern, data_westbahnhof_ikea, data_rösslergasse,
                          data_zielgebiet_donaukanal, data_bildungscampus_berrengasse, data_bildungscampus_attemgasse, data_wohnquartier_spallartgasse, data_begegnungszone_rotenturmstrasse,
                          data_bildungscampus_atzgersdorf,  data_bildungscampus_aron_menczer, data_wohnquartier_unilever_gründe, data_sonnwendviertel,  # all Recently completed ones
                          data_naschmarkt_neugestaltung, data_vienna_business_district_ost, data_vienna_business_district_süd, data_vienna_business_district_nord, data_ostbahn, data_wohnquartier_pilzgasse,
                          data_Bafep21_Freytaggasse, data_entwicklungsgebiet_raffenstättergasse, data_wohnquartier_kurbadstraße, data_stadtrand_siebenhirten, 
                          data_neu_leopoldau, data_bildungscampus_gasometer, data_wohnquartier_amseebogen, data_donau_city,data_wohnhausanlage_dresdnerstraße, data_zentrum_kagran,
                          data_gründerzeit_westgürtel, data_schulcampus_brigittenau, data_semmelweisklinik_areal, data_asparngründe, data_quartier_althan, data_ottowagner_areal, data_stadtquariter_viertelzwei,
                          data_zielgebiet_donaustadt, data_zielgebiet_floridsdorf, data_quartier_süßbrunnerwest, data_kulturzentrum_sagfabrik, data_südraum_favoriten, data_nordwestbahnstraße, data_supergrätzl_favoriten, 
                          data_liesing_mitte, data_sporthalle_jenschikweg, data_urban_gardening_inderwiesenost, data_volksschule_grinzing, data_gemeindebau_epidelauerstraße, data_wohnquartier_wildgarten, 
                          data_wohnquartier_viola_park2, data_wohnquartier_34takt, data_sophienspital, data_wohnhaus_effenbergplatz, data_theresianum, data_wohnen_anderkuhtrift, data_entwicklungsprojekt_kendlerstraße,
                          data_berufsschule_seestadt, data_carre_atzgersdorf, data_wohnquartier_breitenleerstraßesüd, data_wohnquartier_attemsgasse, data_wohnbebauung_amrain, data_busterminal_handelskai, data_aspern_seestadt, 
                          data_nordbahnviertel, data_wohnquartier_kempelenpark, data_wohnquartier_anderschanze, data_entwicklungsprojekt_neueslandgut,  # all currenlty developed ones
                          data_schulneubau_laimäckergasse, data_auva_meidling, data_entwicklungsprojekt_ambrosweg, data_eislaufen_heumarkt, data_ortskern_nussdorf_heiligenstadt, data_arbeitsstätte_karlschäferstraße, data_stadtrand_aspern_hausfeld, 
                          data_betriebsgebiet_stachegasse, data_ortskern_oberlaa, data_entwicklungsprojekt_obereshausfeld, # all adopted ones
                          data_treibstoff_seybelgasse, data_sunken_city, data_höpflerbad, data_karolineperingasse, data_schutzzone_währing, data_kinkplatz, data_feuerwehr_breitenlee, data_porzellanmanufaktur_augarten, data_blockentwicklung_lacknergasse,
                          data_entwicklungsprojekt_dornbacherstraße, data_wohnquartier_tanbruckgasse, data_wohnquartier_kainzgasse, data_hörndlwald, data_entwicklungsprojekt_schrödingerplatz, data_klinik_hitzing, data_wohnquartier_hirschstettnerstraße,
                          data_biotop_wildquell, data_siedlung_werndlgasse, data_bricolage_city_gasometer, data_timber_marina_tower, data_quartier_waterfront, data_wohnquartier_erzherzogkarlstraßesüd, data_stadtentwicklungsgebiet_nordwestbahnhof, # all under dedication process
                          data_funsport_leopoldau, data_volksschule_selmalagerlöfgasse, data_klinik_ottakring, data_bürokomplex_schöpsstraße, data_stadtteilentwicklung_kaiserebersdorf, data_stadtteilentwicklung_arsenal, data_stadtteilentwicklung_mitte15, data_stadtteilentwicklung_rothneusiedl,
                          data_matzleinsdorfer_platz, data_entwicklungsareal_stmarx, data_wh_arena, data_zielgebiet_donaufeld, data_stadtentwicklungsgebiet_amheidjöchl, data_albrechtskaserne, data_postsportareal,   # all Still planning (conceptual)
                          data_borkowskigasse_boku, data_hebbelplatz, data_stadtentwicklungsgebiet_alteslandgut,data_stadtteilentwicklungskonzept_anundunterdertangente, data_kleinerschafberg, data_ortskern_hirschstetten) |>
  select(Name, address, Type, Size, Status, geometry)

data_projects$Size = as.factor(data_projects$Size)
data_projects$Status = as.factor(data_projects$Status)

View(data_projects)

# write to disk
output_path = here("Data/Projects/data_projects.geojson")
# delete old version
unlink(output_path)
write_sf(data_projects, output_path)


mapview(data_projects|> filter(Size == "Large"))


test = read_sf("Data/Projects/data_projects.geojson")
View(test)

test |> filter(Size =="Small")






























