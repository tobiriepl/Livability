# get landmarks. code by selim (see mail)


zbez.shp = read_sf("ZAEHLBEZIRKOGD/ZAEHLBEZIRKOGDPolygon.shp")

shp2 <- st_read("BEZIRKSGRENZEOGD/BEZIRKSGRENZEOGDPolygon.shp",  options = "ENCODING=WINDOWS-1252")
shp3 <- st_read("STRASSENGRAPHOGD/STRASSENGRAPHOGD.shp",  options = "ENCODING=WINDOWS-1252")
shp5 <- st_read("FLIESSGEWOGD/FLIESSGEWOGDLine.shp",  options = "ENCODING=WINDOWS-1252")


# subset
ring.shp <- shp3[shp3$FEATURENAM %in% c("Schottenring", "Burgring", "Opernring", "Kärntner Ring", "Dr.-Karl-Renner-Ring",
                                        "Schubertring", "Parkring", "Universitätsring", "Stubenring"),]
gurtel.shp <- shp3[shp3$FEATURENAM %in% c("Neubaugürtel", "Landstraßer Gürtel", "Döblinger Gürtel", "Margaretengürtel", "Wiedner Gürtel",
                                          "Währinger Gürtel", "Mariahilfer Gürtel", "Lerchenfelder Gürtel", "Lerchenfelder Gürtel",
                                          "Gumpendorfer Gürtel", "Hernalser Gürtel", "Sechshauser Gürtel", "Gaudenzdorfer Gürtel"),]
donau <- shp5[shp5$NAME %in% c("Neue Donau", "Donau"),]
donaukanal <- shp5[shp5$NAME %in% c("Donaukanal"),]


pdf("ssimap.pdf")
map <- ggplot() +
  geom_sf(data = SSI_DATA, mapping = aes(fill = SSI), color = "white", size = 0.1, show.legend = T) +
  scale_fill_viridis_c() +
  geom_sf(data = shp2, fill = "transparent", color = "black", lwd = 1) +
  geom_sf(data = donau, fill = "transparent", color = "lightblue", lwd = 2.2) +
  geom_sf(data = donaukanal, fill = "transparent", color = "lightblue", lwd = 1.5) +
  #theme_bw()
  new_retro() 
print(map)
dev.off()