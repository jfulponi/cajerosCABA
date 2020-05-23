library(maptools)
library(ggmap)
library(rgdal)
library(leaflet)
library(viridis)
library(httr)
library(jsonlite)
library(tidyverse)
library(htmltools)
library(sp)
library(sf)
library(mapview)
library(leaflet)
library(leafem)
library(dplyr)
library(rgeos)
library(stplanr)
library(magicfor)
library(raster)
library(covdata)

cajeros <- readOGR(".", "cajeros-automaticos")

censo <- readOGR(".","informacion_censal_por_radio_2010")

cajerosmt <- spTransform(cajeros, CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))

censomt <- spTransform(censo, CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))

grilla <- readOGR(".", "GrillaHexPB")

buffer <- gBuffer(cajerosmt, width = 500, byid=TRUE)

pobcaj2 <- intersect(buffer, censomt)

plot(pobcaj2)

pobcajsf<- st_as_sf(pobcaj2)

pobcajsf2<- st_drop_geometry(pobcajsf)

pobcajsf2<-  pobcajsf2 %>%
  group_by(RADIO_I)%>%
  summarise(POBLACI = max(POBLACI),
         cant.caj = n(),
         sum.caj = sum(as.numeric(terminales)))
  
censosf <- st_as_sf(censo)

pobcajsffinal <- merge(pobcajsf2, censosf)

pobcajsffinal$cajpers <- pobcajsffinal$cant.caj/pobcajsffinal$POBLACI

pobcajsffinal <- st_as_sf(pobcajsffinal)


pobsp <- as_Spatial(pobcajsffinal)
pobsp <- spTransform(pobsp, CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))
grilla <- spTransform(grilla, CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))

hex <- raster::intersect(grilla, pobsp)

hexsf<-st_as_sf(hex)

hexsf<-hexsf %>%
  group_by(id)%>%
  summarise(caj = sum(cant.caj),
            terminales = sum(sum.caj),
            pob = sum(POBLACI))

hexsf$cajpers <- hexsf$caj/hexsf$pob

hexsf <- st_as_sf(hexsf)

colors <- c("#281A2CFF", "yellow", "darkorange", "darkmagenta")

ggplot(hexsf)+
  geom_sf(mapping = aes(fill= caj, colour = caj))+
   scale_fill_gradientn(colours=colors)+
  scale_colour_gradientn(colours=colors)+
  labs(title="Cajeros.", 
       subtitle = "Según radio de 500 metros.", caption = "Elaborado por @jfulponi. Fuente: Buenos Aires Data", 
       colour = "Cajeros", fill = "Cajeros")+
  theme(panel.background = element_rect(fill="#281A2CFF"), 
        panel.grid = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), panel.border = element_rect(colour = "darkorange", fill = NA, size = 4)) +
  ggsave("cajeros term.png")
