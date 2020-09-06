#Mapas censales según manzana 
#Cargamos nuestro shape con la información relativa a la participación electoral
library(tidyverse)
library(mapview)
library(sf)
library(ggmap)
library(readxl)
library(rgdal)
library(sp)
library(chilemapas)
library(leaflet)
library(leafpop)
#Cargamos nuestro archivo con la información a nivel censal (csv)
municipios<-st_read("C:/Users/pc/Desktop/r13/R13/MANZANA_IND_C17.shp")
##Graficar
paleta <- c("#DCA761", "#CFB567", "#BFBC71", "#9EA887", "#819897")#La paleta de colores que ocuparemos
##Mapeamos nuestro municipio
municipios%>% #DF
  filter(NOM_COMUNA== "RECOLETA")%>% #La comuna a mapear
  ggplot() +
  geom_sf(aes(fill= TOTAL_PERS, geometry = geometry))+ #El argumento por el cual aplicaremos el color
  scale_fill_gradientn(colours = rev(paleta), name = "Poblacion\nCensada")+ #Paleta de colores e identificación 
  labs(title = "Poblacion de Recoleta en Unidades Vecinales")+ #Titulo
  theme(line = element_blank(),  # Quitamos el fono (tema) del mapa
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())

