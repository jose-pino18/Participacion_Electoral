##Heat mao participación electoral en Chile ###
#Cargamos librerias 
library(tidyverse)
##cargamos la dirección donde se encuentran nuestros datos
urlfile="https://raw.githubusercontent.com/jose-pino18/Participacion_Electoral/master/Participacion%20electoral/Participacion_electoral_RM.csv"

##cargamos los datos
mydata<-read_delim(url(urlfile),",", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                   trim_ws = TRUE)
## renombramos nuestros datos
participacion_RM<-mydata
## Eliminamos una variable que no nos sirve para nuestro analisis, que solamente tiene los numeros de las observaciones
participacion_RM <- select(participacion_RM, -X1)
#Renombramos nuestros datos
names (participacion_RM) = c("Comuna", "Elecciones_2012", "Elecciones_2013", "Elecciones_2013_2V", "Elecciones_2016", "Elecciones_2017",
                             "Elecciones_2017_2V")
#El formato en que vienen nuestros datos no es lo optimo para realizar un mapa de calor, e incluso para el computador es más optimo
#tener los datos de manera ordena en formato tidy (largo)
# tidy data
tidy_participacion_RM<-participacion_RM %>% 
  pivot_longer(c("Elecciones_2012", "Elecciones_2013", "Elecciones_2013_2V", "Elecciones_2016", "Elecciones_2017",
                 "Elecciones_2017_2V"), names_to = "eleccion", values_to = "participacion")
#### Crearemos un mapa de calor con la participación electoral en la RM desde 2012
# Heatmap 
ggplot(tidy_participacion_RM, aes(eleccion, Comuna, fill= participacion)) + 
  geom_tile()+
  scale_fill_distiller(palette = "GrandBudapest") +
  geom_text(aes(label = participacion ))#Para agregar los valores de cada año en el grafico 


ggplot(tidy_participacion_RM, aes(eleccion, Comuna, fill= participacion)) + 
  geom_tile()+
  scale_fill_gradient(low= "lightsteelblue", high= "grey40") +
  geom_text(aes(label = participacion))+
  ggtitle("Participación electoral en la RM desde 2012")

##Referencias
#https://www.r-graph-gallery.com/79-levelplot-with-ggplot2.html
#https://rpubs.com/htejero/212365



                   