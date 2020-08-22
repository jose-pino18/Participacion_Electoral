#######Participación electoral RM Chile####
###Cargamos nuestras librerias 
library(tidyverse)
###### elecciones 2013 (primera vuelta presidencia y elecciones al congreso)####
#Cargamos nuestro dataset de las elecciones de 2013 (1 vuelta, incluye parlamentarias)
participacion_2013 <- read_delim("E:/Documentos/Trabajo/electoral/participacion/e_2013/Vuelta_1_2013.csv", 
                                  ";", escape_double = FALSE, trim_ws = TRUE)

#Las variables categoricas/cualitativas que consideramos relevante para el analisis las convertiremos en factor
participacion_2013$SEXO<-as.factor(participacion_2013$SEXO)
participacion_2013$SUFRAGIO<-as.factor(participacion_2013$SUFRAGIO)
participacion_2013$COMUNA<-as.factor(participacion_2013$COMUNA)
#Filtraremos nuestra base de datos, queremos las comunas de la RM y la gente que participo de ella (un resumen)
participacion<-participacion_2013 %>%
  group_by(COMUNA) %>% 
  filter(REGION=="Metropolitana De Santiago")%>%
  count(SUFRAGIO)

## creamos columnas independientes con la gente que voto y la que se abstuvo
voto<- participacion %>%
  filter(SUFRAGIO=="sufragó")

no_voto<-participacion %>%
  filter(SUFRAGIO=="no sufragó")
## Unimos ambas columnas a un nuevo DF creado, llamado votación
votacion<- voto%>%
  left_join(no_voto, by= "COMUNA")
##Calulamos el total del padron (suma de votantes como no votantes)
votacion<- votacion %>%
  mutate(padron= sum(n.x + n.y))
#Calculamos la participacion electoral a nivel comunal en la RM
votacion<- votacion %>%
  mutate(parti= n.x*100/ padron)
#Renombramos nuestras variables
names (votacion) = c("Comuna", "Votó", "Cantidad", "Abstuvo", "Cantidad", "Padron", "Pres_2013")
#Redondeamos los valores obtenidos de particiacion
votacion$Pres_2013<-round(votacion$Pres_2013,2)
#Seleccionamos nuestras variables a ocupar
Presidenciales_2013<-votacion %>%
  select(Comuna, Pres_2013)
#Pasamos nuestro valor de participación a un numero con dos decimanles.
##### Elecciones municipales 2012 ####
#Cargamos la base de datos a ocupar
Municipales_2012 <- read_delim("E:/Documentos/Trabajo/electoral/participacion/e_2012/VW_VOTARON_2012.csv", 
                               ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                               trim_ws = TRUE) #Este archivo tenía una codificación distinta

Municipales_2012$SEXO<-as.factor(Municipales_2012$SEXO)
Municipales_2012$SUFRAGIO<-as.factor(Municipales_2012$SUFRAGIO)
Municipales_2012$COMUNA<-as.factor(Municipales_2012$COMUNA)
#Filtraremos nuestra base de datos, queremos las comunas de la RM y la gente que participo de ella (un resumen)
participacion<-Municipales_2012 %>%
  group_by(COMUNA) %>% 
  filter(REGION=="Metropolitana De Santiago")%>%
  count(SUFRAGIO)

## creamos columnas independientes con la gente que voto y la que se abstuvo
voto<- participacion %>%
  filter(SUFRAGIO=="sufragó")

no_voto<-participacion %>%
  filter(SUFRAGIO=="no sufragó")
## Unimos ambas columnas a un nuevo DF creado, llamado votación
votacion<- voto%>%
  left_join(no_voto, by= "COMUNA")
##Calulamos el total del padron (suma de votantes como no votantes)
votacion<- votacion %>%
  mutate(padron= sum(n.x + n.y))
#Calculamos la participacion electoral a nivel comunal en la RM
votacion<- votacion %>%
  mutate(parti= n.x*100/ padron)
#Renombramos nuestras variables
names (votacion) = c("Comuna", "Votó", "Cantidad", "Abstuvo", "Cantidad", "Padron", "Mun_2012")
#Redondeamos los valores obtenidos de particiacion
votacion$Mun_2012<-round(votacion$Mun_2012,2)
#Seleccionamos nuestras variables a ocupar
Municipales_2012<-votacion %>%
  select(Comuna, Mun_2012)

##### Elecciones 2013 segunda vuelta ####
#Cargamos el dataset
Elecciones_2013_2V <- read_delim("E:/Documentos/Trabajo/electoral/participacion/e_2013_2V/VW_VOTARON_2013_2V.csv", 
                               ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                               trim_ws = TRUE) #Este archivo tenía una codificación distinta

Elecciones_2013_2V$SEXO<-as.factor(Elecciones_2013_2V$SEXO)
Elecciones_2013_2V$SUFRAGIO<-as.factor(Elecciones_2013_2V$SUFRAGIO)
Elecciones_2013_2V$COMUNA<-as.factor(Elecciones_2013_2V$COMUNA)
#Filtraremos nuestra base de datos, queremos las comunas de la RM y la gente que participo de ella (un resumen)
participacion<-Elecciones_2013_2V %>%
  group_by(COMUNA) %>% 
  filter(REGION=="Metropolitana De Santiago")%>%
  count(SUFRAGIO)

## creamos columnas independientes con la gente que voto y la que se abstuvo
voto<- participacion %>%
  filter(SUFRAGIO=="sufragó")

no_voto<-participacion %>%
  filter(SUFRAGIO=="no sufragó")
## Unimos ambas columnas a un nuevo DF creado, llamado votación
votacion<- voto%>%
  left_join(no_voto, by= "COMUNA")
##Calulamos el total del padron (suma de votantes como no votantes)
votacion<- votacion %>%
  mutate(padron= sum(n.x + n.y))
#Calculamos la participacion electoral a nivel comunal en la RM
votacion<- votacion %>%
  mutate(parti= n.x*100/ padron)
#Renombramos nuestras variables
names (votacion) = c("Comuna", "Votó", "Cantidad", "Abstuvo", "Cantidad", "Padron", "Pres_2013_2V")
#Redondeamos los valores obtenidos de particiacion
votacion$Pres_2013_2V<-round(votacion$Pres_2013_2V,2)
#Seleccionamos nuestras variables a ocupar
Presidenciales_2013_2V<-votacion %>%
  select(Comuna, Pres_2013_2V)

##### Elecciones Municipales 2016 ####
Muni_2016 <- read_delim("E:/Documentos/Trabajo/electoral/participacion/e_2016/PARTICIPACION_2016.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE) #Este archivo tenía una codificación tradicional

Muni_2016$SEXO<-as.factor(Muni_2016$SEXO)
Muni_2016$SUFRAGIO<-as.factor(Muni_2016$SUFRAGIO)
Muni_2016$COMUNA<-as.factor(Muni_2016$COMUNA)
#Filtraremos nuestra base de datos, queremos las comunas de la RM y la gente que participo de ella (un resumen)
participacion<-Muni_2016 %>%
  group_by(COMUNA) %>% 
  filter(REGION=="Metropolitana De Santiago")%>%
  count(SUFRAGIO)

## creamos columnas independientes con la gente que voto y la que se abstuvo
voto<- participacion %>%
  filter(SUFRAGIO=="sufragó")

no_voto<-participacion %>%
  filter(SUFRAGIO=="no sufragó")
## Unimos ambas columnas a un nuevo DF creado, llamado votación
votacion<- voto%>%
  left_join(no_voto, by= "COMUNA")
##Calulamos el total del padron (suma de votantes como no votantes)
votacion<- votacion %>%
  mutate(padron= sum(n.x + n.y))
#Calculamos la participacion electoral a nivel comunal en la RM
votacion<- votacion %>%
  mutate(parti= n.x*100/ padron)
#Renombramos nuestras variables
names (votacion) = c("Comuna", "Votó", "Cantidad", "Abstuvo", "Cantidad", "Padron", "Mun_2016")
#Redondeamos los valores obtenidos de particiacion
votacion$Mun_2016<-round(votacion$Mun_2016,2)
#Seleccionamos nuestras variables a ocupar
Municipales_2016<-votacion %>%
  select(Comuna, Mun_2016)

##### Elecciones 2017 (1V y Parlamentarias)####
Elecciones_2017_1V <- read_delim("E:/Documentos/Trabajo/electoral/participacion/e_2017/VW_VOTARON_2017_1V.csv", 
                                 ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                                 trim_ws = TRUE) 
##Convertimos a factor 
Elecciones_2017_1V$SEXO<-as.factor(Elecciones_2017_1V$SEXO)
Elecciones_2017_1V$SUFRAGIO<-as.factor(Elecciones_2017_1V$SUFRAGIO)
Elecciones_2017_1V$COMUNA<-as.factor(Elecciones_2017_1V$COMUNA)
#Filtraremos nuestra base de datos, queremos las comunas de la RM y la gente que participo de ella (un resumen)
participacion<-Elecciones_2017_1V %>%
  group_by(COMUNA) %>% 
  filter(REGION=="Metropolitana De Santiago")%>%
  count(SUFRAGIO)

## creamos columnas independientes con la gente que voto y la que se abstuvo
voto<- participacion %>%
  filter(SUFRAGIO=="sufragó")

no_voto<-participacion %>%
  filter(SUFRAGIO=="no sufragó")
## Unimos ambas columnas a un nuevo DF creado, llamado votación
votacion<- voto%>%
  left_join(no_voto, by= "COMUNA")
##Calulamos el total del padron (suma de votantes como no votantes)
votacion<- votacion %>%
  mutate(padron= sum(n.x + n.y))
#Calculamos la participacion electoral a nivel comunal en la RM
votacion<- votacion %>%
  mutate(parti= n.x*100/ padron)
#Renombramos nuestras variables
names (votacion) = c("Comuna", "Votó", "Cantidad", "Abstuvo", "Cantidad", "Padron", "Pres_2017")
#Redondeamos los valores obtenidos de particiacion
votacion$Pres_2017<-round(votacion$Pres_2017,2)
#Seleccionamos nuestras variables a ocupar
Presidenciales_2017<-votacion %>%
  select(Comuna, Pres_2017)

##### Elecciones 2017 segunda vuelta ####
#Cargamos datos
Elecciones_2017_2V <- read_delim("E:/Documentos/Trabajo/electoral/participacion/e_2017_2V/VW_VOTARON_2017_2V.csv", 
                                 ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                                 trim_ws = TRUE) 
##Convertimos a factor 
Elecciones_2017_2V$SEXO<-as.factor(Elecciones_2017_2V$SEXO)
Elecciones_2017_2V$SUFRAGIO<-as.factor(Elecciones_2017_2V$SUFRAGIO)
Elecciones_2017_2V$COMUNA<-as.factor(Elecciones_2017_2V$COMUNA)
#Filtraremos nuestra base de datos, queremos las comunas de la RM y la gente que participo de ella (un resumen)
participacion<-Elecciones_2017_2V %>%
  group_by(COMUNA) %>% 
  filter(REGION=="Metropolitana De Santiago")%>%
  count(SUFRAGIO)

## creamos columnas independientes con la gente que voto y la que se abstuvo
voto<- participacion %>%
  filter(SUFRAGIO=="sufragó")

no_voto<-participacion %>%
  filter(SUFRAGIO=="no sufragó")
## Unimos ambas columnas a un nuevo DF creado, llamado votación
votacion<- voto%>%
  left_join(no_voto, by= "COMUNA")
##Calulamos el total del padron (suma de votantes como no votantes)
votacion<- votacion %>%
  mutate(padron= sum(n.x + n.y))
#Calculamos la participacion electoral a nivel comunal en la RM
votacion<- votacion %>%
  mutate(parti= n.x*100/ padron)
#Renombramos nuestras variables
names (votacion) = c("Comuna", "Votó", "Cantidad", "Abstuvo", "Cantidad", "Padron", "Pres_2017_2V")
#Redondeamos los valores obtenidos de particiacion
votacion$Pres_2017_2V<-round(votacion$Pres_2017_2V,2)
#Seleccionamos nuestras variables a ocupar
Presidenciales_2017_2V<-votacion %>%
  select(Comuna, Pres_2017_2V)

##### Posible Mejoras ####
#Hacer una función o loop para no repetir tanto el proceso por cada una de las elecciones


####  unir datasets ####
participacion_RM<- Presidenciales_2013 %>%
  left_join(Municipales_2012, by= "Comuna")

participacion_RM<- participacion_RM %>%
  left_join(Presidenciales_2013_2V, by= "Comuna")

participacion_RM<- participacion_RM %>%
  left_join(Municipales_2016, by= "Comuna")

participacion_RM<- participacion_RM %>%
  left_join(Presidenciales_2017, by= "Comuna")

participacion_RM<- participacion_RM %>%
  left_join(Presidenciales_2017_2V, by= "Comuna")

participacion_RM<-participacion_RM %>%
  select("Comuna", "Mun_2012", "Pres_2013", "Pres_2013_2V","Mun_2016", "Pres_2017", "Pres_2017_2V")
#Guardar nuestra base de datos como archivo csv
write.csv(participacion_RM,"Participacion_electoral_RM.csv")
