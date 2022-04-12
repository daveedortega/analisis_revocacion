#########################################
# Análisis de la Revocación de Mandato  #
# En CDMX vs. Alcaldías Perdidas        #
# D. Ortega
#########################################

## Preparamos Entorno ----
pacman::p_load(tidyverse,janitor,readxl,rgdal)

rm(list=ls())

dev.off()
## Cargamos Datos ----
#PREP a 11/04/2022 11:53 (UTC-5) de la Revocación de Mandato, Código de Integridad 0e92c6a87d896c5fbba08d5b99a1b34fba38ae9a49f094acdf69a59c1418dcb6 
# De https://computosrm2022.ine.mx/base-de-datos 
rm_2022 <- read_csv("input/20220411_1153_COMPUTOS_RM2022.csv",
                    locale = locale(encoding = "ISO-8859-1"), 
                    skip = 5) #Cinco Filas Vacías, las saltamos
eleccion_2021 <- read_excel("input/ResultadosAL-2021-06-10-16-09-14.xlsx", 
                            skip = 6) #Seis filas vacías, las saltamos

mapa_secciones <- readOGR("mapa_secciones/seccion_final.shp")
#Queremos sólamente aquellos en cdmx

## Filtramos la Revocación para CDMX ----
table(rm_2022$ENTIDAD)  
cdmx_rm_2022 <- rm_2022 %>% filter(ENTIDAD == "CIUDAD DE MÉXICO")
table(cdmx_rm_2022$SECCIÓN)
length(unique(cdmx_rm_2022$SECCIÓN)) #sólo se pusieron en 2159 secciones

# Nos quedamos con: ENTIDAD, DISTRITO_FEDERAL, SECCIÓN, QUE_SE_LE_REVOQUE_EL_MANDATO_POR_PÉRDIDA_DE_LA_CONFIANZA,
#  OBSERVACIONES, QUE_SIGA_EN_LA_PRESIDENCIA_DE_LA_REPÚBLICA, LISTA_NOMINAL, NULOS, TOTAL, 

cdmx_rm_2022 <- cdmx_rm_2022 %>% select(ENTIDAD, DISTRITO_FEDERAL, SECCIÓN, QUE_SE_LE_REVOQUE_EL_MANDATO_POR_PÉRDIDA_DE_LA_CONFIANZA, 
                        QUE_SIGA_EN_LA_PRESIDENCIA_DE_LA_REPÚBLICA, NULOS, TOTAL_VOTOS_CALCULADOS, LISTA_NOMINAL, OBSERVACIONES)
colnames(cdmx_rm_2022) <-  make_clean_names(names(cdmx_rm_2022))

class(cdmx_rm_2022$seccion) #hacemos numérico
cdmx_rm_2022$seccion <- as.numeric(cdmx_rm_2022$seccion)
cdmx_rm_2022$que_se_le_revoque_el_mandato_por_perdida_de_la_confianza <- as.numeric(cdmx_rm_2022$que_se_le_revoque_el_mandato_por_perdida_de_la_confianza)
cdmx_rm_2022$que_siga_en_la_presidencia_de_la_republica <- as.numeric(cdmx_rm_2022$que_siga_en_la_presidencia_de_la_republica)
cdmx_rm_2022$nulos <- as.numeric(cdmx_rm_2022$nulos)
cdmx_rm_2022$total_votos_calculados <- as.numeric(cdmx_rm_2022$total_votos_calculados)
cdmx_rm_2022$lista_nominal <- as.numeric(cdmx_rm_2022$lista_nominal)

cdmx_rm_2022$observaciones=NULL #Por el momento sin observaciones
#Agrupamos por sección electoral
cdmx_rm_2022 <- cdmx_rm_2022 %>% group_by(entidad,distrito_federal,seccion) %>% 
  summarise(que_siga=sum(que_siga_en_la_presidencia_de_la_republica),que_se_revoque=sum(que_se_le_revoque_el_mandato_por_perdida_de_la_confianza),
            votos_nulos=sum(nulos),total=sum(total_votos_calculados),lista_nom_sec=sum(lista_nominal)) %>% 
  arrange(seccion)

## Modificamos base de Elección 2021 ----
colnames(eleccion_2021) <- make_clean_names(names(eleccion_2021))
# quitamos NAs
eleccion_2021 <- eleccion_2021[-which(is.na(eleccion_2021$seccion)),] 
# Quitamos Casilla para poder unir por sección
eleccion_2021$casilla=NULL
#unimos todos los votos de los partidos
eleccion_2021 <- eleccion_2021 %>% mutate(MORENA=morena+pvem_pt_morena+pt_morena+pvem_morena) %>% 
  mutate(PRIANRD = pan+pri+prd+pan_pri_prd+pan_pri+pan_prd+pri_prd+pvem_pt) %>% 
  mutate(otros = rsp+fxm+rop+pes+haac+prmc+ecr+evfa+agp+rgs+elige) %>% 
  mutate(nulos=votos_nulos+votos_para_candidatos_no_registrados) %>% 
  select(demarcacion,seccion,MORENA,PRIANRD,pt,mc,pvem,otros,nulos,votacion_total_emitida) 

# test <- c("morena","pvem_pt_morena","pt_morena","pvem_morena", "pan","pri","prd","pan_pri_prd",
#           "pan_pri","pan_prd","rsp","fxm","rop","pes","haac","prmc","ecr","evfa","agp","rgs",
#           "votos_nulos","votos_para_candidatos_no_registrados","mc","pt","pvem","elige","pvem_pt","pri_prd")
# colnames(eleccion_2021)[!colnames(eleccion_2021) %in% test]

#Agrupamos funalmente por demarcación y sección

eleccion_2021 <- eleccion_2021 %>% group_by(demarcacion,seccion) %>% 
  summarise(MORENA=sum(MORENA),PRIANRD=sum(PRIANRD),pt=sum(pt),
            mc=sum(mc),pvem=sum(pvem),otros=sum(otros),nulos=sum(nulos),votacion_total_emitida=sum(votacion_total_emitida)) %>% 
  arrange(seccion)

## Queremos porcentaje de Participación ----

cdmx_rm_2022 <- cdmx_rm_2022 %>% mutate(porcentaje_participacion=total/lista_nom_sec)

## Unimos a través de Secciones Electorales Revocacion y Eleccion 2021----
## NO se pusieron casillas para TODAS las secciones electorales. 
secciones_2022 <- unique(eleccion_2021$seccion)[unique(eleccion_2021$seccion) %in% unique(cdmx_rm_2022$seccion)]
# cdmx_rm_2022 <- as.data.frame(cdmx_rm_2022)
## Extraemos secciones de los poligonos ----
secciones_electorales<- as.data.frame(mapa_secciones)
secciones_electorales <- secciones_electorales %>% separate(Name,c("nom","seccion"),sep = "-Secc-")#extraemos secciones
secciones_electorales$seccion <- as.numeric(secciones_electorales$seccion)
# Unimos con revocacción ----
cdmx_secciones_2022 <- left_join(secciones_electorales,cdmx_rm_2022,by="seccion")
# Unimos con eleccion 2021 ----
cdmx_final <- left_join(cdmx_secciones_2022,eleccion_2021,by="seccion")
#Limpiamos final
cdmx_final <- cdmx_final %>% select(seccion,demarcacion,lista_nom_sec,que_siga,que_se_revoque,votos_nulos,total,
                      MORENA,PRIANRD,pt,mc,pvem,otros,nulos,votacion_total_emitida)

mapa_final <- cbind(mapa_secciones,cdmx_final)

## Escribimos mapa final ----

writeOGR(mapa_final,dsn="output/",layer="comparacion_secciones_cdmx",driver = "ESRI Shapefile")

### Analisis de las secciones que no estan vacías ----

cdmx_revocacion <- cdmx_final[!is.na(cdmx_final$lista_nom_sec),]

cdmx_revocacion <- cdmx_revocacion %>% mutate(diferencia_oposicion = PRIANRD - que_siga)

alcaldias_revocacion <- 
  cdmx_revocacion %>% select(demarcacion,lista_nom_sec,que_siga,PRIANRD,diferencia_oposicion) %>% 
  group_by(demarcacion) %>% 
  summarise(lista_nominal_alcaldia_2022 = sum(lista_nom_sec),que_siga_alcaldia = sum(que_siga),oposicion_2021 = sum(PRIANRD),diferencia=sum(diferencia_oposicion))

## Análisis Total voto 2021 vs. total que siga  ----
#quitamos nas raros
cdmx_final <- cdmx_final[-which(is.na(cdmx_final$demarcacion)),]


cdmx_final %>% group_by(demarcacion) %>% 
  summarise(total_que_siga = sum(que_siga,na.rm = T),total_elecciones = sum(votacion_total_emitida),total_oposicion = sum(PRIANRD)) %>% 
  mutate(quesiga_oposicion=total_que_siga - total_oposicion)














#Jugar con Leaflet ----
library(leaflet)


bins <- quantile(cdmx_final$que_siga, probs=seq(0.05,1, by=0.05),na.rm=T)
bins <- factor(bins)
bins <- levels(bins)
pal <- colorBin("YlOrRd", domain = mapa_final$que_siga, bins = c(15,50,100,282,412,500,600,800,1000))

mapa_final$seccion <- as.integer(mapa_final$seccion)

labels <- sprintf(
  "<strong>Sección %g</strong><br/>Que Siga 2022: %g <br/> Oposicion 2021: %g",
  mapa_final$seccion, mapa_final$que_siga, mapa_final$PRIANRD
) %>% lapply(htmltools::HTML)

leaflet(mapa_final) %>% addTiles() %>% 
  addPolygons(fillColor = ~pal(que_siga),
              weight = 0.25,
              opacity = 1,
              color = "white",
              dashArray = "1",
              fillOpacity = 0.7,
              highlightOptions = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))


## Secciones activas 2022 ----


mapa_2022 <- mapa_final[-which(is.na(mapa_final$que_siga)),]

bins <- quantile(mapa_2022$que_siga, probs=seq(0.05,1, by=0.05),na.rm=T)

pal <- colorBin("YlOrRd", domain = mapa_final$que_siga, bins = c(15,50,100,282,412,500,600,800,3000))


labels <- sprintf(
  "<strong>Sección %g</strong><br/>Que Siga 2022: %g <br/> Oposicion 2021: %g",
  mapa_2022$seccion, mapa_2022$que_siga, mapa_2022$PRIANRD
) %>% lapply(htmltools::HTML)

leaflet(mapa_2022) %>% addTiles() %>% 
  addPolygons(fillColor = ~pal(que_siga),
              weight = 0.25,
              opacity = 1,
              color = "white",
              dashArray = "1",
              fillOpacity = 0.7,
              highlightOptions = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")
              
  






