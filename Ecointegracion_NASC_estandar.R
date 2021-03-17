# Script para manipular datos csv del NASC
#Leer los csv 
setwd("D:/2021/nasc_prueba") #Leer el espacio de trabajo 
transectos <- list.files(".", pattern = "*.csv")
crucero <- "JCFINP20_02"
species <- "general"
#Asignar nombre de la capa NASC general
nombreNASCgeneral <- paste0(crucero,"NASC_Demer")
nombreNASCecointegracion <- paste0(crucero,"NASC_sum_meanDemer")
nombreNASCcapa05_15 <- paste0(crucero,"_NASC_capa05_15Demer")
nombreNASCcapa15_20 <- paste0(crucero,"_NASC_capa15_20Demer")
nombreNASCcapa20_30 <- paste0(crucero,"_NASC_capa20_30Demer")
nombreNASCcapa30_40 <- paste0(crucero,"_NASC_capa30_40Demer")
nombreNASCcapa40_50 <- paste0(crucero,"_NASC_capa40_50Demer")
nombreNASCcapa50_75 <- paste0(crucero,"_NASC_capa50_75Demer")
nombreNASCcapa75_100 <- paste0(crucero,"_NASC_capa75_100Demer")
nombreNASCcapa100_200 <- paste0(crucero,"_NASC_capamayora100Demer")

#Librerias para unir todos los csv
library(plyr)
library(tidyverse)
library(lubridate)
#Union de los csv, crea columna crucero, longitud y latitud
datos <- ldply(transectos, read_csv) %>% mutate(cruise= crucero, Latitud = Lat_M, Longitud = Lon_M)%>% 
  tibble::rowid_to_column("ID")
#write.csv(datos, file = paste0(crucero,"_NASCDemer.csv"))###Si se desea guardar el archivo

#Crear el rango de la noche y dia
datos1 <- datos %>%  
  mutate(time2 = format(Time_M, format="%H:%M:%S")) %>%
  filter(time2 > "00:30:00" & time2 < "13:20:00") %>%
  dplyr::select(-time2) %>% mutate(Tiempo = "Nocturno")

datos2 <- datos %>%  
  mutate(time2 = format(Time_M, format="%H:%M:%S")) %>%
  filter(time2 <="00:30:00") %>%
  dplyr::select(-time2) %>% mutate(Tiempo = "Diurno")

datos3 <- datos %>%  
  mutate(time2 = format(Time_M, format="%H:%M:%S")) %>%
  filter(time2 >="13:20:00") %>%
  dplyr::select(-time2) %>% mutate(Tiempo = "Diurno") 

#Unir los datos
datostiempo <- rbind(datos1, datos2, datos3) %>% dplyr::mutate(dia = paste(ymd(Date_M),Time_M)) %>% 
  dplyr::mutate(date_timeUTC = ymd_hms(dia, tz = "UTC"))

#convertir a tiempo
datostiempo$Date_M <- ymd(datostiempo$Date_M)
#Cargar librer?as espaciales
library(sf)
#Crear capa para limpiar los datos que esten fuera de los transectos y Crear Clave primaria 
capaNASC <- st_as_sf(datostiempo, coords = c("Longitud", "Latitud"), crs = 4326) %>% 
  mutate(ID_PK = paste0(cruise,"_",ID))

#Cargar la capa de Time Zones
time_zonesOcean <- st_read("D:/2021/capas_generales/time_zones.gpkg",layer = "ne_10m_time_zones") %>% 
  st_crop(capaNASC) %>% select(tz_name1st)

#Definicion de las zonas horarias
origin_year <- ymd_hms("2020-01-01 00:00:00")
Los_Angeles <- "America/Los_Angeles"
New_York <- "America/New_York"
Denver <- "America/Denver"
Chicago <- "America/Chicago"

#Intersectar para obtener la zona horaria
capaNASC_tz <- capaNASC %>% st_intersection(time_zonesOcean) %>% 
  dplyr::mutate(local_date = case_when(tz_name1st == Denver ~ lubridate::with_tz(date_timeUTC, tz = Denver),
                                       tz_name1st == Chicago ~ lubridate::with_tz(date_timeUTC, tz = Chicago),
                                       tz_name1st == Los_Angeles ~ lubridate::with_tz(date_timeUTC, tz = Los_Angeles),
                                       tz_name1st == New_York ~ lubridate::with_tz(date_timeUTC, tz = New_York))) %>% 
  dplyr::mutate(date = format(local_date, "%Y-%m-%d"), time = format(local_date, "%H:%M:%S"), fid = ID) %>% 
  dplyr::select(-c(tz_name1st,dia, date_timeUTC,local_date)) %>% dplyr::select(fid, ID:Time_M, date,time, Lat_M:ID_PK)

#convertir a minusculas las columnas
colnames(capaNASC_tz) <- tolower(names(capaNASC_tz))
#Convertir a character el tiempo
capaNASC_tz$time_m <- as.character(capaNASC_tz$time_m)

#Exportar CAPA ESTANDAR 
capaNASC_tz_estandar <- capaNASC_tz %>% dplyr::select(fid, process_id:dist_m, date,time, noise_sv_1m:cruise, geometry) 
#Crear nueva carpeta para exportar
dir.create('./capas', recursive = TRUE)

#Exportar como geopackage para limpiar los datos y exportar capa estandar
st_write(capaNASC_tz, paste0("./capas/",nombreNASCgeneral,".gpkg"))
st_write(capaNASC_tz_estandar, paste0("./capas/",nombreNASCgeneral,"Estandar.gpkg"))

##### HASTA AQUI Y SE LIMPIAN LOS DATOS EN UN SIG GRAFICO Y SE REALIZA LA ECOINTEGRACION

datos_sf <- st_read(paste0("./capas/",nombreNASCgeneral,".gpkg"))

#Sumar el NASC general
#Concatenar las coordenadas
datos_sf$coor_concatenadas <- paste0(datos_sf$lon_m,"-",datos_sf$lat_m)
#Extraer los datos para concatenar con la tabla de la suma
datos_concatenar <- datos_sf %>% dplyr::select(id,date_m,time_m, date, time, lat_m, lon_m, tiempo, cruise,geom, coor_concatenadas)

#Realizar la suma y promedio de los datos
nasc_sum_meancon <- datos_sf %>% group_by(coor_concatenadas) %>% 
  summarize(sum_nasc = round(sum(nasc, na.rm = TRUE),4), mean_nasc = round(mean(nasc, na.rm = TRUE),4)) %>% st_drop_geometry() %>% 
  inner_join(datos_concatenar, by = "coor_concatenadas") %>% arrange(id)
#nasc_suma2 <- aggregate(nasc ~coor_concatenadas, FUN =sum, data=datos_sf) %>% rename(sum_nasc = nasc)
#nasc_mean <- aggregate(nasc ~coor_concatenadas, FUN =mean, data=datos_sf)%>% rename(mean_nasc = nasc)

#Unir los datos
#nasc_sum_mean <- merge(nasc_suma, nasc_mean, by= "coor_concatenadas")

#realizar la concatenacion de los datos
#nasc_sum_meancon <- right_join(datos_concatenar,nasc_suma,  by = "coor_concatenadas")
#Eliminar las filas duplicadas y Crear un id al dataframe 
nasc_sum_meanconfin <- nasc_sum_meancon[!duplicated(nasc_sum_meancon$coor_concatenadas),] %>% 
  tibble::rowid_to_column("ID") %>% dplyr::select(-(coor_concatenadas)) %>% 
  mutate(id_pk = paste0(cruise,"_",ID), species = species) %>% 
  dplyr::rename(fid = ID) %>% dplyr::select(date,time, cruise, sum_nasc, mean_nasc,species,geom)

#Exportar como shapefile o geopackage
st_write(nasc_sum_meanconfin, paste0("./capas/",nombreNASCecointegracion,"Estandar.gpkg"))


#Separar las capas por profundidades y Extraer los datos a concatenar de cada capa
capa05_15 <- filter(datos_sf, depth_mean <= 15)
capa15_20 <- filter(datos_sf, depth_mean >= 15 & depth_mean < 20) 
capa20_30 <- filter(datos_sf, depth_mean >= 20 & depth_mean < 30) 
capa30_40 <- filter(datos_sf, depth_mean >= 30 & depth_mean < 40) 
capa40_50 <- filter(datos_sf, depth_mean >= 40 & depth_mean < 50) 
capa50_75 <- filter(datos_sf, depth_mean >= 50 & depth_mean < 75) 
capa75_100 <- filter(datos_sf, depth_mean >= 75 & depth_mean < 100) 
capa100_ <- filter(datos_sf, depth_mean >= 100)

#Extraer los datos a concatenar de cada capa
capa05_15col <- dplyr::select(capa05_15, date_m,time_m, date,time, lat_m, lon_m, tiempo, cruise, geom, coor_concatenadas)
capa15_20col <- dplyr::select(capa15_20, date_m,time_m, date,time, lat_m, lon_m, tiempo, cruise, geom, coor_concatenadas)
capa20_30col <- dplyr::select(capa20_30, date_m,time_m, date,time, lat_m, lon_m, tiempo, cruise, geom, coor_concatenadas)
capa30_40col <- dplyr::select(capa30_40, date_m,time_m, date,time, lat_m, lon_m, tiempo, cruise, geom, coor_concatenadas)
capa40_50col <- dplyr::select(capa40_50, date_m,time_m, date,time, lat_m, lon_m, tiempo, cruise, geom, coor_concatenadas)
capa50_75col <- dplyr::select(capa50_75, date_m,time_m, date,time, lat_m, lon_m, tiempo, cruise, geom, coor_concatenadas)
capa75_100col <- dplyr::select(capa75_100, date_m,time_m, date,time, lat_m, lon_m, tiempo, cruise, geom, coor_concatenadas)
capa100_col <- dplyr::select(capa100_, date_m,time_m, date,time, lat_m, lon_m, tiempo, cruise, geom, coor_concatenadas)

#Realizar la suma de los datos
capa05_15_nasc_suma <- aggregate(nasc ~coor_concatenadas, FUN =sum, data=capa05_15) %>% rename(sum_nasc = nasc)
capa15_20_nasc_suma <- aggregate(nasc ~coor_concatenadas, FUN =sum, data=capa15_20) %>% rename(sum_nasc = nasc)
capa20_30_nasc_suma <- aggregate(nasc ~coor_concatenadas, FUN =sum, data=capa20_30) %>% rename(sum_nasc = nasc)
capa30_40_nasc_suma <- aggregate(nasc ~coor_concatenadas, FUN =sum, data=capa30_40) %>% rename(sum_nasc = nasc)
capa40_50_nasc_suma <- aggregate(nasc ~coor_concatenadas, FUN =sum, data=capa40_50) %>% rename(sum_nasc = nasc)
capa50_75_nasc_suma <- aggregate(nasc ~coor_concatenadas, FUN =sum, data=capa50_75) %>% rename(sum_nasc = nasc)
capa75_100_nasc_suma <- aggregate(nasc ~coor_concatenadas, FUN =sum, data=capa75_100) %>% rename(sum_nasc = nasc)
capamayor100_nasc_suma <- aggregate(nasc ~coor_concatenadas, FUN =sum, data=capa100_) %>% rename(sum_nasc = nasc)

#realizar la concatenacion de los datos
capa05_15_nasc_sumacol <- inner_join(capa05_15col, capa05_15_nasc_suma, by = "coor_concatenadas")
capa15_20_nasc_sumacol <- inner_join(capa15_20col, capa15_20_nasc_suma, by = "coor_concatenadas") 
capa20_30_nasc_sumacol <- inner_join(capa20_30col, capa20_30_nasc_suma, by = "coor_concatenadas") 
capa30_40_nasc_sumacol <- inner_join(capa30_40col, capa30_40_nasc_suma, by = "coor_concatenadas") 
capa40_50_nasc_sumacol <- inner_join(capa40_50col, capa40_50_nasc_suma, by = "coor_concatenadas") 
capa50_75_nasc_sumacol <- inner_join(capa50_75col, capa50_75_nasc_suma, by = "coor_concatenadas") 
capa75_100_nasc_sumacol <- inner_join(capa75_100col, capa75_100_nasc_suma, by = "coor_concatenadas") 
capamayor100_nasc_sumacol <- inner_join(capa100_col, capamayor100_nasc_suma, by = "coor_concatenadas")

#Eliminar las filas duplicadas
capa05_15_nasc_sumacolfin <- capa05_15_nasc_sumacol[!duplicated(capa05_15_nasc_sumacol$coor_concatenadas),] %>% 
  tibble::rowid_to_column("ID") %>% dplyr::select(-(coor_concatenadas))%>% mutate(id_pk = paste0(cruise,"_",ID))
capa15_20_nasc_sumacolfin <- capa15_20_nasc_sumacol[!duplicated(capa15_20_nasc_sumacol$coor_concatenadas),] %>% 
  tibble::rowid_to_column("ID") %>% dplyr::select(-(coor_concatenadas))%>% mutate(id_pk = paste0(cruise,"_",ID))
capa20_30_nasc_sumacolfin <- capa20_30_nasc_sumacol[!duplicated(capa20_30_nasc_sumacol$coor_concatenadas),] %>% 
  tibble::rowid_to_column("ID") %>% dplyr::select(-(coor_concatenadas))%>% mutate(id_pk = paste0(cruise,"_",ID))
capa30_40_nasc_sumacolfin <- capa30_40_nasc_sumacol[!duplicated(capa30_40_nasc_sumacol$coor_concatenadas),] %>% 
  tibble::rowid_to_column("ID") %>% dplyr::select(-(coor_concatenadas))%>% mutate(id_pk = paste0(cruise,"_",ID))
capa40_50_nasc_sumacolfin <- capa40_50_nasc_sumacol[!duplicated(capa40_50_nasc_sumacol$coor_concatenadas),] %>% 
  tibble::rowid_to_column("ID") %>% dplyr::select(-(coor_concatenadas))%>% mutate(id_pk = paste0(cruise,"_",ID))
capa50_75_nasc_sumacolfin <- capa50_75_nasc_sumacol[!duplicated(capa50_75_nasc_sumacol$coor_concatenadas),] %>% 
  tibble::rowid_to_column("ID") %>% dplyr::select(-(coor_concatenadas))%>% mutate(id_pk = paste0(cruise,"_",ID))
capa75_100_nasc_sumacolfin <- capa75_100_nasc_sumacol[!duplicated(capa75_100_nasc_sumacol$coor_concatenadas),] %>% 
  tibble::rowid_to_column("ID") %>% dplyr::select(-(coor_concatenadas))%>% mutate(id_pk = paste0(cruise,"_",ID))
capa100_nasc_sumacolfin <- capamayor100_nasc_sumacol[!duplicated(capamayor100_nasc_sumacol$coor_concatenadas),] %>% 
  tibble::rowid_to_column("ID") %>% dplyr::select(-(coor_concatenadas))%>% mutate(id_pk = paste0(cruise,"_",ID))

#Exportar como shapefile o gpkg
st_write(capa05_15_nasc_sumacolfin, paste0("./capas/",nombreNASCcapa05_15,".shp"))
st_write(capa15_20_nasc_sumacolfin, paste0("./capas/",nombreNASCcapa15_20,".shp"))
st_write(capa20_30_nasc_sumacolfin, paste0("./capas/",nombreNASCcapa20_30,".shp"))
st_write(capa30_40_nasc_sumacolfin, paste0("./capas/",nombreNASCcapa30_40,".shp"))
st_write(capa40_50_nasc_sumacolfin, paste0("./capas/",nombreNASCcapa40_50,".shp"))
st_write(capa50_75_nasc_sumacolfin, paste0("./capas/",nombreNASCcapa50_75,".shp"))
st_write(capa75_100_nasc_sumacolfin, paste0("./capas/",nombreNASCcapa75_100,".shp"))
st_write(capa100_nasc_sumacolfin, paste0("./capas/",nombreNASCcapa100_200,".shp"))

