##### Script PARA ECOINTEGRACION DEL NASC Y ESTANDARIZAR #####
library(tidyverse)
library(lubridate)
library(sf)

setwd("D:/2021/JCFINP2110") #Leer el espacio de trabajo 
transectos <-  list.files(path  = "nasc", recursive = T, full.names = T, pattern = ".csv")
crucero <- "jcfinp2110"
species <- "Pelágicos menores" # De que especie es el NASC
template <- "NOAA" #Nombre de la plantilla
#Asignar nombre de la capa NASC general
nombreNASCgeneral <- paste0(crucero,"nasc_")
nombreNASCecointegracion <- paste0(crucero,"nasc_sum_mean")

#Obtener los nombres de los archivos, quitar el path y lo que sea necesario
nombres_arc <- transectos %>% str_remove_all("nasc/") %>% str_remove_all(".csv") %>% 
  str_remove_all("_NOAA_JCFINP21-10") %>% str_remove_all("_")

##### UNION DE LOS CSV #####
datos <- purrr::map(.x = transectos, .f = read_csv) %>% 
  map(~mutate(.x, cruise = toupper(crucero), Latitud = Lat_M, Longitud = Lon_M))%>% 
  map2(.y = nombres_arc, ~mutate(.x, transects = .y)) %>%
  reduce(rbind) %>% tibble::rowid_to_column("ID")
#write.csv(datos, file = paste0(crucero,"NASC",template,".csv"))###Si se desea guardar el archivo

##### CREAR RANGO PERIODO NOCTURNO Y DIURNO #####
datostiempo <- datos %>% 
  mutate(time2 = format(Time_M, format="%H:%M:%S"), 
         Tiempo = case_when(time2 > "00:30:00" & time2 < "13:20:00" ~ "Nocturno",
                            time2 <="00:30:00" ~ "Diurno",
                            time2 >="13:20:00" ~ "Diurno"),
         dia = paste(ymd(Date_M),Time_M),
         date_timeUTC = ymd_hms(dia, tz = "UTC")) %>% 
  dplyr::select(-time2)

#convertir a tiempo
datostiempo$Date_M <- ymd(datostiempo$Date_M)

#Crear capa para limpiar los datos que esten fuera de los transectos y Crear Clave primaria 
capaNASC <- st_as_sf(datostiempo, coords = c("Longitud", "Latitud"), crs = 4326) %>% 
  mutate(ID_PK = paste0(cruise,"_",ID))

#Cargar la capa de Time Zones
time_zonesOcean <- st_read("D:/2021/capas_generales/time_zones.gpkg",layer = "ne_10m_time_zones") %>% 
  st_crop(capaNASC) %>% select(tz_name1st)

##### ESTANDARIZAR HORA Y FECHA: Definicion de las zonas horarias #####
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
  dplyr::mutate(date = format(local_date, "%Y-%m-%d"), time = format(local_date, "%H:%M:%S"), fid = ID, species = species, template = template) %>% 
  dplyr::select(-c(tz_name1st,dia, date_timeUTC,local_date)) %>% dplyr::select(fid, ID:Time_M, date,time, Lat_M:ID_PK, species, template, transects)

#convertir a minusculas las columnas
colnames(capaNASC_tz) <- tolower(names(capaNASC_tz))
#Convertir a character el tiempo
capaNASC_tz$time_m <- as.character(capaNASC_tz$time_m)

#Exportar CAPA ESTANDAR 
capaNASC_tz_estandar <- capaNASC_tz %>% dplyr::select(fid, process_id:dist_m, date,time, noise_sv_1m:cruise, species, template, geometry)

##### EXPORTACION DE DATOS: Crear nueva carpeta para exportar #####
carpeta_capas <- 'nasc/capas'

if (file.exists(carpeta_capas)) {
  "La carpeta ya existe"
} else {
  dir.create('nasc/capas', recursive = TRUE)
}

#Exportar como geopackage para limpiar los datos y exportar capa estandar
st_write(capaNASC_tz, paste0("nasc/capas/", nombreNASCgeneral, tolower(template),".gpkg"))
st_write(capaNASC_tz_estandar, paste0("nasc/capas/",nombreNASCgeneral, tolower(template),"_estandar.gpkg"))

##### HASTA AQUI Y SE LIMPIAN LOS DATOS EN UN SIG GRAFICO Y SE REALIZA LA ECOINTEGRACION ####

datos_sf <- st_read(paste0("nasc/capas/",nombreNASCgeneral, tolower(template),".gpkg"))

##### ECOINTEGRACION. Sumar el NASC general #####
#Concatenar las coordenadas
datos_sf$coor_concatenadas <- paste0(datos_sf$lon_m,"-",datos_sf$lat_m)
#Extraer los datos para concatenar con la tabla de la suma
datos_concatenar <- datos_sf %>% dplyr::select(id,date_m,time_m, date, time, lat_m, lon_m, tiempo, cruise,geom, coor_concatenadas)

#Realizar la suma y promedio de los datos
nasc_sum_meancon <- datos_sf %>% group_by(coor_concatenadas) %>% 
  summarize(sum_nasc = round(sum(nasc, na.rm = TRUE),4), mean_nasc = round(mean(nasc, na.rm = TRUE),4)) %>% st_drop_geometry() %>% 
  inner_join(datos_concatenar, by = "coor_concatenadas") %>% arrange(id)


#Eliminar las filas duplicadas y Crear un id al dataframe 
nasc_sum_meanconfin <- nasc_sum_meancon[!duplicated(nasc_sum_meancon$coor_concatenadas),] %>% 
  tibble::rowid_to_column("ID") %>% dplyr::select(-(coor_concatenadas)) %>% 
  mutate(id_pk = paste0(cruise,"_",ID), species = species, template = template) %>% 
  dplyr::rename(fid = ID) %>% dplyr::select(date,time, cruise, sum_nasc, mean_nasc,species, template, geom)

#Exportar como shapefile o geopackage
st_write(nasc_sum_meanconfin, paste0("nasc/capas/",nombreNASCecointegracion,tolower(template),"_estandar.gpkg"))


##### FUNCION PARA ECOINTEGRACION POR CAPAS #####
capas_profundidad <- function(data_nasc, depth_min, depth_max) {
  capa_profundidad <- dplyr::filter(data_nasc, depth_mean >= depth_min & depth_mean < depth_max)
  capa_nasc_suma <- aggregate(nasc ~coor_concatenadas, FUN = sum, data=capa_profundidad) %>% rename(sum_nasc = nasc)
  columnas_capas <- dplyr::select(capa_profundidad, date,time, lat_m, lon_m, cruise, geom, coor_concatenadas)
  capa_nasc_sumacol <- inner_join(columnas_capas, capa_nasc_suma, by = "coor_concatenadas")
  capa_nasc_sumacolfin <- capa_nasc_sumacol[!duplicated(capa_nasc_sumacol$coor_concatenadas),] %>% 
    tibble::rowid_to_column("ID") %>% dplyr::select(-c(coor_concatenadas,ID)) %>% 
    dplyr::mutate(layer_z = paste0(depth_min, "-" ,depth_max, " m"))
}


##### ECOINTEGRACION CAPAS #####
capa05_15 <- capas_profundidad(datos_sf, depth_min = 0, depth_max = 15) 

capa15_20 <- capas_profundidad(datos_sf, depth_min = 15, depth_max = 20) 

capa20_30 <- capas_profundidad(datos_sf, depth_min = 20, depth_max = 30) 

capa30_40 <- capas_profundidad(datos_sf, depth_min = 30, depth_max = 40)

capa40_50 <- capas_profundidad(datos_sf, depth_min = 40, depth_max = 50)

capa50_75 <- capas_profundidad(datos_sf, depth_min = 50, depth_max = 75) 

capa75_100 <- capas_profundidad(datos_sf, depth_min = 75, depth_max = 100)

capa100_ <- capas_profundidad(datos_sf, depth_min = 100, depth_max = 500)

capas <- rbind(capa05_15, capa15_20, capa20_30, capa30_40, capa40_50, capa50_75, capa75_100, capa100_) %>% 
  st_write(paste0("nasc/capas/",crucero,"nasc_",tolower(template), "_capas_profundidad.gpkg"))
