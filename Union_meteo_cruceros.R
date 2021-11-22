pacman::p_load(tidyverse, lubridate, sf)
setwd("D:/2021/JCFINP2110/oceanografia/JCFINP2110 METEREOLOGICA")

#Definir el nombre del crucero
cruise <- "JCFINP2110"

#Leer los archivos de la meteo
archivos <- list.files(recursive = T, pattern = ".log")
archivos_log <- archivos[str_detect(archivos, "data_3397_2021")] %>% str_remove_all( "zeno_raw_data_3397_2021")
archivos_log2 <- archivos_log[str_detect(archivos_log, "data_3397_2021")] 

#Funcion para leer los archivos 
readcsv <- function(x){read.csv(file = x, sep = ",")}
#meteo1 <- read.csv(archivos_log2[1], sep = ",")
#meteo29 <- read.csv(archivos_log2[7], sep = ",")

#Obtener los nombres de los archivos, quitar el path y lo que sea necesario
nombres_arc <- archivos_log2 %>% str_remove_all(".log") %>% substr(28,30)

#Leer todos los archivos y unir
meteorologica <- purrr::map(.x = archivos_log2, .f = readcsv) %>% 
  map(~dplyr::select(.x, -c(Null,zeno_alarms,zeno_warnings,
                            zeno_status,UTC,DewPoint,Zeno_Date,
                            Zeno_Time, 
                            zeno_date.zeno_time.zeno_timezone, 
                            ID, Sat.Hit))) %>% 
  map2(.y = nombres_arc, ~mutate(.x, archive = .y)) %>% 
  reduce(rbind)
#Descartar los datos sin posicion geografica
# Extraer las coordenadas mayores a -9959.997
meteorologica2 <- meteorologica %>% dplyr::filter(Latitude >0) %>% dplyr::filter(Longitude < 0) 
#Extraer la fecha
meteorologica_date <- ymd_hms(meteorologica2$date.time.timezone)

#Convertir a numerico y extraer las coordenadas y convertirlas a decimal
meteorologica3 <- meteorologica2 %>% 
  dplyr::select(Barometric.Pressure:archive) %>% 
  mutate_if(is.character,as.numeric) %>% 
  dplyr::mutate(lat_gra = as.numeric(substr(Latitude, 1,2)), lat_min = as.numeric(substr(Latitude, 3,10)), 
                lon_gra = case_when(Longitude <= -10000 ~ abs(as.numeric(substr(Longitude, 1,4))),
                                    Longitude > -10000 ~ abs(as.numeric(substr(Longitude, 1,3)))), 
                lon_min = case_when(Longitude <= -10000 ~ as.numeric(substr(Longitude, 5,10)),
                                    Longitude > -10000 ~ as.numeric(substr(Longitude, 4,10)))) %>% 
  dplyr::mutate(lat = lat_gra+(lat_min/60), lon =(lon_gra+(lon_min/60))*-1) %>% 
  dplyr::mutate(Latitude = lat, Longitude = lon) %>% 
  cbind(meteorologica_date)

#Convertir a espacial
meteorologicasf <- meteorologica3 %>% st_as_sf(coords=c("lon","lat"), crs=4326)
#Cargar la capa de Time Zones
time_zonesOcean <- st_read("D:/2021/capas_generales/time_zones.gpkg",layer = "ne_10m_time_zones") %>% 
  st_crop(meteorologicasf) %>% select(tz_name1st)
#Intersectar para obtener las zonas horarias
origin_year <- "2021-01-01 00:00:00"
Los_Angeles <- "America/Los_Angeles"
New_York <- "America/New_York"
Denver <- "America/Denver"
Chicago <- "America/Chicago"

#Obtener las fechas de acuerdo a las zonas horarias
meteorologicasf_tm <- meteorologicasf %>% st_intersection(time_zonesOcean) %>% 
  dplyr::mutate(local_date = case_when(tz_name1st == Denver ~ lubridate::with_tz(ymd_hms(meteorologica_date, tz = "UTC"), tz =  Denver),
                                       tz_name1st == Chicago ~ lubridate::with_tz(ymd_hms(meteorologica_date, tz = "UTC"), tz = Chicago),
                                       tz_name1st == Los_Angeles ~ lubridate::with_tz(ymd_hms(meteorologica_date, tz = "UTC"), tz = Los_Angeles),
                                       tz_name1st == New_York ~ lubridate::with_tz(ymd_hms(meteorologica_date, tz = "UTC"), tz = New_York)),
                cruise = cruise) %>% 
  dplyr::mutate(date = format(local_date, "%Y-%m-%d"), time = format(local_date, "%H:%M:%S")) %>% 
  dplyr::select( cruise, archive, date, time, Latitude, Longitude, Barometric.Pressure:Cks, PH:Wind.Speed.T, -c(lat_gra:tz_name1st, local_date)) %>% 
  dplyr::rename(barometric_pressure = Barometric.Pressure, relative_humidity = Relative.Humidity, water_temp = Water.Temp, 
                wind_direction10min = Wind.Direction.10Min, wind_direction2Min = Wind.Direction.2Min, 
                wind_direction_apparent = Wind.Direction.Apparent, wind_direction_T = Wind.Direction.T,
                wind_speed10min = Wind.Speed.10Min, wind_speed2min = Wind.Speed.2Min, wind_speed_apparent = Wind.Speed.Apparent,
                wind_speed_t = Wind.Speed.T)

#Convertir columnas a minusculas
colnames(meteorologicasf_tm) <- tolower(colnames(meteorologicasf_tm))
ggplot(data = meteorologicasf_tm)+geom_sf()
#Guardar la capa estandarizada
st_write(meteorologicasf_tm, paste0(cruise,"meteorologica00_",length(nombres_arc),".gpkg"), row.names = F)
