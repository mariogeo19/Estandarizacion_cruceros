##### SCRIPT PARA ESTANDARIZAR DATOS DE LA TERMOSAL  #####
pacman::p_load(tidyverse, lubridate, sf)
setwd("D:/2021/JCFINP2110/oceanografia") #Leer el espacio de trabajo, una carpeta antes donde estan los archivos
#ter1 <- read.table("JCFINP-20-09-TER-00.txt", sep = "\t", header = T)

##### ENTRADA DE DATOS #####
##leer todos los txt, path indica el nombre de la carpeta que contiene los txt
archivos <- list.files(path = "JCFINP2110 _TERMOSAL_TER 0-23", pattern = ".txt", full.names = T)
#En caso de que existan otros txt detectar algun patron, para eliminar
archivos <- archivos[str_detect(archivos,c("-TER-"))]
#read.table(archivos[1], sep = "\t", header = T)
read_txtf <- function(x) {read.table(file = x, header = T, sep ="\t")}

#Obtener los nombres de los archivos, quitar el path y lo que sea necesario
nombres_arc <- archivos %>% str_remove_all("JCFINP2110 _TERMOSAL_TER 0-23/JCFINP21-10-") %>% str_remove_all(".txt")
nombres_crucero <- archivos %>% str_remove_all("JCFINP2110 _TERMOSAL_TER 0-23/") %>% str_remove_all(".txt") %>% 
  str_remove_all("TER") %>% 
  str_remove_all("-") %>% substr(1,10)

#Generar el origen del año para transformar a fecha estandar
#origin_year <- ymd_hms("2020-01-01 00:00:00", tz = "America/Hermosillo")
origin_year <- "2021-01-01 00:00:00"

##### LEER TODOS LOS TXT Y UNIR #####
termosal <- purrr::map(.x = archivos, .f = read_txtf) %>% 
  map(~rename(.x, scan_count=Scan.Count, temperature = Temperature..ITS.90..deg.C., conductivity = Conductivity..S.m., 
             density = Density..sigma.theta..kg.m.3., salinity = Salinity..Practical..PSU., 
             julian_days = Julian.Days, hours = Time..Elapsed..hours., minutes = Time..Elapsed..minutes., 
             seconds = Time..Elapsed..seconds., latitude = Latitude..deg., longitude = Longitude..deg.)) %>% 
  map2(.y = nombres_arc, ~mutate(.x, archive = .y)) %>% 
  map2(.y = nombres_crucero, ~mutate(.x, cruise = .y)) %>% 
  map(~select(.x, scan_count, cruise, archive, temperature:longitude)) %>% 
  reduce(rbind)

termosalsf <- termosal %>%dplyr::mutate(lat = latitude, lon = longitude) %>% st_as_sf(coords=c("lon","lat"), crs=4326)
##### ESTANDARIZAR FECHA Y HORA. Cargar la capa de Time Zones #####
time_zonesOcean <- st_read("D:/2021/capas_generales/time_zones.gpkg",layer = "ne_10m_time_zones") %>% 
  st_crop(termosalsf) %>% select(tz_name1st)
#Intersectar para obtener las zonas horarias
Los_Angeles <- "America/Los_Angeles"; tz_Los_Angeles <- ymd_hms(origin_year, tz = Los_Angeles)
New_York <- "America/New_York"; tz_New_York <- ymd_hms(origin_year, tz = New_York)
Denver <- "America/Denver"; tz_Denver <- ymd_hms(origin_year, tz = Denver)
Chicago <- "America/Chicago"; tz_Chicago <- ymd_hms(origin_year, tz =Chicago)

#Obtener las fechas de acuerdo a las zonas horarias
termosalsf_tm <- termosalsf %>% st_intersection(time_zonesOcean) %>% 
  dplyr::mutate(tz_date = case_when(tz_name1st == Denver ~ tz_Denver + julian_days * 3600 * 24,
                                    tz_name1st == Chicago ~ tz_Chicago + julian_days * 3600 * 24,
                                    tz_name1st == Los_Angeles ~ tz_Los_Angeles + julian_days * 3600 * 24,
                                    tz_name1st == New_York ~ tz_New_York + julian_days * 3600 * 24)) %>% 
  dplyr::mutate(date = format(tz_date, "%Y-%m-%d"), time = format(tz_date, "%H:%M:%S")) %>%
  dplyr::select(scan_count:archive, date, time, temperature:salinity, latitude, longitude) %>% 
  as.data.frame()

#termosal <- termosal %>% mutate(fecha = origin_year + Julian_Days * 3600 * 24) %>% 
#mutate(date = format(fecha, "%Y-%m-%d"), time = format(fecha, "%H:%M:%S")) %>% 
#  select(Scan_Count:archive, date, time, Temperature:Longitude)
##### REVISAR DATOS Y EXPORTAR #####
plot(termosalsf_tm$longitude, termosalsf_tm$latitude)

##### Guardar el archivo gpkg #####
st_write(termosalsf_tm, tolower(paste0(nombres_crucero[1],"_TER00_",length(nombres_arc)-1, ".gpkg")))




