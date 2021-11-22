##### ESTANDARIZAR DATOS DE LA TERMOSAL Y CLOROFILA #####
pacman::p_load(tidyverse, lubridate, sf, readxl)

##### PREPARACION DE LA ENTRADA DE LOS DATOS #####
setwd("D:/2021/JCFINP2110/oceanografia") #Leer el espacio de trabajo, una carpeta antes donde estan los archivos
#ter1 <- read.table("JCFINP-20-09-TER-00.txt", sep = "/t", header = T)
sf_termo <- st_read("D:/2021/JCFINP2110/oceanografia/jcfinp2110_ter00_26.gpkg") %>% 
  dplyr::mutate(time_min = substr(time, 1,5), date_time = paste(ymd(date),time_min))
#Nombre de salida del archivo
nombre_archivo <- "jcfinp2110_ter_chla"
### Con programacion funcional
##leer todos los txt, path indica el nombre de la carpeta que contiene los txt
archivos <- list.files(path = "JCFINP2110 _TERMOSAL_TER 0-23", pattern = ".xls", full.names = T, recursive = T)
#En caso de que existan otros txt detectar algun patron, para eliminar
archivos <- archivos[str_detect(archivos,c("TER"))]
#read.table(archivos[1], sep = "/t", header = T)
read_txtf <- function(x) {read_xls(x, col_names = F)}


##### Leer todos los archivos txt y unir #####
datachla <- purrr::map(.x = archivos, .f = read_txtf) %>% 
  reduce(rbind) %>% 
  as.data.frame()

names(datachla) <- c("id","scant_count","date","time","chla","chla_uni","temp","temp_uni")

datachla$Time <- format(datachla$time,"%H:%M")

datachla1 <- datachla %>% 
  dplyr::mutate(date_time = paste(ymd(date),Time)) %>% 
  dplyr::select(chla, date_time) %>% 
  dplyr::group_by(date_time) %>% 
  dplyr::summarise(chla_mean = round(mean(chla, na.rm = TRUE),4))

##### Realizar union de los datos ##### 
termo_chla <- sf_termo %>% 
  dplyr::left_join(datachla1) %>% 
  dplyr::filter(latitude >0)


#termo_chla_unique <- termo_chla[!duplicated(termo_chla$geom),]
ggplot()+geom_sf(data = termo_chla)

termo_chla_unique <- termo_chla %>% 
  dplyr::select(-c(time_min,date_time))

st_write(termo_chla_unique, paste0(nombre_archivo,".gpkg"))
