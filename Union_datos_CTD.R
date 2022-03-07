##### Leer los archivos separados #####
#ctd <- read.table("oceanografia/JCFINP2110_ CTD FINAL/JCFINP2110004_E2.TXT", skip = 34)
#archivo <- readLines(file("oceanografia/JCFINP2110_ CTD FINAL/JCFINP2110004_E2.TXT"))
library(tidyverse)
##### Atomatizar la lectura de los archivos #####
ctd_txt_data <- list.files(path = "oceanografia/ctd/txt", pattern = ".txt", full.names = TRUE, recursive = TRUE)
ctd_txt_data <- ctd_txt_data[2:length(ctd_txt_data)]
read_ctd <- function(x){read.table(x, skip = 34)}

extract_asterisk <- function(x){
  stringr::str_replace_all(x, "\\*", " ") %>% 
    as.numeric()}

read_archivo_station <- function(y){a = readLines(file(y)); as.numeric(a <- substr(a[3],15,20))}
read_archivo_cruise <- function(y){a = readLines(file(y)); substr(a[6], 15,24)}
read_archivo_year <- function(y){a = readLines(file(y)); as.numeric(substr(a[28], 22,25))}
read_archivo_month <- function(y){a = readLines(file(y)); substr(a[28], 19,20)}
read_archivo_day <- function(y){a = readLines(file(y)); substr(a[28], 16,17)}
read_archivo_hour <- function(y){a = readLines(file(y)); substr(a[28], 27,28)}
read_archivo_minute <- function(y){a = readLines(file(y)); substr(a[28], 30,31)}
read_archivo_second <- function(y){a = readLines(file(y)); substr(a[28], 33,34)}

metadato_ctd_station <- purrr::map(ctd_txt_data, .f = read_archivo_station)
metadato_ctd_cruise <- purrr::map(ctd_txt_data, .f = read_archivo_cruise)
metadato_ctd_year <- purrr::map(ctd_txt_data, .f = read_archivo_year)
metadato_ctd_month <- purrr::map(ctd_txt_data, .f = read_archivo_month)
metadato_ctd_day <- purrr::map(ctd_txt_data, .f = read_archivo_day)
metadato_ctd_hour <- purrr::map(ctd_txt_data, .f = read_archivo_hour)
metadato_ctd_minute <- purrr::map(ctd_txt_data, .f = read_archivo_minute)
metadato_ctd_second <- purrr::map(ctd_txt_data, .f = read_archivo_second)

data_ctd <- purrr::map(ctd_txt_data, .f = read_ctd) %>%
  purrr::map2(.y = metadato_ctd_cruise, ~mutate(.x, Cruise = .y)) %>%
  purrr::map2(.y = metadato_ctd_station, ~mutate(.x, Station = .y)) %>% 
  purrr::map2(.y = metadato_ctd_year, ~mutate(.x, Year = .y)) %>%
  purrr::map2(.y = metadato_ctd_month, ~mutate(.x, Month = .y)) %>%
  purrr::map2(.y = metadato_ctd_day, ~mutate(.x, Day = .y)) %>%
  purrr::map2(.y = metadato_ctd_hour, ~mutate(.x, Hour = .y)) %>%
  purrr::map2(.y = metadato_ctd_minute, ~mutate(.x, Minute = .y)) %>%
  purrr::map2(.y = metadato_ctd_second, ~mutate(.x, Second = .y)) %>%
  purrr::reduce(rbind)

name_variables_ctd <- c("Latitude", "Longitude", "Pressure", "Depth","Temperature","Conductivity",
                        "Salinity","SigmaT","Optical_ppm","Optical","Sound_Velocity ","ECO-SCATT","Chla","pH",
                        "PAR", "PAR-Ref","Eh","TDS","Transmissometer", "Cruise","Station","Year",
                        "Month","Day","Hour","Minute","Second")

colnames(data_ctd) <- name_variables_ctd

data_ctd <- data_ctd %>% 
  dplyr::rename(Oxygen = Optical_ppm, Oxigen_percent = Optical, ECO = `ECO-SCATT`, `ECO-Chla` = Chla) %>% 
  dplyr::select(Cruise:Second, Latitude:SigmaT, Oxygen:`ECO-Chla`, pH:Transmissometer)

# ELIMINAR ASTERISTICOS Y DATOS CON POSICION GEOGRAFICA -------------------

datos_sin_asteriscos <- data_ctd %>% 
  dplyr::select(9:ncol(data_ctd)) %>% 
  purrr::map_dfr(.f = extract_asterisk)

datos_fecha <- data_ctd %>% 
  dplyr::select(1:8)

data_ctd <- bind_cols(datos_fecha, datos_sin_asteriscos) %>% 
  dplyr::filter(Latitude >0, pH > 1)


# OBTENER DATOS DE BAJADA DEL CTD -----------------------------------------

ctd_bajada <- function(data_ctd_completo, station){
  eo <- data_ctd_completo %>% 
    dplyr::filter(Station == station)
  num_nrow <- nrow(eo)
  eo <- eo %>% 
    mutate(fid = 1:num_nrow)
  eo_max <- max(eo$Pressure)
  corte <- eo %>% 
    filter(Pressure == eo_max)
  corte_b <- corte$fid[1]
  eo_bajada <- eo %>% 
    filter(fid <= corte_b) %>% 
    dplyr::select(-fid)
}

estaciones_bajada <- unique(data_ctd$Station)

df_eo <- list()
for (i in estaciones_bajada) {
  df <- ctd_bajada(data_ctd, i)
  df_eo <- rbind(df_eo, df)
}

# GUARDAR ARCHIVO LIMPIO --------------------------------------------------

cruise <- df_eo$Cruise[1]
plot(df_eo$Longitude, df_eo$Latitude)
write.csv(df_eo, paste0("oceanografia/",cruise,"_CTD_completo.csv"), row.names = FALSE)
