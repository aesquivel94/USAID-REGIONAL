rm(list = ls()); gc(reset = TRUE)
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Made by:     Alejandra Esquivel Arias. 
# Created in:  Date: 8-2019.
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Temperatures exploratory analysis. 

# =-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-
# Packages. 
# =-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-
library(tidyverse)
library(lubridate)
library(jsonlite)
library(qmap)
library(sf)
# =-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=- 

# 
shp <- HM_shp <-  raster::getData('GADM', country='HN', level=1) %>% sf::st_as_sf(.) 
dry_C <- sf::read_sf('D:/OneDrive - CGIAR/Desktop/USAID-Regional/USAID-REGIONAL/Honduras_Corredor_Seco.shp') 
#


# Reading max temperature. 
tmax_daily_qc <- read_csv("MSD_Index/to_proof_in_CPT/hnd_copeco/hnd_copeco/daily_processed/tmax_daily_qc.csv")
# tmax_daily_qc %>% filter(year > 1981) %>% dplyr::select(year) %>% count(year) 
tmax_daily_qc <- tmax_daily_qc %>% 
  filter(year > 1981) 

# Computing NA percent
na_percent_max <- tmax_daily_qc %>% 
  select_if(function(x) any(is.na(x)))  %>% 
  summarise_all(funs(sum(is.na(.))/n() * 100)) %>% 
  gather(station, value) %>% 
  filter(value <= 25) %>% 
  mutate(station = str_remove(station, 'X'))


# Reading min temperature. 
tmin_daily_qc <- read_csv("MSD_Index/to_proof_in_CPT/hnd_copeco/hnd_copeco/daily_processed/tmin_daily_qc.csv")
# tmin_daily_qc %>% filter(year > 1981) %>% dplyr::select(year) %>% count(year) 
tmin_daily_qc <- tmin_daily_qc %>% filter(year > 1981)

# Computing NA percent
na_percent_min <- tmin_daily_qc %>% 
  select_if(function(x) any(is.na(x)))  %>% 
  summarise_all(funs(sum(is.na(.))/n() * 100)) %>% 
  gather(station, value) %>% 
  filter(value <= 25)

# NA precent
na_total <- inner_join(na_percent_max, na_percent_min, by = 'station') %>% 
  set_names(c('station', 'max_NA', 'min_NA'))

print(na_total)
rm(na_percent_max, na_percent_min)


# Organizando los datos
na_total$station

tmax <- tmax_daily_qc %>% 
  select_all(~str_remove(., "X")) %>% 
  dplyr::select(day, month,  year, na_total$station) %>% 
  gather(station, tmax, -day, -month, -year)

tmin <- tmin_daily_qc %>% 
  dplyr::select(day, month,  year, na_total$station)  %>% 
  gather(station, tmin, -day, -month, -year)

temperatures <- inner_join(tmax, tmin)
rm(tmin, tmax)

# na_percent 
temperatures %>% group_by(station) %>% 
  summarise(tmax_na = sum(is.na(tmax)/n() * 100), tmin_na = sum(is.na(tmin)/n() * 100))

# Existen temperaturas min == max. 
temperatures %>% mutate(dif = tmax - tmin) %>% filter(dif <= 0)

temperatures <-temperatures %>% 
  mutate(dif = tmax - tmin, dif_cat = ifelse(dif == 0, 1, 0)) %>% 
  dplyr::select(-dif) %>% 
  mutate(tmax = ifelse(dif_cat == 1, NA_real_, tmax),
         tmin = ifelse(dif_cat == 1, NA_real_, tmin)) %>%  
  # nest(-dif_cat) %>% filter(dif_cat == 1) %>% unnest()
  dplyr::select(-dif_cat)


# na_percent 
temperatures %>% group_by(station) %>% 
  summarise(tmax_na = sum(is.na(tmax)/n() * 100), tmin_na = sum(is.na(tmin)/n() * 100))


# Catalogo de datos...
catalog <- read_csv('D:/OneDrive - CGIAR/Desktop/USAID-Regional/USAID-REGIONAL/MSD_Index/to_proof_in_CPT/hnd_copeco/hnd_copeco/stations_catalog_copeco.csv')
catalog %>% names()

# =-=-=
names_var <- do.call(rbind,str_split(na_total$station, '_')) %>% 
  as.tibble() %>% set_names('cod', 'names')

# 
catalog <- catalog %>%
  filter(national_code %in% names_var$cod) %>% 
  mutate(id = 1:n()) %>% 
  # dplyr::select(Lat, Lon, nombre, national_code, Lat_1, Lon_1, ALTITUD)
  dplyr::select(Lat, Lon, nombre, national_code, ALTITUD)


# Join temperaturas con el catalog.
temperatures <- temperatures %>% 
  nest(-station) %>% 
  mutate(station1 = station) %>% 
  separate(station1, c('national_code', 'nombre'), '_') %>% 
  inner_join(catalog, .) %>% 
  dplyr::select(-nombre, -national_code)


# Pruebas de NASA-power. 
Nasa_download <- function(lat, lon){
  # lat <- 13.3
  # lon <- -87.6
  json_file <- paste0("https://power.larc.nasa.gov/cgi-bin/v1/DataAccess.py?&request=execute&identifier=SinglePoint&parameters=ALLSKY_SFC_SW_DWN,RH2M,T2M_MAX,T2M_MIN,WS2M&startDate=19830101&endDate=",format(Sys.Date(),"%Y%m%d"),"&userCommunity=AG&tempAverage=DAILY&outputList=ASCII&lat=",lat,"&lon=",lon)
  json_data <- jsonlite::fromJSON(json_file)
  
  
  data_nasa <-  tibble(dates = seq(as.Date("1983/1/1"), as.Date(format(Sys.Date(),"%Y/%m/%d")), "days")) %>%  
    mutate(year = year(dates), month = month(dates), day = day(dates),
           tmin_N = json_data$features$properties$parameter$T2M_MIN %>% unlist, 
           tmax_N = json_data$features$properties$parameter$T2M_MAX %>% unlist, 
           srad_N = json_data$features$properties$parameter$ALLSKY_SFC_SW_DWN %>% unlist, 
           rhum_N = json_data$features$properties$parameter$RH2M %>% unlist, 
           WS_N = json_data$features$properties$parameter$WS2M %>% unlist) %>% 
    na_if(-99)
return(data_nasa)}


tictoc::tic()
test <- temperatures %>% 
  mutate(data_nasa = purrr::map2(.x = Lat, .y = Lon, .f = Nasa_download)) %>% 
  mutate(join = purrr::map2(.x = data, .y = data_nasa, .f = function(x, y){
    inner_join(x, y) })) #  %>% gather(type, value, -dates, -day, -month, -year)
tictoc::toc() # 3.63 min. 


# Graph ubicacion de las estaciones...
test <- test %>% rename( 'x' = 'Lat', 'y' = 'Lon')
ggplot(test) + 
  geom_point(aes(y, x)) +
  theme_bw() + 
  geom_sf(data = shp, fill = NA, color = gray(.5)) +
  geom_sf(data = dry_C, fill = NA, color = gray(.1)) +
  labs(x = 'Longitud', y = 'Latitud', colour = NULL)


# Comparacion de las temperaturas... Nasa~Original. 
data_complete <- test %>% 
  dplyr::select(-data, -data_nasa) %>%
  unnest() %>% 
  dplyr::select(dates, everything())


a <- data_complete %>%
  dplyr::select(dates, station, tmax, tmax_N) %>% 
  mutate(Temp = 'Tmax') %>% 
  rename('Original' = 'tmax', 'Nasa' = 'tmax_N') %>% 
  gather(type, value, -dates, -station, -Temp)
  

a <- data_complete %>%
  dplyr::select(dates, station, tmin, tmin_N) %>% 
  mutate(Temp = 'Tmin') %>% 
  rename('Original' = 'tmin', 'Nasa' = 'tmin_N') %>% 
  gather(type, value, -dates, -station, -Temp) %>% 
  bind_rows(a, .)


ggplot(a, aes(x = dates, y = value, colour = type)) + 
  geom_line() + 
  facet_grid(Temp~station) + 
  theme_bw() + 
  labs(x = NULL, y = 'Temperature', colour = NULL)


# Los otros graphs. 
b <- test %>% 
  dplyr::select(-data, -join) %>% 
  unnest  %>% 
  na_if(-999) %>% 
  dplyr::select(station, dates, srad_N, rhum_N, WS_N) 

# srad_N
ggplot(b, aes(x = dates, y = srad_N, colour = station)) +
  geom_line() +
  scale_colour_viridis_d(direction = 1) +
  theme_bw() + 
  theme(legend.position = 'top') +
  labs(x = NULL, y = 'Solar radiation')

# rhum_N
ggplot(b, aes(x = dates, y = rhum_N, colour = station)) +
  geom_line() +
  scale_colour_viridis_d(direction = 1) +
  theme_bw() + 
  theme(legend.position = 'top') +
  labs(x = NULL, y = 'rhum_N')

# WS_N
ggplot(b, aes(x = dates, y = WS_N, colour = station)) +
  geom_line() +
  scale_colour_viridis_d(direction = 1) +
  theme_bw() + 
  theme(legend.position = 'top') +
  labs(x = NULL, y = 'WS_N')


# Tener presente que los NA en NASA son -999, hay que hacer un cambio para que funcione correctamente.







# =-=-=-=-=-=-=-=-=


library(qmap)

fit_Qmap <- function(data){
  # data <- f$data[1][[1]]; var <- 'tmax'
  # model <- qmap::fitQmapRQUANT(pull(data, !!var), pull(data, !!glue::glue('{var}_N')), qstep = 0.025, nboot = 100)
  # forecast <- qmap::doQmapRQUANT(pull(data, !!glue::glue('{var}_N')), model, type="linear")
  
  model_tmax <- qmap::fitQmapRQUANT(pull(data, tmax), pull(data, tmax_N), qstep = 0.025, nboot = 100)
  model_tmin <- qmap::fitQmapRQUANT(pull(data, tmin), pull(data, tmin_N), qstep = 0.025, nboot = 100)
  for_tmax_l <- qmap::doQmapRQUANT(pull(data, tmax_N), model_tmax, type="linear")
  for_tmin_l <- qmap::doQmapRQUANT(pull(data, tmin_N), model_tmin, type="linear")
  
  model_data <- data %>% 
    dplyr::select(day, month, year, dates) %>% 
    mutate(tmax_model = for_tmax_l, tmin_model = for_tmin_l) 
  
  return(model_data)}

tictoc::tic()
f <- test %>% 
  dplyr::select(-data, -data_nasa ) %>%
  unnest() %>% 
  dplyr::na_if(-999) %>% 
  nest(-x, -y, -ALTITUD, -station) %>% 
  mutate(model_data = purrr::map(.x = data, .f = fit_Qmap ))
tictoc::toc() #  2.277 min 



# Graph...
a <- f %>%
  unnest() %>% 
  dplyr::select(dates, station, tmax, tmax_N, tmax_model) %>% 
  mutate(Temp = 'Tmax') %>% 
  rename('Original' = 'tmax', 'Nasa' = 'tmax_N', 'Model_l' = 'tmax_model') %>% 
  gather(type, value, -dates, -station, -Temp)


a <- f %>%
  unnest()  %>%
  dplyr::select(dates, station, tmin, tmin_N, tmin_model) %>%
  mutate(Temp = 'Tmin') %>%
  rename('Original' = 'tmin', 'Nasa' = 'tmin_N', 'Model_l' = 'tmin_model') %>%
  gather(type, value, -dates, -station, -Temp) %>%
  bind_rows(a, .)
  


path <- 'D:/OneDrive - CGIAR/Desktop/USAID-Regional/USAID-REGIONAL/MSD_Index/to_proof_in_CPT/hnd_copeco/models_temp/'

a %>% dplyr::select(station) %>% unique()  


graph_temp <- function(x, data){
  
  ggplot(a %>% filter(station == as.character(x)), aes(x = dates, y = value, colour = type)) + 
    geom_line() + 
    scale_x_date(date_breaks = "7 year", date_labels = "%Y") + 
    facet_grid(station~Temp) + 
    theme_bw() + 
    labs(x = NULL, y = 'Temperature', colour = NULL) +
    theme(legend.position = 'bottom')
  
  
  ggsave(glue::glue('{path}{x}.png'), width = 10, height = 5)
}

a %>% 
  nest(-station) %>% 
  mutate(ajam = purrr::map2(.x = station, .y = data, .f = graph_temp))





data <- test %>% nest(-x, -y, -ALTITUD, -station) %>% filter(row_number() == 1) %>% .$data %>% .[[1]]


coef <- function(data){

  
  data_to_model <- data %>% 
    dplyr::select(join) %>% 
    unnest()
  
  data_nasa <- data %>% 
    dplyr::select(data_nasa) %>% 
    unnest()
  
  
  
  data_obs <- data %>% 
    dplyr::select(data) %>% 
    unnest()
  
  
  
  data_obs
  
  
  
  
  
  
  
  
  model <- qmap::fitQmapRQUANT(pull(data_to_model, 'tmax'), pull(data_to_model, 'tmax_N'), qstep = 0.025, nboot = 100)
  forecast <- qmap::doQmapRQUANT(pull(data_nasa,'tmax_N') %>% na.omit(), model, type="linear")
  
  
  
  
  
  
  
  
  
  
  
  
  # model_tmax <- qmap::fitQmapRQUANT(pull(data, tmax), pull(data, tmax_N), qstep = 0.025, nboot = 100)
  # model_tmin <- qmap::fitQmapRQUANT(pull(data, tmin), pull(data, tmin_N), qstep = 0.025, nboot = 100)
}







