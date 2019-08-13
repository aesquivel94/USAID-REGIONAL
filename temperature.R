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
# =-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=- 


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
temperatures %>% 
  group_by(station) %>% 
  summarise(tmax_na = sum(is.na(tmax)/n() * 100), tmin_na = sum(is.na(tmin)/n() * 100))


# Existen temperaturas min == max. 
temperatures %>% 
  mutate(dif = tmax - tmin) %>% 
  filter(dif <= 0)


temperatures <-temperatures %>% 
  mutate(dif = tmax - tmin, dif_cat = ifelse(dif == 0, 1, 0)) %>% 
  dplyr::select(-dif) %>% 
  mutate(tmax = ifelse(dif_cat == 1, NA_real_, tmax),
         tmin = ifelse(dif_cat == 1, NA_real_, tmin)) %>%  
  # nest(-dif_cat) %>% filter(dif_cat == 1) %>% unnest()
  dplyr::select(-dif_cat)




# na_percent 
temperatures %>% 
  group_by(station) %>% 
  summarise(tmax_na = sum(is.na(tmax)/n() * 100), tmin_na = sum(is.na(tmin)/n() * 100))




# Catalogo de datos...

catalog <- read_csv('D:/OneDrive - CGIAR/Desktop/USAID-Regional/USAID-REGIONAL/MSD_Index/to_proof_in_CPT/hnd_copeco/hnd_copeco/stations_catalog_copeco.csv')


catalog %>% names()


catalog$nombre
catalog$national_code


names_var <- do.call(rbind,str_split(na_total$station, '_')) %>% 
  as.tibble() %>% 
  set_names('cod', 'names')


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
  json_file <- paste0("https://power.larc.nasa.gov/cgi-bin/v1/DataAccess.py?&request=execute&identifier=SinglePoint&parameters=ALLSKY_SFC_SW_DWN,T2M_MAX,T2M_MIN&startDate=19830101&endDate=",format(Sys.Date(),"%Y%m%d"),"&userCommunity=AG&tempAverage=DAILY&outputList=ASCII&lat=",lat,"&lon=",lon)
  json_data <- jsonlite::fromJSON(json_file)
  
  
  data_nasa <-  tibble(dates = seq(as.Date("1983/1/1"), as.Date(format(Sys.Date(),"%Y/%m/%d")), "days")) %>%  
    mutate(year = year(dates), month = month(dates), day = day(dates),
           tmin_N = json_data$features$properties$parameter$T2M_MIN %>% unlist, 
           tmax_N = json_data$features$properties$parameter$T2M_MAX %>% unlist) %>% 
    na_if(-99)
return(data_nasa)}


tictoc::tic()
test <- temperatures %>% 
  mutate(data_nasa = purrr::map2(.x = Lat, .y = Lon, .f = Nasa_download)) %>% 
  mutate(join = purrr::map2(.x = data, .y = data_nasa, .f = function(x, y){
    inner_join(x, y) %>% gather(type, value, -dates, -day, -month, -year) }))
tictoc::toc() # 4.05 min. (only with download).




test1 <- test %>% 
  dplyr::select(-data, -data_nasa) %>%
  unnest()



ggplot(test1 , aes(dates, value, colour = type)) + 
  geom_line() + 
  facet_grid(~station) + 
  theme_bw() + 
  labs(x = '', y = 'temperature (c)', colour = '') + 
  theme(legend.position = 'bottom')




shp <- HM_shp <-  raster::getData('GADM', country='HN', level=1) %>% sf::st_as_sf(.) 
dry_C <- sf::read_sf('D:/OneDrive - CGIAR/Desktop/USAID-Regional/USAID-REGIONAL/Honduras_Corredor_Seco.shp') 


test <- test %>% rename( 'x' = 'Lat', 'y' = 'Lon')

ggplot(test) + 
  geom_point(aes(y, x)) +
  theme_bw() + 
  geom_sf(data = shp, fill = NA, color = gray(.5)) +
  geom_sf(data = dry_C, fill = NA, color = gray(.1)) +
  labs(x = 'Longitud', y = 'Latitud', colour = NULL)





# =-=-=-=-=-=-=-=-=


library(qmap)


a <- test %>% 
  dplyr::select(-join) %>% 
  mutate(join = purrr::map2(.x = data, .y = data_nasa, .f = inner_join)) %>% 
  dplyr::select(-data, -data_nasa) %>% 
  filter(row_number() == 1) %>% 
  mutate(mod_tmax = map(.x = join, ~fitQmapRQUANT(.x$tmax, .x$tmax_N, qstep = 0.025, nboot = 100)), 
         mod_tmin = map(.x = join, ~fitQmapRQUANT(.x$tmin, .x$tmin_N, qstep = 0.025, nboot = 100)))


# a <- test %>% 
#   filter(row_number() == 1) %>% 
#   dplyr::select(-data, -data_nasa) %>% 
#   unnest() %>% 
#   dplyr::filter(type %in% c('tmax','tmax_N')) %>% 
#   spread( type, value) %>% 
#   nest(- x, -y, -ALTITUD, -station ) %>% 
#   mutate(mod_tmax = map(.x = data, ~fitQmapRQUANT(.x$tmax, .x$tmax_N, qstep = 0.025, nboot = 100)))
# 
# a$mod_tmax
# 
# 
# data(obsprecip)
# data(modprecip)
# 
# qm.fit <- fitQmapRQUANT(obsprecip[,2],modprecip[,2],
#                         qstep=0.1,nboot=10,wet.day=TRUE)
# qm.a <- doQmapRQUANT(modprecip[,2],qm.fit,type="linear")
# qm.b <- doQmapRQUANT(modprecip[,2],qm.fit,type="tricub")



