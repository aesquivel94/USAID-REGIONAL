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




catalog %>%
  filter(national_code %in% names_var$cod) %>% 
  names()

