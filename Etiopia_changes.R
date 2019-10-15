# =---------------------------------------------
# Cambio para la plataforma de Etiopia. 
# =---------------------------------------------

library(tidyverse)
library(parallel)
library(lubridate)
library(rjson)
library(raster)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# 1. Function to extract NASA POWER daily data 
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# This function 
# INPUT
# * data : station data. 
# * special_data: this contains (lat: latitude of the station / site of interest, 
# lon: longitud of the station / site of interest, year_to: actual year, month_to: actual month).

# OUTPUT: NASA series. 
# It could be possible NASA API in some case sometimes don't work. 
download_data_nasa <- function(data, special_data){
  # data <- Cerete
  # special_data <- tibble(lat, lon, year_to, month_to)
  
  lat <- special_data$lat
  lon <- special_data$lon
  year_to <- special_data$year_to
  month_to <- special_data$month_to
  
  options(timeout=180)
  
  json_file <- paste0("https://power.larc.nasa.gov/cgi-bin/v1/DataAccess.py?&request=execute&identifier=SinglePoint&parameters=ALLSKY_SFC_SW_DWN,T2M_MAX,T2M_MIN&startDate=19830101&endDate=",format(Sys.Date(),"%Y%m%d"),"&userCommunity=AG&tempAverage=DAILY&outputList=ASCII&lat=",lat,"&lon=",lon)
  # Esta mostrando un error que no conozco.
  # json_data <- jsonlite::fromJSON(json_file)
  json_data <- rjson::fromJSON(file=json_file)
  
  
  data_nasa <-  tibble(dates = seq(as.Date("1983/1/1"), as.Date(format(Sys.Date(),"%Y/%m/%d")), "days")) %>%  
    mutate(year_n = year(dates), month = month(dates), day = day(dates),
           t_min = json_data$features[[1]]$properties$parameter$T2M_MIN %>% unlist, 
           t_max = json_data$features[[1]]$properties$parameter$T2M_MAX %>% unlist, 
           sol_rad = json_data$features[[1]]$properties$parameter$ALLSKY_SFC_SW_DWN %>% unlist) %>% 
    na_if(-99)
  
  
  
  # Join observed and NASA data. 
  all_data <- right_join( data %>% 
                            filter(year %in% unique(data_nasa$year_n) ) %>% dplyr::select(-prec),
                          data_nasa %>% 
                            filter(year_n %in% unique(data$year)) %>% 
                            purrr::set_names(c('dates', 'year', 'month', 'day', 't_min_N', 't_max_N', 'sol_rad_N')))
  
  
  # Bias between observed data and NASA data. 
  mean_less <- all_data %>% 
    summarise(mean_max = mean(t_max-t_max_N, na.rm = TRUE), 
              mean_min = mean(t_min - t_min_N, na.rm = TRUE),
              mean_sol_rad = mean(sol_rad - sol_rad_N, na.rm = TRUE))
  
  # data full with mean NASA. 
  nasa_data_dw <- data_nasa %>% 
    filter(year_n == year_to, month == month_to) %>% 
    mutate(t_max = t_max + mean_less$mean_max, 
           t_min = t_min + mean_less$mean_min,
           sol_rad = sol_rad + mean_less$mean_sol_rad) %>% 
    mutate(t_max = ifelse(is.na(t_max), mean(t_max, na.rm = TRUE), t_max),
           t_min = ifelse(is.na(t_min), mean(t_min, na.rm = TRUE), t_min),
           sol_rad = ifelse(is.na(sol_rad), mean(sol_rad, na.rm = TRUE), sol_rad))
  
  return(nasa_data_dw)}


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# 2. Function to extract Chirp data and Join with NASA POWER DATA. 
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# ***** INPUT 
# * data:  path for save files.
# * special_data: data from Chirp and nasa for each station.
# *****  OUTPUT
#  This return resampling scenaries join with satellite data. 
#
# ***** Note: This function save files.
Join_extract <- function(data, special_data, path_Chirp){
  # Download Nasa data. 
  # data<-data_to_replace$stations[[33]]
  # special_data<-data_to_replace$data[[33]]
  
  nasa_data <- download_data_nasa(data, special_data)
  
  # Extract Chirp data 
  # monthly <- ifelse(month_to < 10, glue::glue('0{month_to}'), month_to)
  monthly <- ifelse(month_to < 10, paste0('0', month_to), month_to)
  
  chirp_data <- list.files(path_Chirp, full.names = TRUE, pattern = '.tif') %>%
    stack %>% 
    raster::extract(., data.frame(x= special_data$lon,y= special_data$lat)) %>% # arreglar aqui 
    t() %>% 
    as_tibble() %>% 
    set_names('prec') %>% 
    mutate(names = list.files(path_Chirp,  pattern = '.tif') %>% str_replace('.tif', ''), # arreglar aqui 
           day = names %>% str_sub(. , nchar(names) - 1, nchar(names)) %>% as.numeric()) %>% 
    dplyr::select(-names)
  
  # Join Chirp and NASA data. 
  final_month <- right_join(chirp_data, nasa_data) %>% 
    dplyr::select(day, month, year_n, prec, t_max, t_min, sol_rad) %>% 
    rename(year = year_n)
  
  return(final_month)}