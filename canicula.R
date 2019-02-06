rm(list = ls()); gc(reset = TRUE)
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Made by:     Alejandra Esquivel Arias. 
# Created in:  Date: 2-2019
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Canícula Methodology


library(tidyverse)
library(viridis)
library(tictoc)
library(glue)


#  Read data

tibble(month = 1:12) %>% 
  mutate(name = month.abb) %>% 
  filter(month %in% 5:9)


# prec_daily_qc <- read_csv("station_data/Data_diaria_copeco/prec_daily_qc.csv")
 
# prec_daily_qc %>% 
#   filter(month %in% 5:9) %>% 
#   View

library(ncdf4)
library(raster)

route <- 'D:/OneDrive - CGIAR/Desktop/USAID-Regional/USAID-REGIONAL/station_data/grilled/data.nc'

# nc_proof <- ncdf4::nc_open(route)
# nc_proof %>% names
# nc_proof$dim$X$vals
# nc_proof$dim$Y$vals
# nc_proof$dim$T$vals 
# nc_proof$dim$T$len





  
library(lubridate)

# make_date(1982,1,1) + (13514 - 1)





ajam <-  getData('GADM', country='HN', level=0)
plot(ajam)

proof <- stack(route) %>% flip(1)



HND <- crop(proof, ajam)
HND <- mask(HND, ajam)

# extent(HND) <- extent(ajam)

plot(HND[[1]]) 
plot(ajam, add = TRUE)



tictoc::tic()
Prec_table <- tibble(layer = 1:nc_proof$dim$T$len) %>% 
  mutate(date = make_date(1982,1,1) + (layer-1), julian = yday(date), 
         year = year(date), month = month(date)) %>%  
  # filter(row_number() < 2) %>% 
  mutate(raster = purrr::map(.x = layer, 
                             .f = function(.x){ HND[[.x]] %>% 
                                 rasterToPoints(.) %>% 
                                 data.frame() %>% 
                                 set_names('x', 'y', 'prec') %>% 
                                 mutate(id = 1:nrow(.))} ))
tictoc::toc()
# 728.19 sec = 12.1365


# row_1 <- 



library(pracma)



  
idea <- Prec_table %>% 
  unnest  %>%
  nest(-year, -id)
  



  

row_1 <- idea %>% 
  filter(row_number() == 1) %>% 
  unnest

movavg(x = row_1$prec, n = 31 , type = 't') %>% plot(. ,  type="l")
movavg(x = row_1$prec, n = 31 , type = 't') %>% length()




idea %>% 
  filter(id == 2, year == 2018) %>% 
  unnest() %>% 
  mutate(mov = movavg(x = prec, n = 31, type = 't')) %>% 
  filter(month %in% 6:9) %>% 
  ggplot(aes(as.numeric(julian), mov)) + 
  geom_line() + 
  theme_bw()
  
  






