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
library(ncdf4)
library(raster)
library(lubridate)

route <- 'D:/OneDrive - CGIAR/Desktop/USAID-Regional/USAID-REGIONAL/station_data/grilled/data.nc'

nc_chirps <- ncdf4::nc_open(route)
# nc_proof %>% names
# nc_proof$dim$X$vals
# nc_proof$dim$Y$vals
# nc_proof$dim$T$vals 
# nc_proof$dim$T$len


# make_date(1982,1,1) + (13514 - 1)

HM_shp <-  getData('GADM', country='HN', level=0)
plot(HM_shp)



HND <- stack(route) %>% 
  flip(1) %>% 
  crop(., HM_shp) %>% 
  mask(., HM_shp)




tictoc::tic()
Prec_table <- tibble(layer = 1:nc_chirps$dim$T$len) %>% 
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



library(pracma)

  
idea <- Prec_table %>% 
  unnest  %>%
  nest(-year, -id)
  

row_1 <- idea %>% 
  filter(row_number() == 1) %>% 
  unnest

movavg(x = row_1$prec, n = 31 , type = 't') %>% plot(. ,  type="l")
movavg(x = row_1$prec, n = 31 , type = 't') %>% length()




test <- idea %>% 
  filter(id == 150, year == 1994) %>% 
  unnest() %>% 
  mutate(mov = movavg(x = prec, n = 31, type = 't')) %>% 
  filter(month %in% 6:9) 




test %>%
  ggplot(aes(julian, mov)) + 
  geom_line() + 
  geom_vline(xintercept = 255) +
  # geom_point() +
  theme_bw()
  
  



ajam <- idea %>% 
  filter(id == 150, year == 1994) %>% 
  unnest() %>% 
  mutate(mov = movavg(x = prec, n = 31, type = 't')) %>% 
  filter(month %in% 6:9) 





# 
# pixel_year <- idea %>% 
#   filter(id == 1, year == 1982) %>% 
#   unnest() %>% 
#   mutate(mov = movavg(x = prec, n = 31, type = 't')) %>% 
#   filter(month %in% 5:9) 
# 
# 
# 
# pixel_year[28, ]
# 
# 
# which.min(pixel_year$mov)
# 
# pixel_year %>% 
#   filter(row_number() == which.min(mov))



x <- ajam$julian
y <- ajam$mov


in_p<-inflexi(x,y,1,length(x),5,5,plots=TRUE);in_p$an;in_p$finfl

in_p %>% str



library(RootsExtremaInflections)

data(xydat)
#
#Extract x and y variables:
#
x=xydat$x;y=xydat$y
#
#Find inflection point, plot results, print Taylor coefficients and rho estimation:
#
d<-inflexi(x,y,1,length(x),5,5,plots=TRUE);d$an;d$finfl







ajam %>%  
  mutate(local.minima = if_else(lag(mov) > mov & lead(mov) > mov, TRUE, FALSE)) %>% 
  mutate(local.maxima = if_else(lead(mov) < mov & lag(mov) < mov, TRUE, FALSE)) %>% 
  filter(local.maxima == TRUE) 


ggplot(ajam, aes(julian, mov)) + 
  geom_line() + 
  # geom_vline(xintercept = c(213, 238, 248, 251, 256)) 
  geom_vline(xintercept = c(165, 237, 241, 249, 255)) 


