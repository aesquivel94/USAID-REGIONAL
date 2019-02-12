rm(list = ls()); gc(reset = TRUE)
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Made by:     Alejandra Esquivel Arias. 
# Created in:  Date: 2-2019
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Canícula Methodology

# Chirps
# https://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRPS/.v2p0/.daily-improved/.global/.0p25/.prcp/X/%2882W%29%2893W%29RANGEEDGES/Y/%2817N%29%2810N%29RANGEEDGES/T/%281%20Jan%201982%29%2831%20Dec%202018%29RANGEEDGES/index.html


# =-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-
# Packages. 
# =-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-
library(tidyverse)
library(viridis)
library(tictoc)
library(glue)
library(ncdf4)
library(raster)
library(lubridate)
library(pracma)
# =-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-


# =-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-
# Path --- where are files.
# =-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-

# Chirps path
route <- 'D:/OneDrive - CGIAR/Desktop/USAID-Regional/USAID-REGIONAL/station_data/grilled/data.nc'

# =-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-


# =-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-
# Spatial data reading 
# =-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-

# We Get shp data, if you want other country change country,
# you would find code in: http://kirste.userpage.fu-berlin.de/diverse/doc/ISO_3166.html. 
HM_shp <-  getData('GADM', country='HN', level=0)
# plot(HM_shp)

# Read all Chirps bands and crop by shp file (country). 
HND <- stack(route) %>% 
  flip(1) %>% 
  crop(., HM_shp) %>% 
  mask(., HM_shp)


# Read nc file for see the parameteres. 
nc_chirps <- ncdf4::nc_open(route)
# nc_proof %>% names
# nc_proof$dim$X$vals
# nc_proof$dim$Y$vals
# nc_proof$dim$T$vals 
# nc_proof$dim$T$len


# make_date(1982,1,1) + (13514 - 1)



# =-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-
# Data manasment.  
# =-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-


# Create a tibble with all dates and raster layers like tables. 
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




# Do moving triangular average Filter - by year and pixel. 
  
id_year_prec <- Prec_table %>% 
  unnest  %>%
  nest(-year, -id)  %>% 
  mutate(data = purrr::map(.x = data, .f = function(.x){ data <- .x %>% 
      mutate(mov = movavg(x = prec, n = 31, type = 't')) })) 





test <- id_year_prec %>% 
  filter(id == 1, year == 2010) %>% 
  unnest() %>% 
  filter(month %in% 6:9) 


test1 <- test %>% 
  mutate(Local = case_when(
    lag(mov) > mov & lead(mov) > mov ~ 1, 
    lead(mov) < mov & lag(mov) < mov ~ 2,
    TRUE ~ 0  ) )
## despues del ultimo maximo no contar esos minimos
## antes del primer maximo no deberian haber minimos
## basicamente buscar entre esos dos
## cuando no tenga final de canicula que ponga el ultimo valor


first_maximum <- test1 %>%
  filter(Local == 2) %>%
  filter(row_number()==1) %>%
  pull(julian)

init_canicula <- test1 %>%
  filter(Local ==2) %>%
  filter(julian >=first_maximum, julian <225) %>%
  top_n(4, wt = mov) %>%
  arrange(desc(mov)) %>%
  filter(row_number() <= 3)

last_date <- test1 %>%
  filter(row_number() == n())

# buscar el minimo siguiente

dates_canicula <- init_canicula %>%
  bind_rows(last_date) %>%
  pull(date)

next_minimum <- function(x, fecha){
  
  ## x <- test1
  ## fecha <- dates_canicula
  y = data_frame(date = dates_canicula, date1 = dates_canicula) %>%
    complete(date, date1) %>%
    mutate(diff_length = time_length(date - date1, unit = "day")) %>%
    filter(diff_length < 0) %>%
    group_by(date) %>%
    filter(row_number()==1) %>%
    ungroup() %>%
    mutate(id = 1:nrow(.)) %>%
    dplyr::select(-diff_length) %>%
    nest(-id) %>%
    dplyr::select(-id) %>%
    unlist(recursive = F)
    
  split_date <- function(x){
    
    x %>%
      mutate(id = 1) %>%
      gather(date, value, -id) %>%
      pull(value)
  }
  
  dates_filter <- purrr::map(.x = y, .f = split_date)

  
  ### si no encuentra minimos en la ventana que retorne un dataframe como:
  # year    id layer date       julian month     x     y  prec   mov Local
  
  return_df <- function(fecha, x){
    
    # test1 %>%
      # filter(between(date, dates_filter[[1]][1], dates_filter[[1]][2]))

    x %>%
      filter(between(date, fecha[1], fecha[2])) %>%
      filter(Local ==1)
    
  }
  
  
  minimos <- purrr::map(.x = dates_filter, .f = return_df, test1)

  result <- purrr::map(.x = minimos, .f = function(x){
    
    x %>%
      arrange(mov) %>%
      filter(row_number()==1)
  }) %>%
    bind_rows() %>%
    arrange(mov) %>%
    filter(row_number()==1)
    
  return(result)
  
}
posible_minimo <- next_minimum(x = test1, fecha = dates_canicula)
  # filter(julian >= 219)
  ggplot() + 
  geom_line(data = test1, aes(julian, mov)) + 
  # geom_line(aes(julian, prec)) +
    geom_vline(data = init_canicula, mapping=aes(xintercept=julian), color='blue') +
    geom_vline(data = posible_minimo, mapping=aes(xintercept=julian), color='red')
  
  
  geom_vline(xintercept = c(213, 238, 248, 251, 256))
  geom_vline(data = test1 %>% filter(Local == 1), mapping=aes(xintercept=julian), color='blue') +
    geom_vline(data = test1 %>% filter(Local == 2), mapping=aes(xintercept=julian), color='red')





test <-  test %>% 
  mutate(Local = case_when(
    lag(mov) > mov & lead(mov) > mov ~ 1, 
    lead(mov) < mov & lag(mov) < mov ~ 2,
    TRUE ~ 0  ) ) %>% 
   filter(Local != 0) 
 





# test %>% 
#   mutate(cond = case_when(
#     Local == 1 & row_number() == n() ~ n(), 
#     TRUE ~ 0 
#   ))







test %>% 
  filter(Local != 0) %>%
  arrange(desc(mov)) %>% 
  mutate(Local_less = Local - lead(Local)) %>% 
  filter(Local_less == 0) %>% # fix
  arrange(julian) %>% 
  dplyr::select(-Local_less) %>% 
  mutate(condition = case_when(
    mov == min(mov) ~  1, 
    # mov == max(mov) ~ 1, 
    TRUE ~ 0
  )) %>% ggplot(aes(julian, mov, colour = as.factor(Local))) + geom_point()  







