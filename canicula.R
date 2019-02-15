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
# n = 30, atlas de canicula... (intruduccion)




test <- id_year_prec %>% 
  filter(id == 110, year == 2003) %>% 
  unnest() %>% 
  filter(month %in% 5:10) 


test1 <- test %>% 
  mutate(Local = case_when(
    lag(mov) > mov & lead(mov) > mov ~ 1, 
    lead(mov) < mov & lag(mov) < mov ~ 2,
    TRUE ~ 0  ) )


# =-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-
# New information (2016)
# 

start_to <- test1 %>% 
  filter(month %in% c(6,8)) %>% 
  filter(row_number() %in% c(1, nrow(.))) %>% 
  dplyr::select(julian) 


first_max <- test1 %>% 
  filter(month %in% c(5,6)) %>% 
  filter(row_number() %in% c(1, nrow(.))) %>% 
  dplyr::select(julian) 



second_max <- test1 %>% 
  filter(month %in% c(9,10)) %>% 
  filter(row_number() %in% c(1, nrow(.))) %>% 
  dplyr::select(julian) 



ggplot(test1, aes(julian, mov)) + 
  geom_rect(aes(xmin = as.numeric(second_max[1,]), xmax = as.numeric(second_max[2,]), 
                ymin = -Inf, ymax = Inf), fill = 'mediumspringgreen', alpha = 0.01) + 
  geom_rect(aes(xmin = as.numeric(first_max[1,]), xmax = as.numeric(first_max[2,]), 
                ymin = -Inf, ymax = Inf), fill = 'pink', alpha = 0.01)  +
  geom_rect(aes(xmin = as.numeric(start_to[1,]), xmax = as.numeric(start_to[2,]), 
                ymin = -Inf, ymax = Inf), fill = 'skyblue', alpha = 0.01) + 
  geom_line() +
  theme_bw()









# =-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-
# 
# =-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-
# 1. Canicula between Jun 1 to Aug 30. 
# 2. The shortest MSD would have a duration of 10 days. 
# 3. Start usually after May–June.
# 4. End normally taking place around September–October.
# 5. Restriction that the difference of precipitation between them 
#    and the minimum should be at least 20 % per day.






# =-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-


## despues del ultimo maximo no contar esos minimos
## antes del primer maximo no deberian haber minimos
## basicamente buscar entre esos dos
## cuando no tenga final de canicula que ponga el ultimo valor
## fecha posible canicula es entre 1 junio y 30 agosto
# i=2

# First select id and year data set
test <- id_year_prec %>% 
  filter(id == i, year == 2003) %>% 
  unnest() %>% 
  filter(month %in% 5:10) 

# Create Local = (is it a local max or min). 
test1 <- test %>% 
  mutate(Local = case_when(
    lag(mov) > mov & lead(mov) > mov ~ 1, 
    lead(mov) < mov & lag(mov) < mov ~ 2,
    TRUE ~ 0  ) )


# Cumpute the first local max in the data. 
first_maximum <- test1 %>%
  filter(Local == 2) %>%
  filter(row_number()==1) %>%
  pull(julian)

# Fecha del posible inicio de la canicula. 
init_canicula <- test1 %>%
  filter(Local ==2) %>%
  filter(julian >=first_maximum, julian <225) %>%
  top_n(4, wt = mov) %>%
  arrange(desc(mov)) %>%
  filter(row_number() <= 3)

# ultima fecha de la posible canicula 
last_date <- test1 %>%
  filter(row_number() == n())

# buscar el minimo siguiente (incluyendo el final del periodo. )

dates_canicula <- init_canicula %>%
  bind_rows(last_date) %>%
  pull(date)


#  This function compute a Min local min. 
next_minimum <- function(x, fecha){
  
  ## x <- test1
  ## fecha <- dates_canicula
  y <- data_frame(date = fecha, date1 = fecha) %>%
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
  
  
  minimos <- purrr::map(.x = dates_filter, .f = return_df, x)

  result <- purrr::map(.x = minimos, .f = function(x){
    
    x %>%
      arrange(mov) %>%
      filter(row_number()==1)
  }) %>%
    bind_rows() %>%
    arrange(mov) %>%
    filter(row_number()==1)
    
  return(result) }

# This is the canicula min.
posible_minimo <- next_minimum(x = test1, fecha = dates_canicula)


# This part compute if the mind it's in a establish limits. 
cond_canicula <- posible_minimo %>%
  mutate(cond_midsummer = between(month, 6, 8)) %>%
  pull(cond_midsummer)

# day when occur min local min. 
date_minimo <- posible_minimo %>%
  dplyr::select(date) %>%
  pull

# find the possible start canicula. 
left_maximum <- init_canicula %>%
  filter(date < date_minimo)

# dates to find posibble start canicula.
date_maximo <- left_maximum %>%
  dplyr::select(date) %>%
  pull


# Compute the difference of the days with the min local min. 
left_maximum <- tibble(minimo = date_minimo, maximo = date_maximo) %>%
  mutate(day = time_length(maximo - minimo, "day")) %>%
  arrange(desc(day))
  # filter(row_number()==1)


# Aqui se hay un problema 
left_maximum <- inner_join(init_canicula, left_maximum, by = c('date' = 'maximo')) %>%
  mutate(pendiente = (mov - posible_minimo$mov)/mov) %>%
  filter(pendiente >= 0.20) %>%
  arrange(desc(mov)) %>%
  # dplyr::select(pendiente) %>%
  # summarise(mean())
  filter(row_number()==1)




# Try to do second max. 


End_canicula <- test1 %>%
  filter(Local ==2) %>%
  filter(date >= date_minimo + 5) %>%
  top_n(4, wt = mov) %>%
  arrange(desc(mov)) %>%
  filter(row_number() <= 3) %>% 
  mutate(pendiente = (mov - posible_minimo$mov)/mov) %>%
  filter(pendiente >= 0.20) %>% 
  arrange(desc(mov)) %>%
  # dplyr::select(pendiente) %>%
  # summarise(mean())
  filter(row_number()==1)








 
  






if(isTRUE(cond_canicula)){
  ggplot() + 
    geom_line(data = test1, aes(julian, mov)) + 
    # geom_line(aes(julian, prec)) +
    geom_vline(data = left_maximum, mapping=aes(xintercept=julian), color='blue') +
    geom_vline(data = posible_minimo, mapping=aes(xintercept=julian), color='red') +
    geom_vline(data = End_canicula, mapping=aes(xintercept=julian), color='green') +
    geom_vline(xintercept = 220) +
    geom_smooth(data = test1, aes(julian, mov))
  
}else{
  
  cowsay::say("Lo sentimos, Jeferson esta probando otras metodologias", 
              "smallcat", what_color = "blue")
}





test1 %>% 
  mutate(ti = (mov - lag(mov)) /mov) %>% 
  ggplot(.)  +
  geom_line(aes(julian, ti))
  # End_canicula <- 
    






End <- test1 %>%
  filter(Local ==2) %>%
  filter(date >= date_minimo + 5) %>%
  top_n(4, wt = mov) %>%
  arrange(desc(mov)) %>%
  filter(row_number() <= 3) 
    
    



    
End %>% 
    mutate(pendiente = (mov - posible_minimo$mov)/mov) %>%
    filter(pendiente >= 0.20) %>% 
    arrange(desc(mov)) %>%
    filter(row_number()==1)
  


date_minimo
End


proof <- cbind(date_minimo, End) %>% 
  as.tbl() %>% 
  dplyr::select(date_minimo, date, julian) %>% 
  nest(- julian )


proof %>% 
  select(data) %>% unnest

proof %>%
  mutate(increments = purrr::map(.x = data, .f = function(.x){
    # .x <- proof %>% filter(row_number() == 1) %>% dplyr::select(data) %>% unnest
    test1 %>% 
      mutate(ti = (mov - lag(mov))/mov) %>%
      filter(between(date, .x$date_minimo  + 5, .x$date)) %>% 
      summarise(acum_ti = sum(ti), mean_ti = mean(ti), median_ti = median(ti))
  }) ) %>% 
  dplyr::select(-data) %>% 
  unnest()
  



  
  

