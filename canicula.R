rm(list = ls()); gc(reset = TRUE)
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Made by:     Alejandra Esquivel Arias. 
# Created in:  Date: 2-2019
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Canícula (Midsummer Drought) Methodology.

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
# n = 30, atlas de canicula... (introduccion)... But in the other papers said 31 days.




# =-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-
# (2016). Midsummer drought. 
# =-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-
# 1. Canicula between Jun 1 to Aug 30. 
# 2. The shortest MSD would have a duration of 10 days. 
# 3. Start usually after May–June.
# 4. End normally taking place around September–October.
# 5. Restriction that the difference of precipitation between them 
#    and the minimum should be at least 20 % per day.


# Other restrictions:

# After the last local maximum we shouldn't have local minimum.
# Before the last local minimum. we shouldn't have local maximum
# i=2.

# First select id and year data set and 
# create variable Local (define if it's or not local min-max).
pixel_yearT <- id_year_prec %>% 
  filter(id == i, year == 2003) %>% 
  unnest() %>% 
  filter(month %in% 5:10) %>% 
  mutate(Local = case_when(
    lag(mov) > mov & lead(mov) > mov ~ 1, 
    lead(mov) < mov & lag(mov) < mov ~ 2,
    TRUE ~ 0  ) )


# =-=-=-=
# I guess in this part we need to do the big function. 
# =-=-=-=


# Compute the first local max in the data.
first_maximum <- pixel_yearT %>%
  filter(Local == 2) %>%
  filter(row_number()==1) %>%
  pull(julian)




# Dates would be possible start MSD.
init_canicula <- pixel_yearT %>%
  filter(Local ==2) %>%
  filter(julian >=first_maximum, julian <225) %>%
  top_n(4, wt = mov) %>%
  arrange(desc(mov)) %>%
  filter(row_number() <= 3)




# Last date would be possible MSD. 
last_date <- pixel_yearT %>%
  filter(row_number() == n())


# Look for the next local minimun (including the end of the period). 
dates_canicula <- init_canicula %>%
  bind_rows(last_date) %>%
  pull(date)



# =-=-=-=-=-=-=-=-=-=-=-= Function. 

#  This function compute a Min local min. 
next_minimum <- function(x, fecha){
  
  ## x <- pixel_yearT
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

  
  # If don't find local min in the window, that retuns data frame like: 
  # year    id layer date       julian month     x     y  prec   mov Local
  
  return_df <- function(fecha, x){
    # pixel_yearT %>%
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

# =-=-=-=-=-=-=-=-=-=-=-= 

 

# In this point we compute the Local min. 
posible_minimo <- next_minimum(x = pixel_yearT, fecha = dates_canicula)



# This point we compute if the Local min is in a establish limits. 
cond_canicula <- posible_minimo %>%
  mutate(cond_midsummer = between(month, 6, 8)) %>%
  pull(cond_midsummer)



# =-=-=-=-=-=-=-=-=-=-=-= Function. 

# Function to do start MSD. This it's the first maximum.

left_max_MSD <- function(Possible_dates, MinDate){
  
  # Possible_dates <- init_canicula
  # MinDate <- posible_minimo
  
  
  # day when occur min local min. 
  date_minimo <- MinDate %>%
    dplyr::select(date) %>%
    pull
  
  
  # find the possible start canicula. 
  left_maximum <- Possible_dates %>%
    filter(date < date_minimo - 5)
  
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
  left_maximum <- inner_join(Possible_dates, left_maximum, by = c('date' = 'maximo')) %>%
    mutate(pendiente = (mov - posible_minimo$mov)/mov) %>%
    filter(pendiente >= 0.20) %>%
    arrange(desc(mov)) %>%
    # dplyr::select(pendiente) %>%
    # summarise(mean())
    filter(row_number()==1)

  
return(left_maximum)}

# =-=-=-=-=-=-=-=-=-=-=-= 

# In this point we compute the start MSD. 
left_start <- left_max_MSD(Possible_dates = init_canicula, MinDate =  posible_minimo)






# =-=-=-=-=-=-=-=-=-=-=-= Function. 

# Try to do second max. 
right_max_MSD <- function(pyT, MinDate){
  # pyT <- pixel_yearT
  # MinDate <- posible_minimo

  # day when occur min local min. 
  date_minimo <- MinDate %>%
    dplyr::select(date) %>%
    pull
  
  End <- pyT %>%
    filter(Local ==2, date >= date_minimo + 5)  %>%
    top_n(4, wt = mov) %>%
    arrange(desc(mov))  %>%
    mutate(pendiente = (mov - MinDate$mov)/mov)  %>%
    filter(pendiente >= 0.20)  
  
  
  
  End_1 <- End %>% 
    arrange(desc(pendiente)) %>% 
    filter(row_number() == 1) %>% 
    dplyr::select(-pendiente)
  
  
  second_max <- End %>% 
    mutate(date_minimo = date_minimo) %>% 
    nest(- julian ) %>%
    mutate(increments = purrr::map(.x = data, .f = function(.x){ # Aqui...
      # .x <-pyT %>% filter(Local ==2, date >= date_minimo + 5) %>% top_n(4, wt = mov) %>%
      #   arrange(desc(mov))  %>%  mutate(pendiente = (mov - MinDate$mov)/mov)  %>%
      #   filter(pendiente >= 0.20)  %>% mutate(date_minimo = date_minimo) %>%
      #   nest(- julian ) %>% filter(row_number() == 1) %>% dplyr::select(data) %>% unnest

      pyT %>% 
        mutate(ti = (mov - lag(mov))/mov) %>%
        filter(between(date, .x$date_minimo  + 5, .x$date)) %>% 
        summarise(acum_ti = sum(ti), mean_ti = mean(ti), median_ti = median(ti))
    }) ) %>% 
    dplyr::select(-data)  %>% 
    unnest() %>%
    arrange(desc(mean_ti)) %>% 
    filter(row_number() == 1) 
  
 
  second_max <-   pyT %>% 
    filter(julian ==  second_max %>% dplyr::select(julian) %>% as.numeric()) %>% 
    bind_rows(End_1) %>% 
    mutate(type = c('End', 'End2'))
 
 return(second_max)}

# =-=-=-=-=-=-=-=-=-=-=-= 

right_End <- right_max_MSD(pyT = pixel_yearT, MinDate = posible_minimo)


if(isTRUE(cond_canicula)){
  ggplot() + 
    geom_line(data = pixel_yearT, aes(julian, mov)) + 
    # geom_line(aes(julian, prec)) +
    geom_vline(data = left_start, mapping=aes(xintercept=julian), color='blue') +
    geom_vline(data = posible_minimo, mapping=aes(xintercept=julian), color='red') +
    geom_vline(data = right_End, mapping=aes(xintercept=julian)) +
    geom_smooth(data = pixel_yearT, aes(julian, mov)) +
    labs(x = 'Julian day', y = 'Triangular Moving Average (mm)') + 
    theme_light()
  
}else{
  
  cowsay::say("Lo sentimos, Jeferson esta probando otras metodologias", 
              "smallcat", what_color = "blue")
}


# Create MDS Index.
    
 MSD <- bind_rows(left_start, posible_minimo, right_End) %>% 
    dplyr::select(-minimo, -day, -pendiente) %>% 
    mutate(type = c('Start', 'Min', 'End', 'End2'))
  
  
    

 
 MSD_index <- function(pyT, canicula){
  # pyT <- pixel_yearT
  # canicula <- MSD
 
    
 Length <- canicula$julian[3] - canicula$julian[1]
 
 Intensity <- pyT %>% 
   filter(between(julian, canicula$julian[1], canicula$julian[3])) %>% 
   summarise(Intensity =  mean(prec)) %>% 
   as.numeric()
 
 Magnitude <- pyT %>% 
   filter(julian == canicula$julian[2]) %>% 
   dplyr::select(prec) %>% 
   as.numeric()

 
 
 MSD_I <- canicula %>% 
   dplyr::select(id, x, y, julian) %>% 
   filter(row_number() == 2) %>% 
   rename(Min = 'julian') %>% 
   bind_cols(.,  tibble(Start = canicula$julian[1], End = canicula$julian[3], End_P = canicula$julian[4],
                        Length = Length, Intensity = Intensity, Magnitude = Magnitude) )
 
 
 
return(MSD_I)}

     







    
    



    

  



  
  

