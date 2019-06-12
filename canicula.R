rm(list = ls()); gc(reset = TRUE)
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Made by:     Alejandra Esquivel Arias. 
# Created in:  Date: 5-2019
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Canícula (Midsummer Drought) Methodology.

# Chirps
# https://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRPS/.v2p0/.daily-improved/.global/.0p25/.prcp/X/%2882W%29%2893W%29RANGEEDGES/Y/%2817N%29%2810N%29RANGEEDGES/T/%281%20Jan%201982%29%2831%20Dec%202018%29RANGEEDGES/index.html

# ERSST -- x 130 340 ; y -15 35

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
library(cowsay) # For now i don't use this packages. 
library(sf)
library(skimr)
library(gganimate)
library(magick)
# =-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-


# =-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-
# Chirps data.
# =-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-

# .------..------..------..------..------..------.
# |C.--. ||H.--. ||I.--. ||R.--. ||P.--. ||S.--. |
# | :/\: || :/\: || (\/) || :(): || :/\: || :/\: |
# | :\/: || (__) || :\/: || ()() || (__) || :\/: |
# | '--'C|| '--'H|| '--'I|| '--'R|| '--'P|| '--'S|
# `------'`------'`------'`------'`------'`------'
#  Only do de extraction of the data.        
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
HM_shp <-  getData('GADM', country='HN', level=1)
# plot(HM_shp)

# Read all Chirps bands and crop by shp file (country). 
HND <- stack(route) %>% 
  flip(1) %>% 
  crop(., HM_shp)  %>% 
  mask(., HM_shp)


# Read nc file for see the parameteres. 
nc_chirps <- ncdf4::nc_open(route)


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



# =-=-=-=-=-=-=-=
shp <- st_as_sf(HM_shp) 
dry_C <- read_sf('D:/OneDrive - CGIAR/Desktop/USAID-Regional/USAID-REGIONAL/Honduras_Corredor_Seco.shp') 

# This graph do a gif graph. 
# anim <- ggplot(p_data) +
#   geom_tile(aes(x, y, fill = Magnitude )) + 
#   scale_fill_viridis(na.value="white",  direction = -1) + 
#   geom_sf(data = shp, fill = NA, color = gray(.5)) +
#   geom_sf(data = dry_C, fill = NA, color = gray(.1)) + 
#   theme_bw() +
#   # Here comes the gganimate specific bits
#   labs(title = 'Year: {closest_state}', x = 'Longitud', y = 'Latitud') + 
#   transition_states(year, transition_length = 5, state_length = 8) +
#   enter_fade() +
#   exit_fade()


# anim_save("MSD_Index/Magnitude_Chirps.gif", anim)


# =-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-
# Station data ---  Only process data station. 
# =-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-

# .------..------..------..------..------..------..------..------.
# |S.--. ||T.--. ||A.--. ||T.--. ||I.--. ||O.--. ||N.--. ||S.--. |
# | :/\: || :/\: || (\/) || :/\: || (\/) || :/\: || :(): || :/\: |
# | :\/: || (__) || :\/: || (__) || :\/: || :\/: || ()() || :\/: |
# | '--'S|| '--'T|| '--'A|| '--'T|| '--'I|| '--'O|| '--'N|| '--'S|
# `------'`------'`------'`------'`------'`------'`------'`------'

# Read data

Station <- read_csv("station_data/Data_diaria_copeco/prec_daily_qc.csv") %>% 
  filter(year > 1981) 

Catalog <- read_csv("station_data/Data_diaria_copeco/stations_catalog_copeco.csv")

 
# Summary station data
Station %>% 
  dplyr::select(-day, -month, -year) %>% 
  skimr::skim(.) 

# Count NA total.
Count_NA <- Station %>% 
  dplyr::select(-day, -month, -year)  %>%  # replace to your needs
  summarise_all(funs(sum(is.na(.)))) %>% 
  gather() %>% 
  mutate(percent = round(value / 13149 * 100, 2))


# Count_NA %>% write_csv('MSD_Index/Count_NA_Stations.csv')
# arrange(Count_NA, percent) %>% View

 
# Lat and Long
Catalog_M <- Catalog %>%  
  mutate(national_code= str_replace(national_code, '-', '.')) %>%
  mutate(station_N = ifelse(str_detect(national_code, 'SMN.HON'),
                            glue::glue("{national_code}_{name_station}"), 
                            glue::glue("X{national_code}_{name_station}")) ) %>%
  dplyr::select(station_N, Lon, Lat)
  


# Data Filling with Chirps 

Station_Chirps <- raster::extract(HND, Catalog_M %>% dplyr::select(Lon, Lat)) %>% 
  as_tibble() %>% 
  bind_cols(Catalog_M, .) %>% 
  gather(layer, Chirps, -station_N, -Lon, -Lat ) %>%
  mutate(layer = str_remove(layer, 'X')) %>% 
  nest(-station_N)


# MSD_data <- read_csv('MSD_Index/MSD_G.csv')

# Function for do dates...
do_dates <- function(Chirps_S){
  # Chirps_S <- Station_Chirps %>% filter(station_N == 'SMN.HOND020_Galeras') %>% unnest
  
  data_dates <- Chirps_S %>%
    mutate(date = seq( make_date(1982,1,1) , make_date(1982,1,1) + (nrow(.)-1), by = 'day'), 
           julian = yday(date), 
           year = year(date), month = month(date))
  
return(data_dates)}


# =-=-
Station_Chirps <- Station_Chirps %>% 
  mutate(Data_dates = purrr::map(.x = data, .f = do_dates)) %>%
  dplyr::select(-data) %>%
  unnest() 

StationM <- Station %>% 
  mutate(date = seq( make_date(1982,1,1) , make_date(2017,12,31), by = 'day')) %>% 
  gather(station_N, prec, -day, -month, -year, -date)


# =-=-=-=-=- 
# nrow( Station_Chirps %>% filter(year < 2018)) == nrow(StationM)
Joint_CS <-  inner_join(StationM, Station_Chirps %>%  filter(year < 2018)) %>% 
  dplyr::select(-layer) %>% 
  nest(-station_N, -Lon , -Lat) %>%  
  mutate(NA_percent = purrr::map(.x = data,.f = function(.x){
    .x %>% summarise(NA_percent = sum(is.na(prec))/n() * 100)})) %>% 
  unnest(NA_percent) %>% 
  filter(NA_percent <= 25)


# =-=-=-=-=- Esta parte se adiciono. 
# Filtrar para eliminar las estaciones que tengan más del 25% de datos NA 

# No idea but, i want to save this... 
year_Join_CS <- Joint_CS %>% 
  unnest %>% 
  nest(-station_N, -Lon,   -Lat, -year)


# =-=-=-=-= Filling data...
library(broom)

idea_models <- Joint_CS %>% 
  unnest %>% 
  group_by(station_N) %>% 
  na.omit() %>% 
  do(fitHour = lm(prec ~ Chirps, data = .)) 


# Coeficients 
tidy_models <- idea_models %>% 
  tidy(fitHour)


# Intervalos de confianza para los modelos por estacion - Hay que tener en cuenta 
# que estos modelos no pasan por 0, es decir que no se conseguira precipitaciones diarias 0. 
Joint_CS %>% 
  unnest %>% 
  group_by(station_N) %>% 
  na.omit() %>% 
  do(fitHour = confint_tidy(lm(prec ~ Chirps, data = .))  ) %>% 
  unnest


# Resumen de los modelos (el R2 más grande es de 0.23). 
glance(idea_models, fitHour) %>% 
  write_csv('MSD_Index/Station_models.csv')

#  fitted values and residuals 
# augment(idea_models, fitHour)
# get the summary statistics by group in a tidy data_frame
# glance(idea_models, fitHour)

coef <- tidy_models %>% 
  dplyr::select(station_N, term, estimate) %>% 
  nest(-station_N) %>% 
  rename(coef = 'data')


# This function do the filling data... with Chirps. 
filling_data <- function(data, coefficient){
  # row_1 <- Joint_CS %>% right_join(., coef) %>% filter(row_number() == 2)
  # data <-  dplyr::select(row_1, data) %>% unnest
  # coefficient <- dplyr::select(row_1, coef) %>% unnest

  coefficient <- coefficient %>% 
    spread(key = term, value = estimate)  %>% 
    set_names('a', 'b')
  
  a <- coefficient %>% 
    dplyr::select(a) %>% 
    as.numeric()
  
  b <- coefficient %>% 
    dplyr::select(b) %>% 
    as.numeric()

    
  Fill_data <- data %>% 
    mutate(prec_R =  if_else(is.na(prec), a +  (b * Chirps), prec), 
           prec_C = if_else(is.na(prec), Chirps, prec)) 
    
return(Fill_data)}


# =-=-=-=-=-=-=-=-=-=-=--=--=-=-=-=-=-=-=

# =-=-=-=-=-=-=
# Fix this part
new_JointCS <-  Joint_CS %>%
  right_join(., coef) %>%
  mutate(data_filling = purrr::map2(.x = data, .y = coef, .f = filling_data)) %>%
  dplyr::select(-data, -coef) %>%
  mutate(data = purrr::map(.x = data_filling, .f = function(.x){ data <- .x %>%
    mutate(mov = movavg(x = prec_C, n = 31, type = 't'),
           mov_R = movavg(x = prec_R, n = 31, type = 't')) }))




# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Proof a new metogology spliting data time serie.

# .------..------..------..------..------.
# |P.--. ||R.--. ||O.--. ||O.--. ||F.--. |
# | :/\: || :(): || :/\: || :/\: || :(): |
# | (__) || ()() || :\/: || :\/: || ()() |
# | '--'P|| '--'R|| '--'O|| '--'O|| '--'F|
# `------'`------'`------'`------'`------'


##############################################################################
station <- read_csv("Drought/dr.csv")


# This function (curve HoltWinters) summarise the time 
# filter by year and compute HoltWinters - triangular mean. 
curve_HoltWinters <- function(dr){
  
  # Testing other methodologies...
  year_pixel <-  dr %>% 
    dplyr::select(day, month , julian, prec_C)
  # mutate(mov =  movavg(x = prec_C, n = 31, type = 't'))
  
  rain <- HoltWinters(year_pixel$prec_C, beta=FALSE, gamma=FALSE)$fitted[,1] 
  # rainseriesforecasts <- HoltWinters(rainfall, beta=FALSE, gamma=FALSE)
  
  ideas_try <- year_pixel %>%
    mutate(mov =  movavg(x = prec_C, n = 31, type = 't'), ts = c(0,rain), 
           mov_ts = movavg(x = ts, n = 31, type = 't')) %>% 
    dplyr::select(-prec_C)
  
  
  jmm <- ideas_try %>% 
    filter(month %in% 5:8) %>% 
    dplyr::select(julian, month, mov_ts) %>% 
    mutate(type = case_when(
      lag(mov_ts) - mov_ts > 0  & lead(mov_ts) - mov_ts > 0  ~ 1, 
      lead(mov_ts) - mov_ts < 0 & lag(mov_ts) - mov_ts < 0  ~ 2,
      TRUE ~ 0  ) )  
  return(jmm)}

# Here we will do step 3. 
# This function find the canicula when the general local maximo is canicula's start.
canicula_after <- function(local_max, mins, maxs){
  # local_max <- GL_max; mins <- min; maxs <- max
  
  # Step 3. Find the minimum ... possible.
  
  # This computes all possible local min after general local max. 
  min_after <- mins %>% 
    filter(julian > local_max$julian)
  
  # This line join max with mins in one tibble. 
  base_filter_Ind <- bind_rows(min_after, maxs) %>%
    dplyr::select(-testing) %>%
    arrange(julian)
  
  # This line calculate the number of rows... that the dataset has.
  nrow_base <- nrow(min_after)
  
  # making the arrangement or trap to add mutates.
  min_after <- base_filter_Ind %>%
    filter(row_number() > 1) 
  
  # add mutates.
  j <- 1 
  for(i in 1:nrow_base){
    # print(i) ; print(j)
    
    # cat(glue::glue('(i = {i} ; j = {j}) '))
    varname <- glue::glue('sub{j}L')
    varname1 <- glue::glue('jul{j}L')
    
    min_after <- min_after %>% 
      mutate(!!varname := mov_ts - lead(mov_ts, n = j), 
             !!varname1 := lead(julian, n = j) - julian) 
    
    j <-  (2*i) + 1 }
  
  # Here we extract the second local max. 
  
  jul_sub <- min_after %>%
    filter(type == 1)  %>% 
    select(julian, month, mov_ts, type, contains('jul'))  %>% 
    gather(type, jul_sub, -julian, -month, -mov_ts, -type) %>% 
    mutate(type = str_replace(type, 'jul', 'sub'))
  
  min_after <- min_after %>% 
      filter(type == 1)  %>% 
      select(julian, month, mov_ts, type, contains('sub')) %>% 
      gather(type, value, -julian, -month, -mov_ts, -type) %>% 
      right_join(. , jul_sub) %>% 
      filter(jul_sub > 5) %>% 
      arrange(value)  %>% 
      slice(1) %>% 
    dplyr::select(-contains('jul_'))
    
  
  # If we find min... do second max and canicula. 
  # In other case only create canicula with data NA.
  if(nrow(min_after) != 0){
    # Ok this line select the second max.     
    second_max <- maxs %>% 
      filter(julian > min_after$julian) %>% 
      mutate(num = paste0( 'sub', seq(1, (2*(nrow(.)-1) + 1), by = 2), 'L')) %>% 
      filter(num == min_after$type)

    # (2*(1-1) + 1) ; (2*(2-1) + 1) ; (2*(3-1) + 1) ; (2*(4-1) + 1)
    # Here we create the final object...
    canicula_after <- tibble(start_date = local_max$julian, min_date = min_after$julian, 
                             end_date = second_max$julian, length = end_date - start_date)
  }else{
    canicula_after <- tibble(start_date = NA, min_date = NA, end_date = NA, length = NA)
  }
  
  return(canicula_after)}

# Step 4. Here we will do step 4. The idea it's run before_canicula function.  
canicula_before <- function(local_max, mins, maxs){
  # local_max <- GL_max; mins <- min; maxs <- max
  
  
  # Step 4. Find the minimum ... possible. 
  min_before <- mins %>% 
    filter(julian < local_max$julian)
  
  # This line join max with mins in one tibble.  
  min_b <- bind_rows(min_before, maxs) %>%
    dplyr::select(-testing) %>%
    arrange(julian)
  
  
  # This line calculate the number of rows... that the dataset has.
  nrow_base <- nrow(min_before)
  
  # making the arrangement or trap to add mutates.
  # =-=-=-=-=-=
  j <- 1 
  for(i in 1:nrow_base){
    # print(i) ; print(j)
    
    # cat(glue::glue('(i = {i} ; j = {j}) '))
    varname <- glue::glue('sub{j}L')
    varname1 <- glue::glue('jul{j}L')
    
    min_b <- min_b %>% 
      mutate(!!varname := mov_ts - lag(mov_ts, n = j), 
             !!varname1 := julian - lag(julian, n = j)) 
    
    j <-  (2*i) + 1 }
  
  
  jul_sub <- min_b %>%
    filter(type == 1)  %>% 
    select(julian, month, mov_ts, type, contains('jul'))  %>% 
    gather(type, jul_sub, -julian, -month, -mov_ts, -type) %>% 
    mutate(type = str_replace(type, 'jul', 'sub'))
  
  min_b <- min_b %>% 
    filter(type == 1)  %>% 
    select(julian, month, mov_ts, type, contains('sub')) %>% 
    gather(type, value, -julian, -month, -mov_ts, -type) %>% 
    right_join(. , jul_sub) %>% 
    filter(jul_sub > 5) %>% 
    arrange(value)  %>% 
    slice(1) %>% 
    dplyr::select(-contains('jul_'))
  
  
  # If we find min... do first max and canicula. 
  # In other case only create canicula with data NA.
  if(nrow(min_b) != 0){
    
    
    # seq(1, (2*(nrow(.)-1) + 1), by = 2)
    
    ini_max <- maxs %>% 
      filter(julian < min_b$julian) %>% 
      # mutate(num = paste0( 'sub',rev(1:nrow(.)), 'L')) %>%
      mutate(num = paste0( 'sub', rev(seq(1, (2*(nrow(.)-1) + 1), by = 2)), 'L')) %>%
      filter(num == min_b$type)
    
    # Here we create the final object...
    canicula_before <- tibble(start_date = ini_max$julian, min_date = min_b$julian, 
                             end_date = local_max$julian, length = end_date - start_date)
  }else{
    canicula_before <- tibble(start_date = NA, min_date = NA, end_date = NA, length = NA)
  }
  
  return(canicula_before)}

# jmm <- curve_HoltWinters(year_to = 2010)

# jmm %>% ggplot(aes(julian, mov_ts)) + geom_line(colour ='pink') + geom_point() + theme_bw()

# I guess this function should be the final function with all MSD types. 
define_two_MSD <- function(data){
  # data <- jmm
  # First step: define general local maximum and 
  # filter minimums that do not have a maximum next or before.
  
  #Note: I think it's important compute time series maximum and include if that is possible.
  
  # Compute 
  max <- filter(data, type == 2)
  
  # For Now we don't use this information on next step. 
  # max_G <- filter(data, row_number() == which.max(data$mov_ts))
  
  # This line filter minimums local 
  # when doesn't have local max before or next.  
  min <- filter(data, type == 1) %>%
    mutate(testing = case_when(
      julian < max$julian[1] ~ 1,
      julian > last(max$julian) ~ 2,
      TRUE ~ 0)) %>%
    filter(testing ==  0)
  
  #  Step 2. Compute the max of local_max.
  GL_max <- max %>%
    arrange(desc(mov_ts)) %>%
    slice(1)
  
  if(nrow(GL_max) == 1){
    # Step 3. Here we will do step 3. The idea it's run after_canicula function. 
    canicula_A <- canicula_after(local_max = GL_max, mins = min, maxs = max)
    # Step 4. Here we will do step 4. The idea it's run before_canicula function.  
    canicula_B <- canicula_before(local_max = GL_max, mins = min, maxs = max)
    
    # Step_5. Here we join two MSD. 
    MSD_two_sides <- bind_rows(canicula_A, canicula_B) %>%
      mutate(type = c('A', 'B')) %>%
      dplyr::select(type, start_date, min_date, end_date, length)  
  }else{
    MSD_two_sides <- tibble(type = 'N', start_date = NA, min_date = min$julian, end_date = NA, length = NA) %>% 
      bind_rows(tibble(type = 'N', start_date = NA, min_date = min$julian, end_date = NA, length = NA), .)
  }
  
  
return(MSD_two_sides)}


# here we subtract the maximum - minimum of the MSD and then we averaged the differences.
dif_mean <- function(f){
  a <- filter(f, dates == 'start_date')$mov_ts
  b <- filter(f, dates == 'min_date')$mov_ts
  c <- filter(f, dates == 'end_date')$mov_ts
  
  # mean_dif <- ((a-b) + (c-b) + abs(a-c))/3
  median_dif <- median(c((a-b), (c-b), abs(a-c)))
  # return(mean_dif)
  return(median_dif)}

# This function identify MSD between MSD before and after. 
One_MSD <- function(MSD1, data){
  
  # r <- 6
  # data <- MSD_proof  %>% filter(year_to %in% c(1988,1992,1993,2002,2007,2008)) %>%
    # filter(row_number() == r) %>% dplyr::select(data) %>% unnest()
  # MSD1 <- MSD_proof  %>% filter(year_to %in% c(1988,1992,1993,2002,2007,2008)) %>%
  #   filter(row_number() == r) %>% dplyr::select(Two_MSD) %>% unnest()
  
  # data1 %>% ggplot(aes(julian, mov_ts)) + geom_line(colour ='pink') + geom_point() + 
  #   theme_bw()+ geom_vline(xintercept = as.numeric(MSD1[1, 2:4]), col = 'red') + 
  #   geom_vline(xintercept = as.numeric(MSD1[2, 2:4]), col = 'blue') 
  
  MSD <- MSD1 %>%
    dplyr::select(-length) %>% 
    gather(dates, julian, -type) %>% 
    mutate(mov_ts = purrr::map(.x = julian, .f = function(.x){data %>% filter(julian == .x) %>% .$mov_ts}) ) %>% 
    unnest(mov_ts)  %>% 
    nest(-type) %>% 
    mutate(dif = purrr::map(.x = data, .f = dif_mean)) %>% 
    unnest(dif) %>% 
    # arrange(desc(dif)) %>% 
    arrange(dif) %>% 
    filter(row_number() == 1)
  
  return(MSD)}


# This function organize the MSD, if we don't identify MSD put type = N, 
# in the case we own two MSD the function selects one. 
MSD_correction <- function(MSD_C){
  
  # Identify which it's MSD when we have two (what are the cases?)
  MSD_C1 <- MSD_C %>% 
    mutate(proof = purrr::map(.x = Two_MSD, .f = function(.x){.x %>% na.omit() %>% nrow()})) %>% 
    unnest(proof) %>% 
    filter(proof == 2) %>% 
    mutate(Two_MSD = purrr::map2(.x = Two_MSD, .y = data,.f = One_MSD)) %>% 
    dplyr::select(station_N, Lon, Lat,  year, Two_MSD) %>% 
    unnest %>% 
    dplyr::select(station_N, Lon, Lat,  year, type)
  
  
  # Extract the MSD selected. 
  MSD_C1 <- MSD_C %>% 
    dplyr::select(station_N, Lon, Lat,  year, Two_MSD) %>%
    unnest %>% 
    nest(-station_N, -Lon, -Lat,  -year, -type) %>% 
    inner_join(MSD_C1, .) %>% 
    unnest
  
  
  # Change type when we don't identify MSD. 
  No_MSD <- MSD_C %>%
    mutate(proof = purrr::map(.x = Two_MSD, .f = function(.x){.x %>% na.omit() %>% nrow()})) %>%
    unnest(proof)  %>%
    filter(proof == 0) %>% 
    dplyr::select(station_N, Lon, Lat,  year, Two_MSD) %>% 
    unnest %>% 
    mutate(type = 'N') %>% 
    unique()
  
  # Join all MSD. 
  MSD <- MSD_C %>%
    mutate(proof = purrr::map(.x = Two_MSD, .f = function(.x){.x %>% na.omit() %>% nrow()})) %>%
    unnest(proof)  %>%
    filter(proof == 1) %>% 
    dplyr::select(station_N, Lon, Lat,  year, Two_MSD) %>%
    unnest %>% 
    na.omit() %>% 
    bind_rows(MSD_C1, No_MSD, .) %>% 
    arrange(year)
  
  return(MSD)}

# This function return MSD object. 
run_for_each_station <- function(dr){

  # dr <- station
  
   # for(i in 1:36){
   #   print(paste0('number_', i))
   # 
   #   proof <- dr %>%
   #     nest(-station_N, -Lon, -Lat, -year) %>%
   #     mutate(data = purrr::map(.x = data, .f = curve_HoltWinters))  %>%
   #     filter(row_number() == i) %>% dplyr::select(data) %>% unnest
   # 
   #   define_two_MSD(proof)
   # }
   # 
   # proof <- dr %>%
   #   nest(-station_N, -Lon, -Lat, -year) %>%
   #   mutate(data = purrr::map(.x = data, .f = curve_HoltWinters))  %>%
   #   filter(row_number() == 8) %>% dplyr::select(data) %>% unnest
   # 
   # ggplot(proof, aes(julian, mov_ts, colour = as.factor(type))) + geom_point() + theme_bw()
   # 
   # define_two_MSD(proof)

  
  
  MSD_proof <- dr %>% 
    nest(-station_N, -Lon, -Lat, -year) %>% 
    mutate(data = purrr::map(.x = data, .f = curve_HoltWinters), 
           Two_MSD = purrr::map(.x = data, .f = define_two_MSD))
  
  
  MSD_proof <- MSD_correction(MSD_proof)
  return(MSD_proof)}


# jmm %>% ggplot(aes(julian, mov_ts)) + geom_line(colour ='pink') + geom_point() + theme_bw()+
#   geom_vline(xintercept = as.numeric(canicula_B[1,-4]), col = 'red')

# jmm <- curve_HoltWinters(year_to = 2010)
# define_two_MSD(jmm)



##################################################
# Pruebas para correr todas las estaciones...

MSD_test <- new_JointCS %>% 
  dplyr::select(-NA_percent, -data_filling) %>% 
  mutate(id = 1:nrow(.)) %>% 
  unnest %>% 
  dplyr::select(-prec_R, -mov_R) %>% 
  nest(-id)



# Aquí se corre la canicula por estaciones. 
test<- MSD_test %>%  # filter(row_number() == 6) %>%
  mutate(MSD = purrr::map(.x = data, .f = run_for_each_station)) %>% 
  dplyr::select(MSD) %>% 
  unnest 


test1 <- test %>% 
  rename(x = 'Lon', y = 'Lat') %>% 
  dplyr::select( station_N, x,  y , length) %>% 
  group_by(station_N,  x,  y) %>% 
  summarise(NA_p = (sum(is.na(length))/36)*100, count_NA = sum(is.na(length))) %>% 
  mutate(na_cat = case_when(
    NA_p < 10 ~ '[0-10) %',
    NA_p < 20 & NA_p >= 10 ~ '[10-20) %', 
    NA_p < 30 & NA_p >= 20 ~ '[20-30) %', 
    NA_p >= 30 ~ '30% +'))

p <- ggplot(test1)  + 
  geom_point(aes(x = x, y =  y, colour = NA_p)) +
  scale_colour_viridis(na.value="white",  direction = -1) + 
  geom_sf(data = shp, fill = NA, color = gray(.5)) +
  geom_sf(data = dry_C, fill = NA, color = gray(.1)) + 
  theme_bw() + 
  labs(x = 'Longitud', y = 'Latitud', colour = '% NA')
  

q <- ggplot(test1)  + 
  geom_point(aes(x = x, y =  y, colour = na_cat)) +
  scale_colour_viridis(na.value="white",  direction = -1, discrete=TRUE) + 
  geom_sf(data = shp, fill = NA, color = gray(.5)) +
  geom_sf(data = dry_C, fill = NA, color = gray(.1)) + 
  theme_bw() + 
  labs(x = 'Longitud', y = 'Latitud', colour = '% NA')


out_folder <- 'D:/OneDrive - CGIAR/Desktop/USAID-Regional/USAID-REGIONAL/Drought/Station_R/'

gridExtra::grid.arrange(p, q, ncol = 2)

write.csv(test, glue::glue('{out_folder}HW_TM_MSD.csv'))


# data <- MSD_test %>% filter(row_number() == 5) %>% dplyr::select(data) %>% 
#   unnest

# test %>%
#   dplyr::select(station_N, length) %>%
#   group_by(station_N) %>% 
#   skimr::skim()



# Gift por estaciones. 

data <- MSD_test %>% 
  filter(id == 1) %>% 
  dplyr::select(data) %>% 
  unnest %>% 
  filter(year == 1982)

# curve_HoltWinters(data)


data_curve_by_year <- function(data){
  # Testing other methodologies...
  year_pixel <-  data %>% 
    dplyr::select(day, month , julian, prec_C)
  # mutate(mov =  movavg(x = prec_C, n = 31, type = 't'))
  
  rain <- HoltWinters(year_pixel$prec_C, beta=FALSE, gamma=FALSE)$fitted[,1] 
  # rainseriesforecasts <- HoltWinters(rainfall, beta=FALSE, gamma=FALSE)
  
  data_base <- year_pixel %>%
    mutate(mov =  movavg(x = prec_C, n = 31, type = 't'), ts = c(0,rain), 
           mov_ts = movavg(x = ts, n = 31, type = 't')) %>% 
    dplyr::select(-prec_C)  %>% 
    filter(month %in% 5:8) %>%
    dplyr::select(julian, month, mov_ts) %>% 
    mutate(type = case_when(
      lag(mov_ts) - mov_ts > 0  & lead(mov_ts) - mov_ts > 0  ~ 1, 
      lead(mov_ts) - mov_ts < 0 & lag(mov_ts) - mov_ts < 0  ~ 2,
      TRUE ~ 0  ) ) 
return(data_base)}

 

prueba <- MSD_test %>% 
  unnest() %>% 
  nest(-id, -station_N, -Lon, -Lat, -year) %>% 
mutate(data_curve = purrr::map(.x = data, .f = data_curve_by_year))


ajam <- test %>% 
  mutate(year_to = year) %>% 
  nest(-Lon, -Lat, -year) %>% 
  rename(MSD = 'data')



prueba <- prueba %>%
  dplyr::select(-data) %>% 
  # filter(id == 1, year == 1982) %>%
  inner_join(ajam, .)

# Aqui se genera la forma del grafico...
 Individual_graph <- function(Data_index, Data){
   # Data_index<- MSD_Local$MSD[[1]]
   # Data <- MSD_Local$data_curve[[1]]

   # Data_index <- Data_index %>%
   #   na_if(-999)

   graph <-  ggplot(Data) +
     geom_point(aes(julian, mov_ts, colour = as.factor(type))) + 
     geom_line(aes(julian, mov_ts)) +
     theme_bw() +
     labs(x = 'Julian day', y = "HoltWinters - Triangular Average (mm)", colour = '') +
     geom_vline( xintercept = c(Data_index$start_date, Data_index$end_date), 
                 linetype=4, colour = 'blue') +
     geom_vline(xintercept = Data_index$min_date,  linetype=4, colour = 'skyblue') +
     labs(title = glue::glue('Station {Data_index$station_N} --- year {Data_index$year_to}')) +
     scale_colour_manual(values = c('gray', 'blue', 'red'), 
                         breaks = c("0", "1", "2"), 
                         labels = c('Normal', 'Local_Min', 'Local_Max')) # +
     # theme(legend.position = 'top')
   print(graph)
 }

 # Aqui se genera el gift...
 by_id <- function(MSD_Local, id_pixel){
   # MSD_Local <- prueba %>%  filter(id == 2) %>% dplyr::select(data) %>% unnest
   
   # MSD_Local <- unnest(MSD_Local)
   

    img <- image_graph(1200, 680, res = 96)
    out <- purrr::map2(.x = MSD_Local$MSD,
                       .y = MSD_Local$data_curve,
                       .f = Individual_graph)
    dev.off()

    animation <- magick::image_animate(img, fps = 0.5)
    # print(animation)
    image_write(animation, glue::glue("Drought/Station_R/station/id_{id_pixel}.gif"))
 }
 
 ## Listo probando. 
 prueba <- prueba %>% 
   # filter(id == 1)  %>% 
   dplyr::select(id, MSD, data_curve) %>% 
   nest(-id )
 
tictoc::tic()
 test1 <-  prueba %>%
   # filter(row_number() == 1) %>% 
   mutate(ajam = purrr::map2(.x = data, .y = id, .f = by_id))
tictoc::toc() # 1.37 h
 # by_id(MSD_Local = test1$data[[1]], id_pixel = test1$id)
 
 
 
 
 
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Proof a new metogology spliting data time serie  ---- using 

# .------..------..------..------..------.
# |P.--. ||R.--. ||O.--. ||O.--. ||F.--. |
# | :/\: || :(): || :/\: || :/\: || :(): |
# | (__) || ()() || :\/: || :\/: || ()() |
# | '--'P|| '--'R|| '--'O|| '--'O|| '--'F|
# `------'`------'`------'`------'`------'


##############################################################################
# Test with acum data...


for(i in 1982:2017){
  up <- station %>%  filter(year == i)  %>% 
    mutate(roll_acum = zoo::rollsum(x = prec_C, 31, align = "right", fill = NA))
  
  a <- HoltWinters(up$prec_C, beta=FALSE, gamma=FALSE)$fitted[,1]
  
  up2 <- station %>%  filter(year == i) %>% 
    mutate(roll_A = zoo::rollsum(x = prec_C, 31, align = "right", fill = NA),
      HW = c(0,HoltWinters(up$prec_C, beta=FALSE, gamma=FALSE)$fitted[,1]), 
           roll_acum = zoo::rollsum(x = HW, 31, align = "right", fill = NA), 
           mov_HW = movavg(x = HW , n = 31, type = 't')) %>% 
    filter(month %in% 5:8)
  
  a <- up2 %>%  ggplot() +  geom_line(aes(x = julian, y = roll_acum)) + 
    geom_point(aes(x = julian, y = roll_acum)) +  
    geom_line(aes(x = julian, y = roll_A), colour = 'red') + 
    geom_point(aes(x = julian, y = roll_A), colour = 'red')+ theme_bw() +
    labs(title = 'Cumulative prec: red (non-transformation) - black (HoltWinters)', x = 'Julian day')
  
  b <- up2 %>% ggplot() + geom_line(aes(x = julian, y = mov_HW)) +
    geom_line(aes(x = julian, y = mov), colour = 'red') + 
    geom_point(aes(x = julian, y = mov), colour = 'red') +
    geom_point(aes(x = julian, y = mov_HW)) + theme_bw() + 
    labs(title = 'Triangular averange: red (non-transformation) - black (HoltWinters)', x = 'Julian day')
  
  c <- gridExtra::grid.arrange(a, b, ncol = 2)
  ggsave(c, filename = glue::glue('Drought/Station_R/acum_mov/y{i}.png'), width = 32, height = 16, units = "cm")
}








# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Proof a new metogology spliting data time serie  ---- using 

# .------..------..------..------..------.
# |P.--. ||R.--. ||O.--. ||O.--. ||F.--. |
# | :/\: || :(): || :/\: || :/\: || :(): |
# | (__) || ()() || :\/: || :\/: || ()() |
# | '--'P|| '--'R|| '--'O|| '--'O|| '--'F|
# `------'`------'`------'`------'`------'


##############################################################################
# Test with orginal data triangular mean.


# dr <- station

# This function (curve HoltWinters) summarise the time 
# filter by year and compute HoltWinters - triangular mean. 
filter_data <- function(dr){
  
  # Testing other methodologies...
  year_pixel <-  dr %>% 
    dplyr::select(day, month , julian, mov) %>% 
    filter(month %in% 5:8) %>% 
    dplyr::select(julian, month, mov) %>% 
    rename(mov_ts = 'mov') %>% 
    mutate(type = case_when(
      lag(mov_ts) - mov_ts > 0  & lead(mov_ts) - mov_ts > 0  ~ 1, 
      lead(mov_ts) - mov_ts < 0 & lag(mov_ts) - mov_ts < 0  ~ 2,
      TRUE ~ 0  ) )  
  
  return(year_pixel)}


run_for_each_OS <- function(dr){
  
  # dr <- station
  
  # for(i in 1:36){
  #   print(paste0('number_', i))
  # 
  #   proof <- dr %>%
  #     nest(-station_N, -Lon, -Lat, -year) %>%
  #     mutate(data = purrr::map(.x = data, .f = curve_HoltWinters))  %>%
  #     filter(row_number() == i) %>% dplyr::select(data) %>% unnest
  # 
  #   define_two_MSD(proof)
  # }
  # 
  # proof <- dr %>%
  #   nest(-station_N, -Lon, -Lat, -year) %>%
  #   mutate(data = purrr::map(.x = data, .f = curve_HoltWinters))  %>%
  #   filter(row_number() == 8) %>% dplyr::select(data) %>% unnest
  # 
  # ggplot(proof, aes(julian, mov_ts, colour = as.factor(type))) + geom_point() + theme_bw()
  # 
  # define_two_MSD(proof)
  
  
  
  MSD_proof <- dr %>% 
    nest(-station_N, -Lon, -Lat, -year) %>% 
    mutate(data = purrr::map(.x = data, .f = filter_data), 
           Two_MSD = purrr::map(.x = data, .f = define_two_MSD))
  
  
  MSD_proof <- MSD_correction(MSD_proof)
  return(MSD_proof)}






MSD_tO <- new_JointCS %>% 
  dplyr::select(-NA_percent, -data_filling) %>% 
  mutate(id = 1:nrow(.)) %>% 
  unnest %>% 
  dplyr::select(-prec_R, -mov_R) %>% 
  nest(-id)



# Aquí se corre la canicula por estaciones. 
test_o <- MSD_tO %>%  # filter(row_number() == 6) %>%
  mutate(MSD = purrr::map(.x = data, .f = run_for_each_OS)) %>%
  dplyr::select(MSD) %>%
  unnest


test1_O <- test_o %>%
  rename(x = 'Lon', y = 'Lat') %>%
  dplyr::select( station_N, x,  y , length) %>%
  group_by(station_N,  x,  y) %>%
  summarise(NA_p = (sum(is.na(length))/36)*100, count_NA = sum(is.na(length))) %>%
  mutate(na_cat = case_when(
    NA_p < 10 ~ '[0-10) %',
    NA_p < 15 & NA_p >= 10 ~ '[10-15) %',
    NA_p < 20 & NA_p >= 15 ~ '[15-20) %',
    NA_p < 30 & NA_p >= 20 ~ '[20-30) %',
    NA_p >= 30 ~ '30% +'))


p <- ggplot(test1_O)  + 
  geom_point(aes(x = x, y =  y, colour = NA_p)) +
  scale_colour_viridis(na.value="white",  direction = -1) + 
  geom_sf(data = shp, fill = NA, color = gray(.5)) +
  geom_sf(data = dry_C, fill = NA, color = gray(.1)) + 
  theme_bw() + 
  labs(x = 'Longitud', y = 'Latitud', colour = '% NA')


q <- ggplot(test1_O)  + 
  geom_point(aes(x = x, y =  y, colour = na_cat)) +
  scale_colour_viridis(na.value="white",  direction = -1, discrete=TRUE) + 
  geom_sf(data = shp, fill = NA, color = gray(.5)) +
  geom_sf(data = dry_C, fill = NA, color = gray(.1)) + 
  theme_bw() + 
  labs(x = 'Longitud', y = 'Latitud', colour = '% NA')


out_folder <- 'D:/OneDrive - CGIAR/Desktop/USAID-Regional/USAID-REGIONAL/Drought/Station_R/original_data/'

gridExtra::grid.arrange(p, q, ncol = 2)

write.csv(test_o, glue::glue('{out_folder}O_TM_MSD_Median.csv'))



# =-=-=-=-=-=-=

data_Oy <- function(data){

  data_base <- data %>%
    filter(month %in% 5:8) %>%
    rename(mov_ts = mov) %>% 
    dplyr::select(julian, month, mov_ts) %>% 
    mutate(type = case_when(
      lag(mov_ts) - mov_ts > 0  & lead(mov_ts) - mov_ts > 0  ~ 1, 
      lead(mov_ts) - mov_ts < 0 & lag(mov_ts) - mov_ts < 0  ~ 2,
      TRUE ~ 0  ) ) 
  return(data_base)}

# MSD_tO

prueba_o <- MSD_tO %>% 
  unnest() %>% 
  nest(-id, -station_N, -Lon, -Lat, -year) %>% 
  mutate(data_curve = purrr::map(.x = data, .f = data_Oy))


ajam_o <- test_o %>% 
  mutate(year_to = year) %>% 
  nest(-Lon, -Lat, -year) %>% 
  rename(MSD = 'data')



prueba_o <- prueba_o %>%
  dplyr::select(-data) %>% 
  # filter(id == 1, year == 1982) %>%
  inner_join(ajam_o, .)


# Data_index<- MSD_Local$MSD[[1]]
# Data <- MSD_Local$data_curve[[1]]


## Listo probando. 
prueba_o <- prueba_o %>% 
  # filter(id == 1)  %>% 
  dplyr::select(id, MSD, data_curve) %>% 
  nest(-id )



# Aqui se genera la forma del grafico...
Individual_graph_o <- function(Data_index, Data){
  # Data_index<- MSD_Local$MSD[[1]]
  # Data <- MSD_Local$data_curve[[1]]
  
  # Data_index <- Data_index %>%
  #   na_if(-999)
  
  graph <-  ggplot(Data) +
    geom_point(aes(julian, mov_ts, colour = as.factor(type))) + 
    geom_line(aes(julian, mov_ts)) +
    theme_bw() +
    labs(x = 'Julian day', y = "Triangular Average (mm)", colour = '') +
    geom_vline( xintercept = c(Data_index$start_date, Data_index$end_date), 
                linetype=4, colour = 'blue') +
    geom_vline(xintercept = Data_index$min_date,  linetype=4, colour = 'skyblue') +
    labs(title = glue::glue('Station {Data_index$station_N} --- year {Data_index$year_to}')) +
    scale_colour_manual(values = c('gray', 'blue', 'red'), 
                        breaks = c("0", "1", "2"), 
                        labels = c('Normal', 'Local_Min', 'Local_Max')) # +
  # theme(legend.position = 'top')
  print(graph)
}

# Aqui se genera el gift...
by_id_o <- function(MSD_Local, id_pixel){
  # MSD_Local <- prueba_o %>%  filter(id == 2) %>% dplyr::select(data) %>% unnest
  
  # MSD_Local <- unnest(MSD_Local)
  
  
  img <- image_graph(1200, 680, res = 96)
  out <- purrr::map2(.x = MSD_Local$MSD,
                     .y = MSD_Local$data_curve,
                     .f = Individual_graph_o)
  dev.off()
  
  animation <- magick::image_animate(img, fps = 0.5)
  # print(animation)
  image_write(animation, glue::glue("Drought/Station_R/original_data/id_{id_pixel}.gif"))
}







tictoc::tic()
test1_o <-  prueba_o %>%
  filter(row_number() > 2) %>% 
  mutate(ajam = purrr::map2(.x = data, .y = id, .f = by_id_o))
tictoc::toc() # 1.37 h
# by_id(MSD_Local = test1$data[[1]], id_pixel = test1$id)







# CPT_file_s <- function(data, var){
#   data <- proof
#   var <- 'Intensity'
#   
#   CPT_data <- data %>% 
#     dplyr::select(-id, -data) %>% 
#     unnest %>% 
#     dplyr::select(year, id, !!var) %>% 
#     mutate_if(is.numeric, list(~round(., 1))) %>%
#     spread(key = id, value = !!var)  
#   
#   Lat_Long  <- data %>% 
#     dplyr::select(-id, -data) %>% 
#     unnest %>% 
#     dplyr::select(x, y) %>% 
#     unique %>% 
#     t() 
#   
#   colnames(Lat_Long) <- paste0(1:10)
#   rownames(Lat_Long) <- NULL
#   
#   Lat_Long <- add_column(as_tibble(Lat_Long), year = c('cpt:X', 'cpt:Y'), .before = 1)  
#   
#   names(Lat_Long) <- c('', paste0('V',1:10))
#   names(CPT_data) <- c('', paste0('V',1:10))
#   
#   # =-=-=-=-=-=-=-=-=-=-=-=
#   CPT_data <- CPT_data %>% 
#     mutate_if(is.factor, as.character) %>% 
#     mutate_if(is.character, as.numeric)  %>%
#     rbind(Lat_Long, .) 
#   
#   file <- paste0('D:/OneDrive - CGIAR/Desktop/USAID-Regional/USAID-REGIONAL/MSD_Index/CPT_files/Station_', var, '.txt')
#   
#   sink(file = file)
#   cat('xmlns:cpt=http://iri.columbia.edu/CPT/v10/', sep = '\n')
#   cat('cpt:nfield=1', sep = '\n')
#   cat(glue("cpt:field=days, cpt:nrow=36, cpt:ncol=10, cpt:col=station, cpt:row=T, cpt:units=julian;cpt:missing=-999"), sep = '\n')
#   cat(write.table(CPT_data, sep = '\t', col.names = TRUE, row.names = FALSE, na = "", quote = FALSE))
#   sink()
# }  

# proof
# Intensity - Length - Magnitude
# CPT_file_s(data = proof, var = 'Intensity' )



# +-+ +-+ +-+ +-+ +-+ +-+   +-+   +-+ +-+ +-+ +-+
# |C| |h| |i| |r| |p| |s|   |-|   |T| |e| |s| |t|
# +-+ +-+ +-+ +-+ +-+ +-+   +-+   +-+ +-+ +-+ +-+

out_folder <- 'D:/OneDrive - CGIAR/Desktop/USAID-Regional/USAID-REGIONAL/Drought/Station_R/Chirps_results/original/png_dif_1/'

# chirps_p <- id_year_prec %>% 
#   unnest %>% 
#   mutate(station_N = id) %>% 
#   rename(day = 'date',  Lon = 'x', Lat = 'y') %>% 
#   nest(-id)



# read_chirps <- 
data_C <- read_csv("Drought/Station_R/Chirps_results/original/data.csv")

chirps_p <- data_C %>% 
  dplyr::select(-X1) %>% 
  nest(-id)


# station <- chirps_p %>% filter(row_number() == 1) %>% dplyr::select(data) %>% unnest

# Aquí se corre la canicula por estaciones. 
tictoc::tic()
Chirps_MSD <- chirps_p %>%  # filter(row_number() == 6) %>%
  mutate(MSD = purrr::map(.x = data, .f = run_for_each_OS)) %>%
  dplyr::select(MSD) %>%
  unnest
tictoc::toc() # 9.81




test1_Chirps <- Chirps_MSD %>%
  rename(x = 'Lon', y = 'Lat') %>%
  dplyr::select( station_N, x,  y , length) %>%
  group_by(station_N,  x,  y) %>%
  summarise(NA_p = (sum(is.na(length))/37)*100, count_NA = sum(is.na(length))) %>%
  mutate(na_cat = case_when(
    NA_p < 10 ~ '[0-10) %',
    NA_p < 15 & NA_p >= 10 ~ '[10-15) %',
    NA_p < 20 & NA_p >= 15 ~ '[15-20) %',
    NA_p < 30 & NA_p >= 20 ~ '[20-30) %',
    NA_p >= 30 ~ '30% +'))


p <- ggplot(test1_Chirps)  + 
  geom_tile(aes(x = x, y =  y, fill = NA_p)) +
  scale_fill_viridis(na.value="white",  direction = -1) + 
  geom_sf(data = shp, fill = NA, color = gray(.5)) +
  geom_sf(data = dry_C, fill = NA, color = gray(.1)) + 
  theme_bw() + 
  labs(x = 'Longitud', y = 'Latitud', colour = '% NA')


q <- ggplot(test1_Chirps)  + 
  geom_tile(aes(x = x, y =  y, fill = na_cat)) +
  scale_fill_viridis(na.value="white",  direction = -1, discrete=TRUE) + 
  geom_sf(data = shp, fill = NA, color = gray(.5)) +
  geom_sf(data = dry_C, fill = NA, color = gray(.1)) + 
  theme_bw() + 
  labs(x = 'Longitud', y = 'Latitud', colour = '% NA')




gridExtra::grid.arrange(p, q, ncol = 2)
write.csv(Chirps_MSD, glue::glue('{out_folder}Chirps_MSD_Median_min.csv'))



# chirps_p %>% unnest() %>% 
#   write.csv(glue::glue('{out_folder}data.csv'))



# =-=-=-=-=-=-=-=-=-=-=-=

prueba_chirps <- chirps_p %>% 
  unnest() %>% 
  nest(-station_N, -Lon, -Lat, -year) %>% 
  mutate(data_curve = purrr::map(.x = data, .f = data_Oy))



Chirps_o <- Chirps_MSD %>% 
  mutate(year_to = year) %>% 
  nest(-Lon, -Lat, -year) %>% 
  rename(MSD = 'data')


prueba_chirps <- prueba_chirps %>%
  dplyr::select(-data) %>% 
  # filter(id == 1, year == 1982) %>%
  inner_join(Chirps_o, .)




prueba_chirps <- prueba_chirps %>% 
  # filter(id == 1)  %>% 
  mutate(id = station_N) %>% 
  dplyr::select(id, MSD, data_curve) %>% 
  nest(-id )


# Don't run this.
by_id_o <- function(MSD_Local, id_pixel){
  # MSD_Local <- prueba_o %>%  filter(id == 2) %>% dplyr::select(data) %>% unnest
  
  # MSD_Local <- unnest(MSD_Local)
  
  
  img <- image_graph(1200, 680, res = 96)
  out <- purrr::map2(.x = MSD_Local$MSD,
                     .y = MSD_Local$data_curve,
                     .f = Individual_graph_o)
  dev.off()
  
  animation <- magick::image_animate(img, fps = 0.5)
  # print(animation)
  image_write(animation, glue::glue("Drought/Station_R/Chirps_results/original/id_{id_pixel}.gif"))
}


tictoc::tic()
test1_chirps <-  prueba_chirps %>%
  # filter(row_number() > 2) %>% 
  mutate(ajam = purrr::map2(.x = data, .y = id, .f = by_id_o))
tictoc::toc() # Aun no tenenmos estimado de tiempo total por id. 




#################################
# Individual graphs...

# Data_index <- prueba_chirps %>% unnest %>% filter(row_number() == 1) %>% dplyr::select(MSD) %>% unnest
# Data <- prueba_chirps %>% unnest %>% filter(row_number() == 1) %>% dplyr::select(data_curve) %>% unnest
Igraph_save <- function(Data_index, Data){
  x <- Data %>% slice(1, 30) %>% .$julian
  
  
  p <- ggplot(Data) +
    geom_point(aes(julian, mov_ts, colour = as.factor(type))) + 
    geom_line(aes(julian, mov_ts)) +
    annotate("rect", xmin=x[1], xmax=x[2], ymin=-Inf, ymax=Inf, alpha = .1)  +
    labs(x = 'Julian day', y = "Triangular Average (mm)", colour = '') +
    geom_vline( xintercept = c(Data_index$start_date, Data_index$end_date), 
                linetype=4, colour = 'blue') +
    geom_vline(xintercept = Data_index$min_date,  linetype=4, colour = 'turquoise2') +
    labs(title = glue::glue('Id {Data_index$station_N} --- year {Data_index$year_to}')) +
    scale_colour_manual(values = c('gray', 'blue', 'red'), 
                        breaks = c("0", "1", "2"), 
                        labels = c('Normal', 'Local_Min', 'Local_Max')) +
    theme_bw() 
  
  
  ggsave(glue::glue("Drought/Station_R/Chirps_results/original/png_dif_1/id_{Data_index$station_N}_{Data_index$year_to}.png"), 
         width = 8, height = 4)
  
}


tictoc::tic()
prueba_chirps %>% 
  filter(id < 11) %>% 
  unnest %>% 
  mutate(graphs = purrr::map2(.x = MSD, .y = data_curve, .f = Igraph_save))
tictoc::toc()








# Chirps_MSD1 <-  Chirps_MSD








# Here we will do step 3. 
# This function find the canicula when the general local maximo is canicula's start.
canicula_after <- function(local_max, mins, maxs){
  # local_max <- GL_max; mins <- min; maxs <- max
  
  # Step 3. Find the minimum ... possible.
  
  # This computes all possible local min after general local max. 
  min_after <- mins %>% 
    filter(julian > local_max$julian)
  
  # This line join max with mins in one tibble. 
  base_filter_Ind <- bind_rows(min_after, maxs) %>%
    dplyr::select(-testing) %>%
    arrange(julian) %>% # Here i change this. 
    mutate(dif = julian - local_max$julian) %>% 
    filter(dif > 5) %>% 
    dplyr::select(-dif) %>%
    bind_rows(local_max, .)
  
  # This line calculate the number of rows... that the dataset has.
  nrow_base <- nrow(min_after)
  
  # making the arrangement or trap to add mutates.
  min_after <- base_filter_Ind %>%
    filter(row_number() > 1) 
  

  
  # add mutates.
  j <- 1 
  for(i in 1:nrow_base){
    # print(i) ; print(j)
    
    # cat(glue::glue('(i = {i} ; j = {j}) '))
    varname <- glue::glue('sub{j}L')
    varname1 <- glue::glue('jul{j}L')
    
    min_after <- min_after %>% 
      mutate(!!varname := mov_ts - lead(mov_ts, n = j), 
             !!varname1 := lead(julian, n = j) - julian) 
    
    j <-  (2*i) + 1 }
  
  # Here we extract the second local max. 
  
  jul_sub <- min_after %>%
    filter(type == 1)  %>% 
    select(julian, month, mov_ts, type, contains('jul'))  %>% 
    gather(type, jul_sub, -julian, -month, -mov_ts, -type) %>% 
    mutate(type = str_replace(type, 'jul', 'sub'))
  
  min_after <- min_after %>%
    filter(type == 1)  %>% 
    select(julian, month, mov_ts, type, contains('sub')) %>% 
    gather(type, value, -julian, -month, -mov_ts, -type) %>% 
    right_join(. , jul_sub) %>% 
    filter(jul_sub > 5) %>% 
    arrange(value)  %>% 
    filter(value < 0) %>% # Here i change this line. 
    slice(1) %>% 
    dplyr::select(-contains('jul_'))
  
  
  # If we find min... do second max and canicula. 
  # In other case only create canicula with data NA.
  if(nrow(min_after) != 0){
    # Ok this line select the second max.     
    second_max <- maxs %>% 
      filter(julian > min_after$julian) %>% 
      mutate(num = paste0( 'sub', seq(1, (2*(nrow(.)-1) + 1), by = 2), 'L')) %>% 
      filter(num == min_after$type)
    
    # (2*(1-1) + 1) ; (2*(2-1) + 1) ; (2*(3-1) + 1) ; (2*(4-1) + 1)
    # Here we create the final object...
    canicula_after <- tibble(start_date = local_max$julian, min_date = min_after$julian, 
                             end_date = second_max$julian, length = end_date - start_date)
  }else{
    canicula_after <- tibble(start_date = NA, min_date = NA, end_date = NA, length = NA)
  }
  
  return(canicula_after)}

# Step 4. Here we will do step 4. The idea it's run before_canicula function.  
canicula_before <- function(local_max, mins, maxs){
  # local_max <- GL_max; mins <- min; maxs <- max
  
  
  # Step 4. Find the minimum ... possible. 
  min_before <- mins %>% 
    filter(julian < local_max$julian)
  
  # This line join max with mins in one tibble.  
  min_b <- bind_rows(min_before, maxs) %>%
    dplyr::select(-testing) %>%
    arrange(julian) %>% # Here i change this. 
    mutate(dif = local_max$julian - julian)  %>% 
    filter(dif > 5)  %>% 
    dplyr::select(-dif) %>%
    bind_rows(., local_max)
  
  
  # This line calculate the number of rows... that the dataset has.
  nrow_base <- nrow(min_before)
  
  # making the arrangement or trap to add mutates.
  # =-=-=-=-=-=
  j <- 1 
  for(i in 1:nrow_base){
    # print(i) ; print(j)
    
    # cat(glue::glue('(i = {i} ; j = {j}) '))
    varname <- glue::glue('sub{j}L')
    varname1 <- glue::glue('jul{j}L')
    
    min_b <- min_b %>% 
      mutate(!!varname := mov_ts - lag(mov_ts, n = j), 
             !!varname1 := julian - lag(julian, n = j)) 
    
    j <-  (2*i) + 1 }
  
  
  jul_sub <- min_b %>%
    filter(type == 1)  %>% 
    select(julian, month, mov_ts, type, contains('jul'))  %>% 
    gather(type, jul_sub, -julian, -month, -mov_ts, -type) %>% 
    mutate(type = str_replace(type, 'jul', 'sub'))
  
  min_b <- min_b %>% 
    filter(type == 1)  %>% 
    select(julian, month, mov_ts, type, contains('sub')) %>% 
    gather(type, value, -julian, -month, -mov_ts, -type) %>% 
    right_join(. , jul_sub) %>% 
    filter(jul_sub > 5) %>% 
    arrange(value)  %>% 
    filter(value < 0) %>% # Here i change this line. 
    slice(1) %>% 
    dplyr::select(-contains('jul_'))
  
  
  # If we find min... do first max and canicula. 
  # In other case only create canicula with data NA.
  if(nrow(min_b) != 0){
    
    
    # seq(1, (2*(nrow(.)-1) + 1), by = 2)
    
    ini_max <- maxs %>% 
      filter(julian < min_b$julian) %>% 
      # mutate(num = paste0( 'sub',rev(1:nrow(.)), 'L')) %>%
      mutate(num = paste0( 'sub', rev(seq(1, (2*(nrow(.)-1) + 1), by = 2)), 'L')) %>%
      filter(num == min_b$type)
    
    # Here we create the final object...
    canicula_before <- tibble(start_date = ini_max$julian, min_date = min_b$julian, 
                              end_date = local_max$julian, length = end_date - start_date)
  }else{
    canicula_before <- tibble(start_date = NA, min_date = NA, end_date = NA, length = NA)
  }
  
  return(canicula_before)}

# data <- chirps_p %>% 
#   filter(id == 2 ) %>% 
#   unnest() %>% 
#   nest(-station_N, -Lon, -Lat, -year)  %>% 
#   mutate(data = purrr::map(.x = data, .f = filter_data)) %>% 
#   filter(year == 1991) %>% 
#   dplyr::select(data) %>% 
#   unnest()


define_two_MSD <- function(data){
  # data <- jmm
  # First step: define general local maximum and 
  # filter minimums that do not have a maximum next or before.
  
  #Note: I think it's important compute time series maximum and include if that is possible.
  
  # Compute 
  max <- filter(data, type == 2)
  
  # For Now we don't use this information on next step. 
  # max_G <- filter(data, row_number() == which.max(data$mov_ts))
  
  # This line filter minimums local 
  # when doesn't have local max before or next.  
  min <- filter(data, type == 1) %>%
    mutate(testing = case_when(
      julian < max$julian[1] ~ 1,
      julian > last(max$julian) ~ 2,
      TRUE ~ 0)) %>%
    filter(testing ==  0)
  
  #  Step 2. Compute the max of local_max.
  GL_max <- max %>%
    arrange(desc(mov_ts)) %>%
    slice(1)
  
  if(nrow(GL_max) == 1){
    # Step 3. Here we will do step 3. The idea it's run after_canicula function. 
    canicula_A <- canicula_after(local_max = GL_max, mins = min, maxs = max)
    # Step 4. Here we will do step 4. The idea it's run before_canicula function.  
    canicula_B <- canicula_before(local_max = GL_max, mins = min, maxs = max)
    
    # Step_5. Here we join two MSD. 
    MSD_two_sides <- bind_rows(canicula_A, canicula_B) %>%
      mutate(type = c('A', 'B')) %>%
      dplyr::select(type, start_date, min_date, end_date, length)  
  }else{
    MSD_two_sides <- tibble(type = 'N', start_date = NA, min_date = min$julian, end_date = NA, length = NA) %>% 
      bind_rows(tibble(type = 'N', start_date = NA, min_date = min$julian, end_date = NA, length = NA), .)
  }
  
  
  return(MSD_two_sides)}


# here we subtract the maximum - minimum of the MSD and then we averaged the differences.
dif_mean <- function(f){
  a <- filter(f, dates == 'start_date')$mov_ts
  b <- filter(f, dates == 'min_date')$mov_ts
  c <- filter(f, dates == 'end_date')$mov_ts
  
  # mean_dif <- ((a-b) + (c-b) + abs(a-c))/3
  median_dif <- median(c((a-b), (c-b), abs(a-c)))
  # return(mean_dif)
  return(median_dif)}

# This function identify MSD between MSD before and after. 
One_MSD <- function(MSD1, data){
  
  # MSD1 <- MSD_two_sides
  
  # r <- 6
  # data <- MSD_proof  %>% filter(year_to %in% c(1988,1992,1993,2002,2007,2008)) %>%
  # filter(row_number() == r) %>% dplyr::select(data) %>% unnest()
  # MSD1 <- MSD_proof  %>% filter(year_to %in% c(1988,1992,1993,2002,2007,2008)) %>%
  #   filter(row_number() == r) %>% dplyr::select(Two_MSD) %>% unnest()
  
  # data1 %>% ggplot(aes(julian, mov_ts)) + geom_line(colour ='pink') + geom_point() + 
  #   theme_bw()+ geom_vline(xintercept = as.numeric(MSD1[1, 2:4]), col = 'red') + 
  #   geom_vline(xintercept = as.numeric(MSD1[2, 2:4]), col = 'blue') 
  
  MSD <-  MSD1 %>%
    dplyr::select(-length) %>% 
    gather(dates, julian, -type) %>% 
    mutate(mov_ts = purrr::map(.x = julian, .f = function(.x){data %>% filter(julian == .x) %>% .$mov_ts}) ) %>% 
    unnest(mov_ts)  %>% 
    nest(-type) %>% 
    mutate(dif = purrr::map(.x = data, .f = dif_mean)) %>% 
    unnest(dif) %>% 
    # arrange(desc(dif)) %>% 
    arrange(dif) 
  
  MSD_F <- MSD %>% 
    filter(dif > 1) %>%
    filter(row_number() == 1)
  
  MSD <- if(nrow(MSD_F) < 1){filter(MSD, row_number() == 1)}else{MSD_F}

  return(MSD)}


# This function organize the MSD, if we don't identify MSD put type = N, 
# in the case we own two MSD the function selects one. 
MSD_correction <- function(MSD_C){
  
  # Identify which it's MSD when we have two (what are the cases?)
  MSD_C1 <- MSD_C %>% 
    mutate(proof = purrr::map(.x = Two_MSD, .f = function(.x){.x %>% na.omit() %>% nrow()})) %>% 
    unnest(proof) %>% 
    filter(proof == 2) %>% 
    mutate(Two_MSD = purrr::map2(.x = Two_MSD, .y = data,.f = One_MSD)) %>% 
    dplyr::select(station_N, Lon, Lat,  year, Two_MSD) %>% 
    unnest %>% 
    dplyr::select(station_N, Lon, Lat,  year, type)
  
  
  # Extract the MSD selected. 
  MSD_C1 <- MSD_C %>% 
    dplyr::select(station_N, Lon, Lat,  year, Two_MSD) %>%
    unnest %>% 
    nest(-station_N, -Lon, -Lat,  -year, -type) %>% 
    inner_join(MSD_C1, .) %>% 
    unnest
  
  
  # Change type when we don't identify MSD. 
  No_MSD <- MSD_C %>%
    mutate(proof = purrr::map(.x = Two_MSD, .f = function(.x){.x %>% na.omit() %>% nrow()})) %>%
    unnest(proof)  %>%
    filter(proof == 0) %>% 
    dplyr::select(station_N, Lon, Lat,  year, Two_MSD) %>% 
    unnest %>% 
    mutate(type = 'N') %>% 
    unique()
  
  # Join all MSD. 
  MSD <- MSD_C %>%
    mutate(proof = purrr::map(.x = Two_MSD, .f = function(.x){.x %>% na.omit() %>% nrow()})) %>%
    unnest(proof)  %>%
    filter(proof == 1) %>% 
    dplyr::select(station_N, Lon, Lat,  year, Two_MSD) %>%
    unnest %>% 
    na.omit() %>% 
    bind_rows(MSD_C1, No_MSD, .) %>% 
    arrange(year)
  
  return(MSD)}



##############################################################################
filter_data <- function(dr){
  
  # Testing other methodologies...
  year_pixel <-  dr %>% 
    dplyr::select(day, month , julian, mov) %>% 
    filter(month %in% 5:8) %>% 
    dplyr::select(julian, month, mov) %>% 
    rename(mov_ts = 'mov') %>% 
    mutate(type = case_when(
      lag(mov_ts) - mov_ts > 0  & lead(mov_ts) - mov_ts > 0  ~ 1, 
      lead(mov_ts) - mov_ts < 0 & lag(mov_ts) - mov_ts < 0  ~ 2,
      TRUE ~ 0  ) )  
  
  return(year_pixel)}

run_for_each_OS <- function(dr){
  
  MSD_proof <- dr %>% 
    nest(-station_N, -Lon, -Lat, -year) %>% 
    mutate(data = purrr::map(.x = data, .f = filter_data), 
           Two_MSD = purrr::map(.x = data, .f = define_two_MSD))
  
  
  MSD_proof <- MSD_correction(MSD_proof)
  return(MSD_proof)}

data_Oy <- function(data){
  
  data_base <- data %>%
    filter(month %in% 5:8) %>%
    rename(mov_ts = mov) %>% 
    dplyr::select(julian, month, mov_ts) %>% 
    mutate(type = case_when(
      lag(mov_ts) - mov_ts > 0  & lead(mov_ts) - mov_ts > 0  ~ 1, 
      lead(mov_ts) - mov_ts < 0 & lag(mov_ts) - mov_ts < 0  ~ 2,
      TRUE ~ 0  ) ) 
  return(data_base)}








# =-=-=-=-=-=-=-=-=-=-=-=-=-=
# Do CPT file.
# =-=-=-=-=-=-=-=-=-=-=-=-=-= 
 CPT_file <- function(data, var){
  # data <- MSD_data
  # var <- 'Intensity'
   
    CPT_data <- data %>% 
     dplyr::select(-id, -data) %>% 
     unnest %>% 
     dplyr::select(year, id, !!var) %>% 
     spread(key = id, value = !!var) 
    
   Lat_Long  <- data %>% 
     dplyr::select(-id, -data) %>% 
     unnest %>% 
     dplyr::select(x, y) %>% 
     unique %>% 
     t() 
 
   colnames(Lat_Long) <- paste0(1:150)
   rownames(Lat_Long) <- NULL
   
   
   Lat_Long <- add_column(as_tibble(Lat_Long), year = c('cpt:X', 'cpt:Y'), .before = 1)  
   
   names(Lat_Long) <- c('', paste0('V',1:150))
   names(CPT_data) <- c('', paste0('V',1:150))

  # =-=-=-=-=-=-=-=-=-=-=-=
   CPT_data <- CPT_data %>% 
     mutate_if(is.factor, as.character) %>% 
     mutate_if(is.character, as.numeric)  %>%
     rbind(Lat_Long, .) 
   
 
   file <- paste0('D:/OneDrive - CGIAR/Desktop/USAID-Regional/USAID-REGIONAL/MSD_Index/CPT_files/Chirps_', var, '.txt')
   
   
   sink(file = file)
   cat('xmlns:cpt=http://iri.columbia.edu/CPT/v10/', sep = '\n')
   cat('cpt:nfield=1', sep = '\n')
   cat(glue("cpt:field=days, cpt:nrow=37, cpt:ncol=150, cpt:col=station, cpt:row=T, cpt:units=julian;cpt:missing=-999"), sep = '\n')
   cat(write.table(CPT_data, sep = '\t', col.names = TRUE, row.names = FALSE, na = "", quote = FALSE))
   sink()
 
 }  

# MSD_data %>%
#   dplyr::select(-id, -data) %>%
#   unnest
#  Var <- c(Length, Intensity, Magnitude)

CPT_file(data = MSD_data, var = 'Length')
# CPT_file(data = MSD_data, var = 'Intensity')
# CPT_file(data = MSD_data, var = 'Magnitude')



















# =-=-=-=-=-=-=-=-=-=-=-=-=-=
# Do CPT file.
# =-=-=-=-=-=-=-=-=-=-=-=-=-= 
# CPT_file <- function(data, var){
#  # data <- MSD_data
#  # var <- 'Intensity'
#   
#    CPT_data <- data %>% 
#     dplyr::select(-id, -data) %>% 
#     unnest %>% 
#     dplyr::select(year, id, !!var) %>% 
#     spread(key = id, value = !!var) 
#    
#   Lat_Long  <- data %>% 
#     dplyr::select(-id, -data) %>% 
#     unnest %>% 
#     dplyr::select(x, y) %>% 
#     unique %>% 
#     t() 
# 
#   colnames(Lat_Long) <- paste0(1:150)
#   rownames(Lat_Long) <- NULL
#   
#   
#   Lat_Long <- add_column(as_tibble(Lat_Long), year = c('cpt:X', 'cpt:Y'), .before = 1)  
#   
#   names(Lat_Long) <- c('', paste0('V',1:150))
#   names(CPT_data) <- c('', paste0('V',1:150))
# 
# 
#   
#   # =-=-=-=-=-=-=-=-=-=-=-=
#   CPT_data <- CPT_data %>% 
#     mutate_if(is.factor, as.character) %>% 
#     mutate_if(is.character, as.numeric)  %>%
#     rbind(Lat_Long, .) 
#   
# 
#   file <- paste0('D:/OneDrive - CGIAR/Desktop/USAID-Regional/USAID-REGIONAL/MSD_Index/CPT_files/Chirps_', var, '.txt')
#   
#   
#   sink(file = file)
#   cat('xmlns:cpt=http://iri.columbia.edu/CPT/v10/', sep = '\n')
#   cat('cpt:nfield=1', sep = '\n')
#   cat(glue("cpt:field=days, cpt:nrow=37, cpt:ncol=150, cpt:col=station, cpt:row=T, cpt:units=julian;cpt:missing=-999"), sep = '\n')
#   cat(write.table(CPT_data, sep = '\t', col.names = TRUE, row.names = FALSE, na = "", quote = FALSE))
#   sink()
# 
# }  

## MSD_data %>%
##   dplyr::select(-id, -data) %>%
##   unnest
##  Var <- c(Length, Intensity, Magnitude)

# CPT_file(data = MSD_data, var = 'Length')
# CPT_file(data = MSD_data, var = 'Intensity')
# CPT_file(data = MSD_data, var = 'Magnitude')



