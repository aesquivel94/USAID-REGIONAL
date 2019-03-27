rm(list = ls()); gc(reset = TRUE)
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Made by:     Alejandra Esquivel Arias. 
# Created in:  Date: 3-2019
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
library(cowsay)
library(sf)
library(skimr)
library(gganimate)
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
                              '                                  '             
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


month_min <- 8;  tolerance <-  0.2

# =-=-=-=-=-=-=-=-=-=-=-= Function ---- next_minimum. 
#  This function compute a Min local min. 
next_minimum <- function(x, fecha){
  
  # x <- pixel_yearT
  # fecha <- dates_canicula
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


# =-=-=-=-=-=-=-=-=-=-=-= Function  ---- left_max_MSD.
# Function to do start MSD. This it's the first maximum.
left_max_MSD <- function(Possible_dates, MinDate, tolerance){
  
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
    mutate(pendiente = (mov - MinDate$mov)/mov) %>%
    filter(pendiente >= tolerance) %>% # Mod
    arrange(desc(mov)) %>% 
    filter(row_number()==1)
  
  
  return(left_maximum)}
# =-=-=-=-=-=-=-=-=-=-=-= 


# =-=-=-=-=-=-=-=-=-=-=-= Function  ---- right_max_MSD.
# Try to do second max. 
right_max_MSD <- function(pyT, MinDate, tolerance){
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
    # filter(pendiente >= 0.20)  manana pienso que hacer aqui
    filter(pendiente >= tolerance)
  
  
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
  
  if(nrow(second_max) != 0){
    second_max <-   pyT %>% 
      filter(julian ==  second_max %>% dplyr::select(julian) %>% as.numeric()) %>% 
      bind_rows(End_1) %>% 
      mutate(type = c('End', 'End2'))
    
  } else{
    second_max <- second_max 
  }
  
  return(second_max)}
# =-=-=-=-=-=-=-=-=-=-=-= 


# =-=-=-=-=-=-=-=-=-=-=-= Function  ---- MSD index. 
# MSD index canicula 
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
# =-=-=-=-=-=-=-=-=-=-=-= 


# =-=-=-=-=-=-=-=-=-=-=-= Function  ---- MSD_id_Year. 
# This function do MSD for each id-year. 
# For use this function is necesary next_minimum, left_max_MSD, right_max_MSD, MSD_index functions. 
MSD_id_Year <- function(id, pixel_yearT){

  # pixel_yearT <- id_year_prec %>%
  #   # filter(year == 2008, id ==34) %>%
  #   filter(row_number() == 2) %>%
  #   dplyr::select(data) %>%
  #   unnest()
  # 
  # id <- id_year_prec %>%
  #   # filter(year == 2008, id ==34) %>%
  #   dplyr::select(id) %>%
  #   filter(row_number() == 2) %>%
  #   unique() %>%
  #   as.numeric()
  
  # First select id and year data set and 
  # create variable Local (define if it's or not local min-max).
  pixel_yearT <-    pixel_yearT  %>% 
    mutate(id = id) %>% 
    filter(month %in% 5:10) %>% # 
    mutate(Local = case_when(
      lag(mov) > mov & lead(mov) > mov ~ 1, 
      lead(mov) < mov & lag(mov) < mov ~ 2,
      TRUE ~ 0  ) )
 
  
  print( pixel_yearT %>% 
    filter(row_number() == 1) %>% 
    dplyr::select(date, id) %>% 
      mutate(month_min = month_min, tolerance = tolerance))
  
  
  
  # Compute the first local max in the data.
  first_maximum <- pixel_yearT %>%
    filter(Local == 2) %>%
    filter(row_number()==1) %>%
    pull(julian)

  
  # Dates would be possible start MSD.
  init_canicula <- pixel_yearT %>%
    filter(Local ==2) %>%
    filter(julian >=first_maximum, julian < 225) %>%
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
  

  # =-=-=-=-=-=-=-=-=-=-=-= Function ---- next_minimum. 

  
  # In this point we compute the Local min. 
  posible_minimo <- next_minimum(x = pixel_yearT, fecha = dates_canicula)


  # rm(pixel_yearT, id, init_canicula, first_maximum, last_date, dates_canicula, posible_minimo)
   
  # This point we compute if the Local min is in a establish limits. 
  cond_canicula <- posible_minimo %>%
    mutate(cond_midsummer = between(month, 6, month_min)) %>% # Mod
    pull(cond_midsummer)
  
  
  # =-=-=-=-=-=-=-=-=-=-=-= Function  ---- left_max_MSD.
 
  # =-=-=-=-=-=-=-=-=-=-=-= Function  ---- right_max_MSD.
  
  # =-=-=-=-=-=-=-=-=-=-=-= Function  ---- MSD index. 
  
  
  # In this point we compute the start MSD. 
  left_start <- left_max_MSD(Possible_dates = init_canicula, MinDate =  posible_minimo, tolerance = tolerance)
  right_End <- right_max_MSD(pyT = pixel_yearT, MinDate = posible_minimo, tolerance = tolerance)
  
 
  
  if(isTRUE(cond_canicula) &  nrow(left_start) != 0 &  nrow(right_End) != 0){
    
    # Create MDS Index.
    MSD <- bind_rows(left_start, posible_minimo, right_End) %>% 
      dplyr::select(-minimo, -day, -pendiente) %>% 
      mutate(type = c('Start', 'Min', 'End', 'End2'))
    
    # p <- ggplot() + 
    #   geom_line(data = pixel_yearT, aes(julian, mov)) + 
    #   # geom_line(aes(julian, prec)) +
    #   geom_vline(data = left_start, mapping=aes(xintercept=julian), color='blue') +
    #   geom_vline(data = posible_minimo, mapping=aes(xintercept=julian), color='red') +
    #   # geom_vline(data = right_End, mapping=aes(xintercept=julian)) +
    #   geom_smooth(data = pixel_yearT, aes(julian, mov)) +
    #   labs(x = 'Julian day', y = 'Triangular Moving Average (mm)') + 
    #   theme_light()
    
    
    MSD_R <- MSD_index(pyT = pixel_yearT, canicula = MSD)
    
    # print(p)
    
  }else{
    
    cowsay::say("Sorry we couldn't find midsummer drought (canicula)!", 
                "smallcat", what_color = "blue")
    
    MSD_R <- pixel_yearT %>% 
      filter(row_number() == 1) %>% 
      dplyr::select(id, x, y) %>% 
      mutate(Min = -999, Start = -999, End = -999, End_P = -999, 
             Length = -999, Intensity = -999, Magnitude = -999)
  }


return(MSD_R)}
# =-=-=-=-=-=-=-=-=-=-=-= 


# =-=-=-=-=

tictoc::tic()
MSD_data <- id_year_prec %>% 
  # filter(row_number() <5) %>% 
  mutate(MSD = purrr::map2(.x = id, .y = data, .f = MSD_id_Year)) # %>% 
  # dplyr::select(-data) %>% 
  # unnest()
tictoc::toc() # 24.7





# Save MSD data Grilled. 
MSD_data %>% 
  dplyr::select(year, MSD) %>% 
  unnest %>% 
  write_csv('MSD_Index/MSD_G.csv')


# Save MSD data Grilled. 
MSD_data %>% 
  dplyr::select(year, id,  data) %>% 
  unnest %>% 
  write_csv('MSD_Index/Data_G.csv')




# MSD_data <- read_csv('MSD_Index/MSD_G.csv')

# Percent NA by year.
MSD_data %>%
  dplyr::select(-data, -id) %>%
  unnest %>%
  group_by(year) %>%
  summarise_at(vars(Min:Magnitude), .funs = function(x){round(sum(x == -999)/150, 2)}) %>%
  ggplot(aes(x =  year, y =  Min)) +
  geom_bar(stat = 'identity', fill = 'red') +
  geom_hline(yintercept = 0.3) +
  theme_bw() +
  labs(x = NULL, y = '% NA')



#  Summary by year (for the most important variables. )
MSD_data %>%
  dplyr::select(-data, -id)  %>% 
  unnest %>% 
  na_if(-999) %>%
  dplyr::select( year, Length, Intensity, Magnitude) %>% 
  group_by(year) %>% 
  skimr::skim(.)  


# Summary for each year... Count NA data.
MSD_data %>%
  dplyr::select(-data, -id)  %>% 
  unnest %>% 
  na_if(-999) %>%
  dplyr::select( year, Length) %>% 
  group_by(year) %>% 
  skimr::skim(.) 


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=-
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Testing gganimate for do animation graphs.
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

# =-=-=-=-=-=-=-=
shp <- st_as_sf(HM_shp) 
dry_C <- read_sf('D:/OneDrive - CGIAR/Desktop/USAID-Regional/USAID-REGIONAL/Honduras_Corredor_Seco.shp') 



p_data  <- MSD_data %>%
  dplyr::select(-data, -id)  %>%
  unnest %>%
  na_if(-999) %>% 
  # dplyr::select(year,  x, y, Magnitude ) %>% 
  # filter(between(year, 2015, 2018)) %>%
  mutate(year = as.integer(year)) 

p_data %>% dplyr::select(year) %>% unique


anim <- ggplot(p_data) +
  geom_tile(aes(x, y, fill = Magnitude )) + 
  scale_fill_viridis(na.value="white",  direction = -1) + 
  geom_sf(data = shp, fill = NA, color = gray(.5)) +
  geom_sf(data = dry_C, fill = NA, color = gray(.1)) + 
  theme_bw() +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {closest_state}', x = 'Longitud', y = 'Latitud') + 
  transition_states(year, transition_length = 5, state_length = 8) +
  enter_fade() +
  exit_fade()


anim_save("MSD_Index/Magnitude_Chirps.gif", anim)


# =-=-=-=-= Generación de los gif para las series de tiempo. 
library(magick)
# library(ggplot2)

MSD_data1 <- MSD_data %>% 
  mutate(MSD = purrr::map2(.x = MSD, year,.f = function(.x, .y){
    .x <- .x %>% 
      mutate(year = .y)
  }))



Individual_graph <- function(Data_index, Data){
  # Data_index<- MSD_data1$MSD[[1]]
  # Data <- MSD_data1$data[[1]]
  
  Data_index <- Data_index %>% 
    na_if(-999)
  
  graph <-  Data  %>% 
    filter(between(month, 5, 10)) %>% 
    ggplot() + 
    geom_line(aes(julian, mov)) +
    theme_bw() +
    labs(x = 'Día Juliano', y = "Promedio Triangular (mm)") + 
    geom_vline( xintercept = c(Data_index$Start, Data_index$End), linetype=4, colour = 'blue') +
    geom_vline(xintercept = Data_index$Min,  linetype=4, colour = 'skyblue') + 
    labs(title = glue::glue('Id {Data_index$id}: Lon {Data_index$x} - Lat {Data_index$y} --- year {Data_index$year}'))
  
  print(graph)
}


by_id <- function(MSD_Local, id_pixel){
  # MSD_Local <- MSD_data1 %>%  filter(id == 2)

  img <- image_graph(600, 340, res = 96)
  out <- purrr::map2(.x = MSD_Local$MSD, 
                     .y = MSD_Local$data, 
                     .f = Individual_graph)
  dev.off()
  
  animation <- image_animate(img, fps = 2)
  # print(animation)
  image_write(animation, glue::glue("MSD_Index/Chirps_series_by_id/id_{id_pixel}.gif"))
}

tictoc::tic()
MSD_data1 %>%  
  dplyr::select(-year) %>% 
  nest(-id) %>% 
  mutate(try = purrr::map2(.x = data, .y = id, .f = by_id))
tictoc::toc() # 1.47 h
# rm(MSD_data1)




 
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=-  
  
# =-=-=-=-=-=-=-=
Honduras_art <- tibble(x = -c(87.65, 87.15, 87.22), y = c(13.29, 13.32,14.06))

# a <- MSD_data %>%
#   dplyr::select(-data, -id) %>%
#   unnest %>%
#   group_by(id, x, y) %>% 
#   summarise(percent = round(sum(Start == -999)/37, 2)*100)   %>% 
#   filter(percent < 40)
# 
# 
#  # f <-  ggplot(a) +
#   geom_tile(aes(x, y, fill = percent))  + 
#   scale_fill_viridis() + 
#   geom_sf(data = shp, fill = NA, color = gray(.5)) +
#   geom_sf(data = dry_C, fill = NA, color = gray(.1)) + 
#   theme_bw() + 
#   labs(x = NULL, y = NULL, fill = 'NA %') + 
#   geom_point(data = Honduras_art, aes(x, y))

  


 
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
  

  file <- paste0('D:/OneDrive - CGIAR/Desktop/USAID-Regional/USAID-REGIONAL/MSD_Index/', var, '.txt')
  
  
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
CPT_file(data = MSD_data, var = 'Intensity')
CPT_file(data = MSD_data, var = 'Magnitude')






######### en prueba... 

# =-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-
# Station data.
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


# test <- id_year_prec %>%
#   unnest %>% 
#   dplyr::select(-layer, -julian, -mov) %>% 
#   nest(-id, -x, -y) 


# test$x
# Catalog_M$Lon


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


# Joint_CS %>% 
#   dplyr::select(data) %>% 
#   filter(row_number() == 1) %>% 
#   unnest  %>% 
#   slice(n()) 



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


# =-=-=-=-=-
# Creo que justo aquí iba trabajando el viernes. 
testing <- Joint_CS %>%
  right_join(., coef) %>% 
  mutate(data_filling = purrr::map2(.x = data, .y = coef, .f = filling_data)) %>%
  dplyr::select(-data, -coef) %>% 
  dplyr::select(-NA_percent) %>%
  # filter(row_number() == 1) %>% 
  unnest 


# Grafico de prec de las diapositivas. 
testing %>% 
  dplyr::select(station_N, prec, Chirps, julian, prec_R, prec_C) %>% 
  gather(type, value, -station_N, -julian) %>% 
  filter( type %in% c('prec', 'Chirps')) %>% 
  ggplot(aes( x = julian, y = value, colour = type)) +
  geom_line(alpha = 0.7) +
  scale_color_viridis_d() +
  facet_wrap(~station_N, nrow = 2) +
  theme_bw()  +
  theme(legend.position = 'top') + 
  labs(x = 'Julian Day', y = 'Precipitation (mm)')


# :P


# Grafico del llenado de datos. 
testing %>% 
  dplyr::select(station_N, prec, julian, prec_R) %>% 
  mutate(Fill = ifelse(prec == prec_R, 'Original', 'Model')) %>%
  ggplot(aes(x = julian, y = prec_R, group = station_N)) +
  geom_line(colour = 'red') +
  geom_line(mapping = aes(y = prec), lwd = 1.3) +
  facet_wrap( ~ station_N,  nrow = 2) + 
  theme_bw()  + 
  labs(x = 'Julian Day', y = 'Precipitation (mm)')



# Por estación ------ sin embargo hay que tener encuenta que no se presentan mas del 5% de NA. 
testing %>% 
  dplyr::select(station_N, prec, julian, prec_R) %>% 
  mutate(Fill = ifelse(prec == prec_R, 'Original', 'Model')) %>%
  filter(station_N == 'X56035_21deOctubre') %>% 
  ggplot(aes(x = julian, y = prec_R)) +
  geom_line(colour = 'red') +
  geom_line(mapping = aes(y = prec), lwd = 1.3) +
  theme_bw()  + 
  labs(x = 'Julian Day', y = 'Precipitation (mm)')



# new_JointCS %>% 
#   dplyr::select(station_N, Lon,Lat,  data) %>% 
#   unnest %>% 
#   write_csv('MSD_Index/DataS_O.csv')
  

# =-=-=-=-=-=-=-=-

test <- new_JointCS %>% 
  rename(x = 'Lon', y = 'Lat', id = 'station_N') %>% 
  dplyr::select(-data_filling) %>% 
  unnest %>% 
  mutate(prec = prec_C) %>% # new row
  dplyr::select(year, id, date, julian, month, x, y,  prec, mov, mov_R) %>% 
  nest(-year, -id)

 
choluteca <- new_JointCS %>% 
  dplyr::select(station_N , data) %>% 
  filter(row_number() == 10) %>% 
  unnest


choluteca %>% 
  dplyr::select(year, date, date, prec, prec_R) %>% 
  filter(year == 2017) %>% 
  ggplot(.) + 
  geom_line(aes(date, prec_R), colour = 'red') + 
  geom_line(aes(date, prec)) + 
  theme_bw() +
  labs(x = NULL, y = 'Precipitación (mm)') + 
  geom_vline( xintercept = as.numeric(as.Date("2017-05-01")), linetype=4, colour = 'blue') +
  geom_vline( xintercept = as.numeric(as.Date("2017-09-01")), linetype=4, colour = 'blue')
  
  




# row_test <- test %>% 
#   filter(row_number() == 300) 
#   
#   
# row_test %>% 
#   dplyr::select(data) %>% 
#   unnest %>% dplyr::select(julian) %>% 
#   unique %>%
#   slice(n())
#   
#   # mutate(test = purrr::map2(.x = id, .y = data, .f = MSD_id_Year))
#   # MSD_id_Year(id = id , pixel_yearT = data)
# 
# id <- row_test$id
# pixel_yearT <-row_test %>% dplyr::select(data) %>% unnest
# 
# MSD_id_Year(id = id, pixel_yearT = pixel_yearT)



# Voy a hacer esta prueba con el llenado de la regresión: R. 
proof <- test %>% 
  unnest %>% 
  dplyr::select(-mov) %>% 
  rename(mov = 'mov_R') %>% 
  nest(-year, -id) %>% 
  mutate(MSD = purrr::map2(.x = id, .y = data, .f = MSD_id_Year))


# Skim (solo para resumen. )
proof %>% 
  dplyr::select(-data) %>% 
  unnest %>% 
  dplyr::select(id, Intensity, Length, Magnitude) %>% 
  dplyr::select(id, Length) %>% 
  na_if(-999) %>% 
  group_by(id) %>% 
  skim()


# =-=-=-=-=-=-=-=-=-=
graph <- proof %>% 
  # filter(year == 2017) %>% 
  dplyr::select(-data) %>%
  unnest %>% 
  na_if(-999) %>% 
  mutate(year = as.integer(year))


 
  anim <- ggplot(graph) +
  geom_point(aes(x, y, colour = Length)) +  
  scale_colour_viridis(na.value="white",  direction = -1) +  
  geom_sf(data = shp, fill = NA, color = gray(.5)) +
  geom_sf(data = dry_C, fill = NA, color = gray(.1)) + 
  theme_bw() + 
  # geom_point(data = Honduras_art, aes(x, y)) # +
    # Here comes the gganimate specific bits
  labs(title = 'Year: {closest_state}', x = 'Longitud', y = 'Latitud') + 
  transition_states(year, transition_length = 5, state_length = 8) +
  enter_fade() +
  exit_fade()
  
  anim_save("MSD_Index/Length_stations.gif", anim)
  


# Hacer un comparativo en esos puntos por lo menos para la estación 
# tratar de terminarlo de aquí a mañana. 
  
# Hacer la serie de llenado de datos para estaciones
# Intentar hacer el llenado de datos (modelo-por año)...


  choluteca_MSD <- proof %>% 
    dplyr::select(id, year, MSD) %>%
    filter(id == 'X78724_choluteca', year == 2004) %>% 
    unnest %>% 
    dplyr::select(-id1)

  
  
choluteca %>% 
    filter(year == 2017) %>% 
    filter(between(month, 5, 9)) %>% 
    ggplot(.) +
    geom_line(aes(julian, mov)) + 
    geom_vline( xintercept = c(choluteca_MSD$Start, choluteca_MSD$End), 
                linetype=4, colour = 'blue') +
   geom_vline(xintercept = c(choluteca_MSD$Min), 
              linetype=4, colour = 'skyblue') + 
  theme_bw()
    











# =-=-=-=-= Generación de los gif para las series de tiempo. 


proof1 <- proof %>% 
  mutate(MSD = purrr::map2(.x = MSD, year,.f = function(.x, .y){
    .x <- .x %>% 
      mutate(year = .y)
  }))



Individual_graph <- function(Data_index, Data){
  # Data_index<- MSD_data1$MSD[[1]]
  # Data <- MSD_data1$data[[1]]
  
  Data_index <- Data_index %>% 
    na_if(-999)
  
  graph <-  Data  %>% 
    filter(between(month, 5, 10)) %>% 
    ggplot() + 
    geom_line(aes(julian, mov)) +
    theme_bw() +
    labs(x = 'Día Juliano', y = "Promedio Triangular (mm)") + 
    geom_vline( xintercept = c(Data_index$Start, Data_index$End), linetype=4, colour = 'blue') +
    geom_vline(xintercept = Data_index$Min,  linetype=4, colour = 'skyblue') + 
    labs(title = glue::glue('Id {Data_index$id}: Lon {Data_index$x} - Lat {Data_index$y} --- year {Data_index$year}'))
  
  print(graph)
}
by_id <- function(MSD_Local, id_pixel){
  # MSD_Local <- MSD_data1 %>%  filter(id == 2)
  
  img <- image_graph(600, 340, res = 96)
  out <- purrr::map2(.x = MSD_Local$MSD, 
                     .y = MSD_Local$data, 
                     .f = Individual_graph)
  dev.off()
  
  animation <- image_animate(img, fps = 2)
  # print(animation)
  image_write(animation, glue::glue("MSD_Index/Station_by_id/id_{id_pixel}.gif"))
}




tictoc::tic()
proof1 %>%  
  dplyr::select(-year) %>% 
  nest(-id) %>% 
  mutate(try = purrr::map2(.x = data, .y = id, .f = by_id))
tictoc::toc() # 1.47 h
# rm(MSD_data1)






CPT_file_s <- function(data, var){
  data <- proof
  var <- 'Intensity'
  
 
  
  CPT_data <- data %>% 
    dplyr::select(-id, -data) %>% 
    unnest %>% 
    dplyr::select(year, id, !!var) %>% 
    mutate_if(is.numeric, list(~round(., 1))) %>%
    spread(key = id, value = !!var)  
  
  Lat_Long  <- data %>% 
    dplyr::select(-id, -data) %>% 
    unnest %>% 
    dplyr::select(x, y) %>% 
    unique %>% 
    t() 
  
  colnames(Lat_Long) <- paste0(1:10)
  rownames(Lat_Long) <- NULL
  
  
  Lat_Long <- add_column(as_tibble(Lat_Long), year = c('cpt:X', 'cpt:Y'), .before = 1)  
  
  names(Lat_Long) <- c('', paste0('V',1:10))
  names(CPT_data) <- c('', paste0('V',1:10))
  
  
  
  # =-=-=-=-=-=-=-=-=-=-=-=
  CPT_data <- CPT_data %>% 
    mutate_if(is.factor, as.character) %>% 
    mutate_if(is.character, as.numeric)  %>%
    rbind(Lat_Long, .) 
  
 
  file <- paste0('D:/OneDrive - CGIAR/Desktop/USAID-Regional/USAID-REGIONAL/MSD_Index/CPT_files/Station_', var, '.txt')
  
  sink(file = file)
  cat('xmlns:cpt=http://iri.columbia.edu/CPT/v10/', sep = '\n')
  cat('cpt:nfield=1', sep = '\n')
  cat(glue("cpt:field=days, cpt:nrow=36, cpt:ncol=10, cpt:col=station, cpt:row=T, cpt:units=julian;cpt:missing=-999"), sep = '\n')
  cat(write.table(CPT_data, sep = '\t', col.names = TRUE, row.names = FALSE, na = "", quote = FALSE))
  sink()
  
}  

# proof
# Intensity - Length - Magnitude
CPT_file_s(data = proof, var = 'Intensity' )


  
