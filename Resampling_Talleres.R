rm(list = ls()); gc(reset = TRUE)
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Made by:     Alejandra Esquivel Arias. 
# Created in:  Date: 7-2019.
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Resampling Methodology 


# If it's the first time you run this script.
list.of.packages <- c('tidyverse', 'lubridate', 'glue')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages) > 0) install.packages(new.packages,dependencies = TRUE)
rm(new.packages)


# =-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-
# Packages. 
# =-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-
library(tidyverse)
library(glue)
library(lubridate)
# =-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=- 


# =-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=- Functions by sections.
# .------..------..------..------..------..------..------..------..------.
# |F.--. ||U.--. ||N.--. ||C.--. ||T.--. ||I.--. ||O.--. ||N.--. ||S.--. |
# | :(): || (\/) || :(): || :/\: || :/\: || (\/) || :/\: || :(): || :/\: |
# | ()() || :\/: || ()() || :\/: || (__) || :\/: || :\/: || ()() || :\/: |
# | '--'F|| '--'U|| '--'N|| '--'C|| '--'T|| '--'I|| '--'O|| '--'N|| '--'S|
# `------'`------'`------'`------'`------'`------'`------'`------'`------'
# =-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=- 


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# 1. Fix february: depends if leap year it's true or false.
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

# It's use when leap == FALSE (this function add a row in each february with 28 days). 
add_29_day <- function(to_change){
  # to_change <-  testing_Data %>%  filter(leap ==  FALSE) %>% dplyr::select(-leap) 
  
  Dato_C <- to_change %>%  
    nest(-year) %>% 
    mutate(data = purrr::map(.x = data, .f = function(.x){
      data_add <- bind_rows(.x, .x %>% sample_n(size = 1) %>% mutate(day = 29)) 
      return(data_add)})) %>% 
    unnest %>% 
    dplyr::select(day, month,  year, precip,  tmax,  tmin,  srad)
  return(Dato_C)}

# It's use when leap == TRUE (this function delete a row in each february with 29 days). 
less_29_day <- function(to_change){
  # to_change <-  testing_Data %>%  filter(leap ==  TRUE) %>% dplyr::select(-leap) 
  
  Dato_C <- to_change %>% 
    nest(-year) %>% 
    mutate(data = purrr::map(.x = data, .f = function(.x){
      data_less <- .x %>% slice(-n())
      return(data_less)})) %>% 
    unnest %>% 
    dplyr::select(day, month,  year, precip,  tmax,  tmin,  srad) 
  return(Dato_C)}

# This function organize the february data.
change_Leap <- function(leap_forecast, feb_data){
  # feb_data <- data_P %>% filter(month_P == 2) %>% dplyr::select(data) %>% unnest
  # leap_forecast <- FALSE
  
  data_to_change <- feb_data %>% 
    mutate(leap = leap_year(year)) %>% 
    nest(-leap)
  
  if (leap_forecast == TRUE) { # if year_forecast == TRUE (all days need to have 29 days).
    data_to_change <- data_to_change %>% 
      mutate(data = purrr::map_if(.x = data, .p = leap == FALSE , .f = add_29_day))
  } else {
    data_to_change <- data_to_change %>% 
      mutate(data = purrr::map_if(.x = data, .p = leap ==  TRUE, .f = less_29_day))
  }
  
  data_to_change <- data_to_change %>% 
    unnest %>% 
    dplyr::select(-leap) %>%  
    arrange(year) 
  
  return(data_to_change) }

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# 2. Organize Probability, monthly data and daily data. 
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

# .1. Put in the probabily object start month of season and end month. 
season_to_months <-  function(season){
  
  # season <- CPT_prob %>% 
  #   nest(-Season) %>% 
  #   dplyr::select(Season) %>% 
  #   filter(row_number() == 1)
  
  all_seasons <-  paste0(str_sub(month.abb, 1, 1), lead(str_sub(month.abb, 1, 1)),
                         lead(lead(str_sub(month.abb, 1, 1), n = 1))) %>% 
    tibble(x = . ) %>% 
    mutate(x = case_when(x == 'NDNA' ~ 'NDJ', 
                         x == 'DNANA' ~ 'DJF', 
                         TRUE ~ as.character(x)) ) %>% 
    mutate(start_month = 1:12, end_month = c(3:12, 1, 2))
  
  
  
  all_seasons <- all_seasons %>% 
    filter(str_detect(x, as.character(season)) ) %>% 
    dplyr::select(-x)
  
  
  return(all_seasons)}

# .2.This function organize and classify monthly data by category for one season.
do_organize_data <- function(Season, xi, data, Intial_year, last_year){
  
  # xi <- Times %>% filter(row_number() == 1) %>% dplyr::select(data) %>% unnest
  # Season <- 'NDJ'
  
  month_ini <- xi %>% 
    dplyr::select(start_month) %>% 
    unique() %>% 
    as.numeric()
  
  month_end <- xi %>% 
    dplyr::select(end_month) %>% 
    unique() %>% 
    as.numeric()
  
  
  if(Season == 'NDJ'){
    new_data <- data %>%
      filter(month %in% c(11,12,1)) %>% 
      mutate(year_M = ifelse(month == 1, year, year+1)) %>% 
      filter(year_M >= (Intial_year + 1), year_M < (last_year +1 ))%>%
      group_by(year_M) %>% 
      summarise(precip = sum(precip)) %>% 
      mutate(year = year_M - 1) %>% 
      dplyr::select(year, precip)
    
  } else if(Season == 'DJF'){
    new_data <- data %>%
      filter(month %in% c(11,12,1)) %>% 
      mutate(year_M = ifelse(month == 1, year, year+1)) %>% 
      filter(year_M >= (Intial_year + 1), year_M < (last_year +1 ))  %>%
      group_by(year_M) %>% 
      summarise(precip = sum(precip)) %>% 
      mutate(year = year_M - 1) %>% 
      dplyr::select(year, precip)
    
  } else{
    new_data <-  data %>%
      filter(between(month, month_ini, month_end)) %>%
      group_by(year) %>%
      summarise(precip = sum(precip))%>% 
      dplyr::select(year, precip)
  }
  
  
  # Se genera los quantiles de los promedios mensuales... (tener cuidado con como se generaron)
  quantile <- quantile(new_data$precip, probs = c(0.33, 0.66))
  
  # ClasificaciÃ³n de las series mensuales... 
  new_data <-  new_data %>% 
    mutate(condtion = case_when(
      precip < quantile[1] ~  'Below',
      precip > quantile[2] ~ 'Above' ,
      TRUE ~ 'Normal'
    )  ) %>% 
    nest(-condtion)
  
  
  return(new_data)}

# .3. This function create 100 samples category (only name).
sample_category <- function(Prob){
  # Prob <- Times %>%  filter(row_number() == 1) %>% dplyr::select(xi) %>% unnest()
  
  # Does the re-sampling of the categories...
  Type_cat <- tibble( id = 1:100) %>% 
    mutate(sample_cat = purrr::map(.x = id, .f = function(.x){
      sample_n(Prob,  size = 1, weight = Prob) }))
  
  return(Type_cat)}

# =-=-=-=-=
# .4. This function dependent of the category, we do the year sample.
year_function <- function(base_cat, mothly_data){
  
  # mothly_data <- Times %>% filter(row_number() == 1) %>% dplyr::select(month_data) %>% unnest
  # base_cat <- Times %>% filter(row_number() == 1) %>% dplyr::select(cat) %>% unnest
  
  by_cat <- function(cat, mothly_data){
    # cat <- base_cat %>% filter(row_number() < 2 ) %>% unnest %>% select( Type)
    
    mothly_data <- mothly_data %>% 
      filter(condtion == !!cat$Type) %>% 
      unnest %>% 
      sample_n(size = 1) %>% 
      dplyr::select(-precip)
    
    return(mothly_data)}
  
  year_sample <- base_cat %>% 
    mutate(sample =  purrr::map(.x = sample_cat, .f = by_cat, mothly_data = mothly_data)) %>% 
    # dplyr::select(-sample_cat) %>%
    unnest %>% 
    dplyr::select( -Type, -Prob)
  
  return(year_sample)}

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=
# 3. Daily data
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=
# This function extract daily data using sample year. 
day_sample <- function(Season, cat, data, Intial_year, last_year){
  # data it's station data.  
  # cat <-  Times %>%  dplyr::select(cat) %>% filter(row_number() < 2) %>% unnest
  # Season <- 'NDJ'
  
  month_ini <- cat %>% 
    dplyr::select(start_month) %>% 
    unique() %>% 
    as.numeric()
  
  month_end <- cat %>% 
    dplyr::select(end_month) %>% 
    unique() %>% 
    as.numeric()
  
  # Filter by season data serie.
  if(Season == 'NDJ'){
    Daily_filter <- data %>%
      filter(month %in% c(11,12,1)) %>% 
      mutate(year_M = ifelse(month == 1, year, year+1)) %>% 
      filter(year_M >= (Intial_year + 1), year_M < (last_year + 1))%>%
      mutate(year = year_M - 1) %>% 
      dplyr::select(-year_M)
    
  } else if(Season == 'DJF'){
    Daily_filter <- data %>%
      filter(month %in% c(11,12,1)) %>% 
      mutate(year_M = ifelse(month == 1, year, year+1)) %>% 
      filter(year_M >= (Intial_year + 1), year_M < (last_year +1 ))%>%
      mutate(year = year_M - 1) %>% 
      dplyr::select(-year_M)
    
  } else{
    Daily_filter <-  data %>%
      filter(between(month, month_ini, month_end)) 
  }
  
  
  Daily_data <- cat %>% 
    dplyr::select(-start_month, -end_month) %>% 
    mutate(daily_data = purrr::map(.x = year, .f = function(.x){
      Daily_filter %>% filter(year == .x)})) %>% 
    dplyr::select(-year)
  
}

# This function return a tibble with daily sceneries min and max. 
Find_Summary <- function(daily_by_season){
  # daily_by_season <-   data_to_esc %>% nest(-Season) %>%
  # filter(Season == 'FMA') %>% dplyr::select(data) %>% unnest
  
  # Solo se hace la agrupacion mensual.
  monthly <- daily_by_season %>% 
    group_by(year) %>% 
    summarise(monthly = sum(precip)) 
  
  # se extrae el minimo y maximo de la precipitacion. 
  Min_Max <-  monthly %>% 
    arrange(monthly) %>% 
    slice(c(1,n())) %>% 
    mutate(Type = c('min', 'max')) %>% 
    dplyr::select(-monthly)
  
  
  Lenght <-  daily_by_season %>% 
    filter(year %in% Min_Max$year) %>% 
    count(id) %>% 
    filter(row_number() == 1) %>% 
    dplyr::select(n) %>% 
    as.numeric
  
  
  
  Indicators <-  daily_by_season %>% 
    filter(year %in% Min_Max$year) %>% 
    dplyr::select(-id) %>% 
    unique %>%
    mutate(Type = rep(Min_Max$Type, each = Lenght )) %>% 
    nest(-Type)
  
  return(Indicators)}

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=
# 4. Resampling
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=
resampling <-  function(data, CPT_prob, year_forecast){

  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  # 1. Fix february: depends if leap year it's true or false.
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  
  # Create a new data (with standard february).
  data <- data %>% 
    mutate(month_P = month) %>% 
    nest(-month_P) %>% 
    mutate(data = purrr::map_if(.x = data ,.p = month_P == 2 ,
                                .f = change_Leap, leap_forecast = leap_year(year_forecast))) %>% 
    dplyr::select(data) %>% 
    unnest %>% 
    arrange(year)
  
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= 
  # =-=-=-=-=  Do years (start year and end year)...
  Intial_year <- data %>% 
    dplyr::select(year) %>% 
    unique %>% 
    slice(1) %>% 
    as.numeric()
  
  last_year <- data %>% 
    dplyr::select(year) %>% 
    unique %>% 
    slice(n()) %>% 
    as.numeric()
  
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  # 2. Organize Probability, monthly data and daily data. 
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  
  # Add start_month and end_month. 
  Times <- CPT_prob %>% 
    nest(-Season) %>% 
    mutate(Times = purrr::map(.x = Season, .f = season_to_months)) %>% 
    unnest(Times) %>% 
    unnest() %>% 
    nest(-Season)
  
  # In this part we create a new variable with monthly data classify.
  Times <- Times %>% 
    rename(xi = 'data') %>% 
    mutate(month_data = purrr::map2(.x = Season, .y = xi, 
                                    .f = do_organize_data, data = data, 
                                    Intial_year = Intial_year, last_year = last_year))
  
  # This function do the 100 category samples.   
  Times <- Times %>%
    mutate(cat = purrr::map(.x = xi,.f = sample_category))
  
  # =-=-=-=-=-=-=-=-
  # This function do the year sample depends of the sample category.
  Times <- Times %>% 
    mutate(cat = purrr::map2(.x = cat, .y = month_data, .f = year_function))

  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  Base_years <- Times %>% 
    mutate(order = paste0(letters[1:2], '.',Season)) %>% 
    dplyr::select(order, cat) %>% 
    unnest %>%
    dplyr::select(order, year) %>% 
    nest(-order) %>%
    spread(key = order, value = data) %>%
    unnest %>% 
    set_names(paste0(letters[1:2], '.',  Times$Season)) %>% 
    bind_cols(id = 1:100, .)

  # This function extract daily data using sample year.  
  daily_data <- Times %>% 
    mutate(daily_data = purrr::map2(.x = Season, .y = cat, .f = day_sample, 
                                    data = data, Intial_year = Intial_year, 
                                    last_year = last_year)) %>% 
    dplyr::select(Season, daily_data)
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=
  
  data_to_esc <- daily_data %>% 
    unnest %>% 
    dplyr::select(-condtion) %>% 
    nest(-id) %>% 
    mutate(data = purrr::map(.x = data, .f = function(.x){ .x %>%  unnest %>%   unnest()})) %>% 
    unnest
  
  # =-=-=-=-=-=-=-=
  Escenaries <-  data_to_esc %>% 
    mutate(year = year_forecast) %>% 
    mutate(year = case_when( 
      Season %in% c('NDJ', 'DJF') & month == 1 ~ year + 1,
      Season == 'DJF' & month == 2 ~ year + 1, 
      TRUE ~ year))  %>%
     dplyr::select(-Season) %>% 
     nest(-id) 
  
  
  # Here was Find_Summary function
  
  # In this part we create sceneries with min and max years 
  # (building from aggregate precipitation).
  Esc_Type <- data_to_esc %>%
    nest(-Season) %>%
    mutate(Summary = purrr::map(.x = data, .f = Find_Summary)) %>% 
    dplyr::select(-data) %>% 
    unnest() %>% 
    unnest %>% 
    arrange(Type) %>% 
    dplyr::select(-Season) %>% 
    nest(-Type)
  
  
  # This object is the mix with 3 data set (sceneries, sample years and sceneries types).
  All_data <- bind_cols( Escenaries %>% mutate(Row = 'a') %>% nest(-Row),
                         Base_years %>% mutate(Row = 'a') %>% nest(-Row) %>% rename(Base_years = 'data')) %>% 
    bind_cols(Esc_Type %>% mutate(Row = 'a') %>% nest(-Row) %>% rename(Esc_Type = 'data') ) %>% 
    dplyr::select(-Row1, -Row2) 
  
  return(All_data)}

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
#  5. Function to save all files from resampling.  
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
function_to_save <- function(station, Esc_all, path_out){
  
  # data <- Resam %>%  filter(row_number() == 1) %>%  dplyr::select(names, Escenaries)
  # station <- data %>% dplyr::select(names)
  # Esc_all <- data %>% dplyr::select(Escenaries) %>% unnest
  
  # Daily sceneries (generated with resampling).
  Escenaries <- Esc_all %>%
    dplyr::select(data) %>% 
    unnest
  
  Esc_C <- Escenaries %>% 
    mutate(file_name = glue::glue('{path_out}{station}/escenario_{id}.csv')) 
  
  # Creation of the data folder (where the results will be saved). 
  ifelse(dir.exists(glue::glue('{path_out}{station}')) == FALSE, 
         dir.create(glue::glue('{path_out}{station}')), 'ok')
  
  # Save daily sceneries.
  walk2(.x = Esc_C$data, .y = Esc_C$file_name, 
        .f = function(.x, .y){ write_csv(x = .x, path = .y)})
  
  # Save scenarios type. 
  Type_Esc <- Esc_all %>% 
    dplyr::select(Esc_Type) %>% 
    unnest %>% 
    mutate(file_name = glue::glue('{path_out}{station}/escenario_{Type}.csv'))
  
  walk2(.x = Type_Esc$data, .y = Type_Esc$file_name, 
        .f = function(.x, .y){ write_csv(x = .x, path = .y)})

  # Save resampling years.
  Esc_all %>% 
    dplyr::select(Base_years) %>% 
    unnest %>% 
    write_csv(., path = glue::glue('{path_out}{station}/Escenario_A.csv'))

  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  # summary variables files creation.
  Levels <- Esc_C %>% 
    dplyr::select(data) %>% 
    unnest %>% 
    dplyr::select(month) %>% 
    unique 
  
  summaries <- Esc_C %>% 
    dplyr::select(id, data) %>% 
    unnest %>% 
    mutate(month = factor(month,Levels$month))  %>% 
    group_by(id, month, year) %>% 
    summarise(precip = sum(precip), tmax = mean(tmax), tmin = mean(tmin), srad = mean(srad)) %>% 
    ungroup() %>% 
    dplyr::select(-id) %>%
    group_by(month) %>%
    group_by(year, month) %>%
    summarise(prec_avg = mean(precip), prec_max = max(precip), prec_min = min(precip),
              sol_rad_avg = mean(srad), sol_rad_max = max(srad), sol_rad_min = min(srad),
              t_max_avg = mean(tmax), t_max_max = max(tmax), t_max_min = min(tmax),
              t_min_avg = mean(tmin), t_min_max = max(tmin), t_min_min = min(tmin)) %>%
    ungroup()
  
  summaries <- summaries %>% 
    gather(variable, values, -month, -year) %>% 
    nest(-variable) %>% 
    mutate(data = purrr::map2(.x = variable, .y = data, .f = function(.x, .y){
      
      if(str_detect(.x , 'sol_rad_') == TRUE){
        .y <- .y  %>% 
          set_names(c('year', 'month', str_remove(.x , 'sol_rad_')))
      } else{
        .y <- .y  %>% 
          set_names(c('year', 'month', str_extract( .x ,'_[a-z]+') %>% str_remove('_')))
      }
      return(.y)})) %>% 
    mutate(file_name = glue::glue('{path_out}{station}/{variable}.csv'))
  
  # Aqui se guardan los archivos...
  walk2(.x = summaries$data, .y = summaries$file_name,
        .f = function(.x, .y){write_csv(x = .x, path = .y)})
}

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
#  6. function to save monthly scenarios (forecast + climatology).
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
Gen_to_monthly <- function(Escenaries, stations, names, path_out, year_forecast){
  
  if(dir.exists(glue::glue('{path_out}{names}_month')) == FALSE){dir.create(glue::glue('{path_out}{names}_month'))} else{'ok'}
  
  b <- Escenaries %>% 
    dplyr::select(data) %>% 
    unnest() %>% 
    mutate(summary = purrr::map(.x = data, .f = function(.x){
      .x %>%  mutate(year = year_forecast) %>%  group_by(month, year) %>% 
        summarise(precip = sum(precip), tmax = mean(tmax), tmin = mean(tmin), srad = mean(srad)) %>% 
        mutate(type = 'f')})) %>% 
    dplyr::select(-data)
  
  months <- b %>% filter(row_number() == 1) %>% dplyr::select(summary) %>% unnest() %>% .$month
  
  a <- stations %>% 
    group_by(month, year) %>% 
    summarise(precip = sum(precip), tmax = mean(tmax), tmin = mean(tmin), srad = mean(srad)) %>% 
    ungroup() %>% 
    mutate(year = year_forecast) %>%
    group_by(month) %>% 
    summarise_all(~mean(.))  %>% 
    filter(!(month %in% months)) %>% 
    mutate(type = 'c')
  
  month_sc <- b %>% 
    mutate(summary = purrr::map(.x = summary, .f = function(.x){
      .x %>% bind_rows(a) %>% arrange(month) }))
  
  
  walk2(.x = month_sc$summary, .y = glue::glue('{path_out}{names}_month/Esc_monthly_{month_sc$id}.csv'), 
        .f = function(.x, .y){ write_csv(x = .x, path = .y)})
  
  save_all_years <- Escenaries %>% dplyr::select(Base_years) %>% unnest()
  write_csv(save_all_years, glue::glue('{path_out}{names}_month/Escenario_A.csv'))
  
  
  mean_gen <- month_sc %>% dplyr::select(summary) %>% 
    unnest() %>% group_by(month, year, type) %>% 
    summarise_all(~mean(.)) %>% 
    dplyr::select(month, year, precip, tmax, tmin, srad, type)
  
  write_csv(mean_gen, glue::glue('{path_out}{names}_month/Esc_monthly_Tipo.csv'))
  
  climatology <- stations %>% 
    group_by(month, year) %>% 
    summarise(precip = sum(precip), tmax = mean(tmax), tmin = mean(tmin), srad = mean(srad)) %>% 
    ungroup() %>% 
    mutate(year = year_forecast) %>%
    group_by(month) %>% 
    summarise_all(~mean(.)) 
  
  write_csv(mean_gen, glue::glue('{path_out}{names}_month/Esc_monthly_Tipo.csv'))
}


#.------..------..------.
#|R.--. ||U.--. ||N.--. |
#| :(): || (\/) || :(): |
#| ()() || :\/: || ()() |
#| '--'R|| '--'U|| '--'N|
#`------'`------'`------'

# =-=-=-= Ejemplo 1. =-=-=-= Un solo archivo. 

# Carpeta en la que se guardan los resultados (Modificar). 
# path_out <- 'D:/OneDrive - CGIAR/Desktop/Costa_Rica/Resampling/Ejemplo_1/results/'
# 
# # Path del archivo de probabilidades (Modificar).
# CPT_path <-  'D:/OneDrive - CGIAR/Desktop/Costa_Rica/Resampling/Ejemplo_1/CPT_prob/CPT_prob_cerete.csv'
# #Path del archivo de estaciones (Modificar).
# Station_path <- 'D:/OneDrive - CGIAR/Desktop/Costa_Rica/Resampling/Ejemplo_1/daily_preliminar/Cerete.csv'
# # Año objetivo
# year_forecast <- Sys.Date() %>% year()
# 
# 
# Initial_data <- tibble(names = Station_path %>% basename() %>% str_remove('.csv')) %>% 
#   mutate(stations = purrr::map(.x = Station_path, .f =  readr::read_csv),
#          CPT_prob = purrr::map(.x = CPT_path, .f =  readr::read_csv))
# 
# # Aquí se corren los escenarios de remuestreo. 
# Resam <- Initial_data %>% 
#   mutate(Escenaries = purrr::map2(.x = stations, .y = CPT_prob, 
#                                   .f = resampling, year_forecast = year_forecast))
# 
# # Ejemplo de escenario
# Escenario_1 <- Resam %>% 
#   dplyr::select(Escenaries) %>% 
#   unnest() %>% 
#   dplyr::select(data) %>% 
#   unnest() %>% 
#   filter(row_number() == 1) %>% unnest()
# 
# print(Escenario_1)
# 
# # Aqui se presentan que años se seleccionaron para cada corrida. 
# years_for_all_sc <- Resam %>% 
#   dplyr::select(Escenaries) %>% 
#   unnest() %>% 
#   dplyr::select(Base_years) %>% 
#   unnest()
# 
# print(years_for_all_sc)
# 
# # =-=-=-=-=-=-=-=-=-=-=-=-=-=-= 
# # Guardado de los archivos del remuestreo.  
# purrr::map2(.x = Resam$names, .y = Resam$Escenaries, 
#             .f = function_to_save, path_out = path_out)



# =-=-=-= Ejemplo 2. =-=-=-= Varios archivos. 

# Carpeta en la que se guardan los resultados (Modificar). 
path_out <- 'D:/OneDrive - CGIAR/Desktop/Costa_Rica/Resampling/Ejemplo_2/results/'

# Path del archivo de probabilidades (Modificar).
Path_Prob <-  'D:/OneDrive - CGIAR/Desktop/Costa_Rica/Resampling/Ejemplo_2/CPT_prob/'
#Path del archivo de estaciones (Modificar).
Path_stations <- 'D:/OneDrive - CGIAR/Desktop/Costa_Rica/Resampling/Ejemplo_2/daily_preliminar/'
# Año objetivo
year_forecast <- Sys.Date() %>% year()



Initial_data <- tibble(names = list.files(Path_stations) %>% str_remove('.csv'), 
                       path_stations = Path_stations %>% list.files(full.names = TRUE), 
                       CPT_path = Path_Prob %>% list.files(full.names = TRUE)) %>% 
  mutate(stations = purrr::map(.x = path_stations, .f =  readr::read_csv),
         CPT_prob = purrr::map(.x = CPT_path, .f =  readr::read_csv)) %>% 
  dplyr::select(-path_stations, -CPT_path) 


# Aquí se corren los escenarios de remuestreo. 
Resam <- Initial_data %>% 
  mutate(Escenaries = purrr::map2(.x = stations, .y = CPT_prob, 
                                  .f = resampling, year_forecast = year_forecast))


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-= 
# Guardado de los archivos del remuestreo.  
purrr::map2(.x = Resam$names, .y = Resam$Escenaries, 
            .f = function_to_save, path_out = path_out)




# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Ejemplo 3. Mensual. 




Initial_data <- tibble(names = list.files(Path_stations) %>% str_remove('.csv'), 
                       path_stations = Path_stations %>% list.files(full.names = TRUE), 
                       CPT_path = Path_Prob %>% list.files(full.names = TRUE)) %>% 
  mutate(stations = purrr::map(.x = path_stations, .f =  readr::read_csv),
         CPT_prob = purrr::map(.x = CPT_path, .f =  readr::read_csv)) %>% 
  dplyr::select(-path_stations, -CPT_path) 


# Aquí se corren los escenarios de remuestreo. 
Resam <- Initial_data %>% 
  mutate(Escenaries = purrr::map2(.x = stations, .y = CPT_prob, 
                                  .f = resampling, year_forecast = year_forecast))
# Aqui se guardan los escenarios mensuales de remuestreo. 
Resam %>% 
  mutate(MD = purrr::pmap(.l = list(Escenaries, stations, names), .f = Gen_to_monthly, path_out = path_out, year_forecast = year_forecast) )

