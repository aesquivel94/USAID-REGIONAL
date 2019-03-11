rm(list = ls()); gc(reset = TRUE)
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Made by:     Alejandra Esquivel Arias. 
# Created in:  Date: 3-2019.
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Resampling Methodology 

# =-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-
# Packages. 
# =-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-
library(tidyverse)
library(viridis)
library(tictoc)
library(glue)
library(lubridate)
library(cowsay)
library(skimr)
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
do_organize_data <- function(Season, xi, data){
  
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
  
  # Clasificación de las series mensuales... 
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
  
  # Hace el re muestreo de las categorías...
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
day_sample <- function(Season, cat, data){
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



 



# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=
###  Run from here results.


# Read daily data: 
Cerete <- readr::read_csv("Resampling/daily_preliminar/Cerete.csv")
Prob <- readr::read_csv("Resampling/CPT_prob/Cerete1_prob_precip.csv")

# year ...  necesito que identifique el sistema si el año es bisiesto. 
year_forecast <- Sys.Date() %>% year()


CPT_prob <- Prob %>%
  .[,c(1, 4)] %>%
  set_names('NDJ', 'FMA') %>%
  mutate(Type = c('Below', 'Normal', 'Above')) %>%
  gather(Season, Prob, -Type)


# Organizar desde esta parte... 


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=
# 4. Resampling
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=
resampling <-  function(data, CPT_prob, year_forecast){
  
  
  # data <- Cerete # Datos de estaciones a nivel diario. 
  # 
  # # Datos de probabilidad cambiando a  Type - Season -  Prob
  # CPT_prob <- Prob %>% 
  #   .[,c(1, 4)] %>% 
  #   set_names('NDJ', 'FMA') %>%
  #   mutate(Type = c('Below', 'Normal', 'Above')) %>% 
  #   gather(Season, Prob, -Type)
  
  
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
                                    .f = do_organize_data, data = data))
  
  # This function do the 100 category samples.   
  Times <- Times %>%
    mutate(cat = purrr::map(.x = xi,.f = sample_category))
  
  # =-=-=-=-=-=-=-=-
  # This function do the year sample depends of the sample category.
  Times <- Times %>% 
    mutate(cat = purrr::map2(.x = cat, .y = month_data, .f = year_function))
  
  # This function extract daily data using sample year.  
  daily_data <- Times %>% 
    mutate(daily_data = purrr::map2(.x = Season, .y = cat, .f = day_sample, data = data)) %>% 
    dplyr::select(Season, daily_data)
  
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=
  
  
  Escenaries <- daily_data %>% 
    unnest %>% 
    dplyr::select(-condtion) %>% 
    nest(-id) %>% 
    mutate(data = purrr::map(.x = data, .f = function(.x){ .x %>%  unnest %>%   unnest() %>%  dplyr::select(-Season) })) 
  
  
  # Escenaries %>% 
  #   mutate(try = purrr::map(.x = data, .f = function(.x){nrow(.x)} )) %>% 
  #   dplyr::select(-data) %>% 
  #   unnest %>% 
  #   View
  
  
  return(Escenaries)}





resampling(data = Cerete, CPT_prob = CPT_prob, 
           year_forecast = year_forecast)





