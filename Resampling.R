rm(list = ls()); gc(reset = TRUE)
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Made by:     Alejandra Esquivel Arias. 
# Created in:  Date: 2-2019
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


# Read daily data: 

Cerete <- readr::read_csv("Resampling/daily_preliminar/Cerete.csv")
Prob <- readr::read_csv("Resampling/CPT_prob/Cerete1_prob_precip.csv")

# year ...  necesito que identifique el sistema si el año es bisiesto. 
year_forecast <- Sys.Date() %>% year()


resampling <-  function(data, CPT_prob, year_forecast){
 
  
  data <- Cerete # Datos de estaciones a nivel diario. 
  
  # Datos de probabilidad cambiando a  Type - Season -  Prob
  CPT_prob <- Prob %>% 
    .[,c(1, 4)] %>% 
    set_names('NDJ', 'FMA') %>%
    mutate(Type = c('Below', 'Normal', 'Above')) %>% 
    gather(Season, Prob, -Type)
  
  
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  # Tratando de arreglar febrero...
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  
  # year_forecast <- 2016
  # Is a year a leap year?
  leap_year(year_forecast)
 
  # add <- data %>% 
  #   filter(month == 2) %>% 
  #   sample_n(size = 1) %>% 
  #   mutate(day = 29)
 
  
  # data %>% 
  #   add_row(.after = 59) %>% 
  #   View
 
  
  # February <- data %>% 
  #   filter(month == 2) %>% 
  #   mutate(condition = leap_year(year)) %>%
  #   nest(-condition, -year) %>% 
  #   mutate(length = purrr::map(.x = data, .f = function(.x){nrow(.x)})) %>% 
  #   unnest(length)
  
 
# Si ... length == 28 # Días incompletos.
  complete_month <- function(data_incomplete){
    # data_incomplete <- filter(February, row_number() == 2 ) %>% dplyr::select(data) %>% unnest
    data_incomplete <- bind_rows(data_incomplete, data_incomplete %>% sample_n(size = 1) %>% mutate(day = 29)) 
  return(data_incomplete)}
  
# Si ... length == 29 # Días completos
  incomplete_month <- function(data_complete){
    # data_complete <- filter(February, row_number() == 1 ) %>% dplyr::select(data) %>% unnest
    data_complete <- data_complete %>% slice(-n())
  return(data_complete)}
  
# Se usa cuando leap == FALSE
  intento_cordura <- function(to_change){
    # to_change <-  testing_Data %>%  filter(leap ==  FALSE) %>% dplyr::select(-leap) 
    
    Dato_C <- to_change %>%  
      nest(-year) %>% 
      mutate(data = purrr::map(.x = data, .f = complete_month)) %>% 
      unnest %>% 
      dplyr::select(day, month,  year, precip,  tmax,  tmin,  srad)
    return(Dato_C)}
  
# Se usa cuando leap == TRUE  
  estoy_loca <- function(to_change){
    # to_change <-  testing_Data %>%  filter(leap ==  TRUE) %>% dplyr::select(-leap) 
    
    Dato_C <- to_change %>% 
      nest(-year) %>% 
      mutate(data = purrr::map(.x = data, .f = incomplete_month)) %>% 
      unnest %>% 
      dplyr::select(day, month,  year, precip,  tmax,  tmin,  srad) 
    return(Dato_C)}
  
  
  
  # 
  
  data_P <- data %>% 
    mutate(month_P = month) %>% 
    nest(-month_P)

  
  # data_P[2,2] <- add

  
  year_forecast <- 2016
  
  # Organizar desde esta parte
  change_Leap <- function(leap, feb_data){
    # feb_data <- data_P %>% filter(month_P == 2) %>% dplyr::select(data) %>% unnest
    
    testing_Data <- feb_data %>% 
      mutate(leap = leap_year(year)) %>% 
      nest(-leap)
  
    if (leap == TRUE) {
      
      testing_Data <- testing_Data %>% 
        mutate(data = purrr::map_if(.x = data, .p = leap ==  TRUE, .f = estoy_loca))
      
    } else {
      
      testing_Data <- testing_Data %>% 
        mutate(data = purrr::map_if(.x = data, .p = leap ==  FALSE, .f = intento_cordura))

    }
  return(testing_Data) }
  
 
  

  
  

  
  
  
  

  
  
  
  
  
  
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= 
  
  
  
  
  # =-=-=-=-=  Do years 
  
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
  
  

  # =-=-=-=-=-=-=-=-=-=-=-=-=
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
  
  
  #  Aqui se extrae el mes de inicio y el final (esto se debe guardar). 
  # Times <- season_to_months(Times)
  
  
  Times <- CPT_prob %>% 
    nest(-Season) %>% 
    mutate(Times = purrr::map(.x = Season, .f = season_to_months)) %>% 
    unnest(Times) %>% 
    unnest() %>% 
    nest(-Season)
  
  
  # =-=-=-=-=-=-=-=-=-=-=-=-=
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
  
  

  Times <- Times %>% 
    rename(xi = 'data') %>% 
    mutate(month_data = purrr::map2(.x = Season, .y = xi, 
                                    .f = do_organize_data, data = data))
  

  # Aqui se hace el re-muestreo de las cateogrias... 
  sample_category <- function(Prob){
    # Prob <- Times %>%  filter(row_number() == 1) %>% dplyr::select(xi) %>% unnest()
 
    # Hace el re muestreo de las categorías...
    Type_cat <- tibble( id = 1:100) %>% 
      mutate(sample_cat = purrr::map(.x = id, .f = function(.x){
        sample_n(Prob,  size = 1, weight = Prob) }))
  
  return(Type_cat)}
  
  Times <- Times %>%
    mutate(cat = purrr::map(.x = xi,.f = sample_category))
  
  
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=
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
    
    
    Times <- Times %>% 
      mutate(cat = purrr::map2(.x = cat, .y = month_data, .f = year_function))
    


    
    
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=
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
    
    daily_data <- Times %>% 
      mutate(daily_data = purrr::map2(.x = Season, .y = cat, .f = day_sample, data = data)) %>% 
      dplyr::select(Season, daily_data)
    
    
    
    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=
    
    # daily_data %>% 
    #   unnest %>% 
    #   dplyr::select(-condtion) %>% 
    #   nest(-id) %>% 
    #   filter(row_number() < 2) %>%
    #   dplyr::select(-id)%>% 
    #   unnest %>% 
    #   unnest() %>% 
    #   dplyr::select(-Season) %>% 
    #   View

    
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








