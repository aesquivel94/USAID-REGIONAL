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
    slice(1,n()) %>% 
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
  
  
  
  
  # =-=-=-=-=-=-=-=
  library(trend)
  
  
  # do_trend <- function(Daily_filter){
  #   
  #   Tmax <- dplyr::select(Daily_filter, tmax)
  #   Tmx <- ts(data = Tmax, start = min(Tmax), end = max(Tmax))
  #   sen_Tmax <- sens.slope(Tmx)
  #   # t <- 1:length(Tmx)
  #   
  #   sen_Tmax$estimates * 1:length(Tmx)
  #   
  #   Tmin <- dplyr::select(Daily_filter, tmin) %>% data_frame() %>% as.vector()
  #   Tmn <- ts(data = Tmin, start = min(Tmin), end = max(Tmin))
  #   # sen_Tmin <- sens.slope(Tmin)
  #   # length(Tmn)
  # }
  
  
  # =-=-=-=-=-=-=-=
  
  # day_sample <- function(Season, cat, data, Intial_year, last_year){
  #   # data it's station data.  
  #   # cat <-  Times %>%  dplyr::select(cat) %>% filter(row_number() < 2) %>% unnest
  #   # Season <- 'NDJ'
  #   
  #   month_ini <- cat %>% 
  #     dplyr::select(start_month) %>% 
  #     unique() %>% 
  #     as.numeric()
  #   
  #   month_end <- cat %>% 
  #     dplyr::select(end_month) %>% 
  #     unique() %>% 
  #     as.numeric()
  #   
  #   # Filter by season data serie.
  #   if(Season == 'NDJ'){
  #     Daily_filter <- data %>%
  #       filter(month %in% c(11,12,1)) %>% 
  #       mutate(year_M = ifelse(month == 1, year, year+1)) %>% 
  #       filter(year_M >= (Intial_year + 1), year_M < (last_year + 1))%>%
  #       mutate(year = year_M - 1) %>% 
  #       dplyr::select(-year_M)
  #     
  #   } else if(Season == 'DJF'){
  #     Daily_filter <- data %>%
  #       filter(month %in% c(11,12,1)) %>% 
  #       mutate(year_M = ifelse(month == 1, year, year+1)) %>% 
  #       filter(year_M >= (Intial_year + 1), year_M < (last_year +1 ))%>%
  #       mutate(year = year_M - 1) %>% 
  #       dplyr::select(-year_M)
  #     
  #   } else{
  #     Daily_filter <-  data %>%
  #       filter(between(month, month_ini, month_end)) 
  #   }
  #   
  #   
  #   # Aqui hay que agregar la parte la función do_trend...
  #   Daily_filter
  #   
  # 
  #     Daily_data <- cat %>% 
  #     dplyr::select(-start_month, -end_month) %>% 
  #     mutate(daily_data = purrr::map(.x = year, .f = function(.x){
  #       Daily_filter %>% filter(year == .x)})) %>% 
  #     dplyr::select(-year)
  #   
  # }
  
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  
  
  
  
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
  
  
  Escenaries <-  data_to_esc %>% 
    dplyr::select(-Season) %>% 
    mutate(year = year_forecast) %>% 
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




# We are testing only for one file... but the idea it's have the other files 
# and put the results in a specific folder. 
# cerete <- resampling(data = Cerete, CPT_prob = CPT_prob, 
#            year_forecast = year_forecast)





# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

# Read daily data: 
Cerete <- readr::read_csv("Resampling/daily_preliminar/Cerete.csv")
Prob <- readr::read_csv("Resampling/CPT_prob/Cerete1_prob_precip.csv")

# year ...  necesito que identifique el sistema si el año es bisiesto. 
year_forecast <- Sys.Date() %>% year()




# Read daily data:
Path_stations <- 'Resampling/daily_preliminar'
Path_Prob <- 'Resampling/CPT_prob'

# =-=-=-=-=-=-=-=-=
Initial_data <- tibble(names = list.files(Path_stations) %>% str_remove('.csv'), 
       path_stations = Path_stations %>% list.files(full.names = TRUE), 
       CPT_path = Path_Prob %>% list.files(full.names = TRUE)) %>% 
  mutate(stations = purrr::map(.x = path_stations, .f =  readr::read_csv),
         CPT_prob = purrr::map(.x = CPT_path, .f =  readr::read_csv)) %>% 
  dplyr::select(-path_stations, -CPT_path) 



#  Transformar esta parte es posible que se necesite cambiarla... dependiendo de la reunion. 
Initial_data <- Initial_data %>% 
  mutate(CPT_prob = purrr::map(.x = CPT_prob, .f = function(.x){
    Prov <- .x %>% .[,c(1, 4)] %>% set_names('NDJ', 'FMA') %>%
      mutate(Type = c('Below', 'Normal', 'Above')) %>% gather(Season, Prob, -Type)
  return(Prov)})) 


# =-=-=-=-=-=-=-=

tictoc::tic()
Resam <- Initial_data %>% 
  mutate(Escenaries = purrr::map2(.x = stations, .y = CPT_prob, 
                                  .f = resampling, year_forecast = year_forecast))
tictoc::toc() # 28.82 -- less than one minute.


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Aquí hay que agregar todo lo de descarga de Chirps y de NASA Power... además agregar a Resam
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Funcion de Chirps. 


# Funcion de NASA Power.

# También hay que revisar la lectura del archivo de datos... preguntarle a ed.

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Aqui hay que poner los guardados extra de 
path_out <- 'D:/OneDrive - CGIAR/Desktop/USAID-Regional/USAID-REGIONAL/Resampling/results/'


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=
function_to_save <- function(station, Esc_all, path_out){
  
  # data <- Resam %>%  filter(row_number() == 1) %>%  dplyr::select(names, Escenaries)
  # station <- data %>% dplyr::select(names)
  # Esc_all <- data %>% dplyr::select(Escenaries) %>% unnest
  
  # Escenarios diarios (generados con el remuestreo)
  Escenaries <- Esc_all %>%
    dplyr::select(data) %>% 
    unnest
  
  Esc_C <- Escenaries %>% 
    mutate(file_name = glue::glue('{path_out}{station}/escenario_{id}.csv')) 
  
  # Creacion del folder de datos... (donde se van a guardar los resultados). 
  ifelse(dir.exists(glue::glue('{path_out}{station}')) == FALSE, 
         dir.create(glue::glue('{path_out}{station}')), 'ok')

  # Guarda los escenarios diarios. 
  walk2(.x = Esc_C$data, .y = Esc_C$file_name, 
                      .f = function(.x, .y){ write_csv(x = .x, path = .y)})
  
  
  # Guardado de escenarios tipo. 
  
  Type_Esc <- Esc_all %>% 
    dplyr::select(Esc_Type) %>% 
    unnest %>% 
    mutate(file_name = glue::glue('{path_out}{station}/escenario_{Type}.csv'))
  
  walk2(.x = Type_Esc$data, .y = Type_Esc$file_name, 
        .f = function(.x, .y){ write_csv(x = .x, path = .y)})
  
  
  # Guardado de los years del remuestreo.
  Esc_all %>% 
    dplyr::select(Base_years) %>% 
    unnest %>% 
    write_csv(., path = glue::glue('{path_out}{station}/Escenario_A.csv'))
  

  
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  Levels <- Esc_C %>% 
    dplyr::select(data) %>% 
    unnest %>% 
    dplyr::select(month) %>% 
    unique 
  
  
  

  summaries <- Esc_C %>% 
    dplyr::select(id, data) %>% 
    unnest %>% 
    # mutate(month = x = fct_recode(month, Levels)) %>% 
    group_by(id, month, year) %>% 
    summarise(precip = sum(precip), tmax = mean(tmax), tmin = mean(tmin), srad = mean(srad)) %>% 
    ungroup() %>% 
    dplyr::select(-id) %>%
    group_by(month) %>%
      group_by(month, year) %>%
      summarise(prec_avg = mean(precip), prec_max = max(precip), prec_min = min(precip),
                sol_rad_avg = mean(srad), sol_rad_max = max(srad), sol_rad_min = min(srad),
                t_max_avg = mean(tmax), t_max_max = max(tmax), t_max_min = min(tmax),
                t_min_avg = mean(tmin), t_min_max = max(tmin), t_min_min = min(tmin)) %>%
    ungroup()
    
    
  
  summaries %>% 
    mutate(month = as.factor(month)) %>% 
    dplyr::select(month) %>% 
    fct_infreq() %>%
    
  
  
    
    # %>% 
    # arrange(., month = Levels)
      # gather(variable, value, -month, -year) %>%
      
      
    
  
  
  arrange(summaries, month, .by_group = Levels)
  


  # walk2(.x = summaries$data, .y = summaries$file_name,
  #       .f = function(.x, .y){write_csv(x = .x, path = .y)})
  
}


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-= 
purrr::map2(.x = Resam$names, .y = Resam$Escenaries, 
            .f = function_to_save, path_out = path_out)









