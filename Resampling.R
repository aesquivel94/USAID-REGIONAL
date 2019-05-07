rm(list = ls()); gc(reset = TRUE)
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Made by:     Alejandra Esquivel Arias. 
# Created in:  Date: 4-2019.
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
library(jsonlite)
library(raster)
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
  
  
  
  # data %>% 
  #   mutate(date = mdy(glue::glue('{month}-{day}-{year}'))) %>% 
  #   filter(year == '2010')  %>% 
  #   ggplot(aes(date, precip)) + 
  #   geom_line() + 
  #   scale_x_date(date_labels = "%b-%d", breaks  = "1 month", 
  #                limits = c(as.Date('2010-01-01'), as.Date('2010-12-31'))) +
  #   labs(x = NULL, y = 'Precipitación (mm)') + 
  #   theme_bw()
  
  
  
  
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
  
  # jam <- Times %>% filter(row_number() == 1) %>% dplyr::select(-xi) %>% unnest %>% unnest 
  # quantile(jam$precip, c(0.33, 0.66))

  # jam %>% 
  #   arrange(year) %>% 
  #   ggplot(aes(year, precip)) +
  #   geom_line() + 
  #   scale_x_continuous(breaks = seq(1980,2013, 4), labels =  glue::glue('NDJ-{seq(1980,2013, 4)}') ) + 
  #   theme_bw()  +
  #   labs(x = NULL, y = 'Precipitación (mm)')  
  
  
  
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
  # library(trend)
 
  
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



# .------..------..------..------..------..------..------.
# |R.--. ||E.--. ||S.--. ||U.--. ||L.--. ||T.--. ||S.--. |
# | :(): || (\/) || :/\: || (\/) || :/\: || :/\: || :/\: |
# | ()() || :\/: || :\/: || :\/: || (__) || (__) || :\/: |
# | '--'R|| '--'E|| '--'S|| '--'U|| '--'L|| '--'T|| '--'S|
# `------'`------'`------'`------'`------'`------'`------'



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

# =-=-=-=-=-=-=-=-= Reading all data sets. 
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


# =-=-=-=-=-=-=-= we are doing resampling for all data sets 
tictoc::tic()
Resam <- Initial_data %>% 
  mutate(Escenaries = purrr::map2(.x = stations, .y = CPT_prob, 
                                  .f = resampling, year_forecast = year_forecast))
tictoc::toc() # 16.55 -- less than one minute.



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


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-= 
purrr::map2(.x = Resam$names, .y = Resam$Escenaries, 
            .f = function_to_save, path_out = path_out)










# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Aquí hay que agregar todo lo de descarga de Chirps y de NASA Power... además agregar a Resam
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


#---------------------------------------------------------------------------------#
#-------------Function to extract NASA POWER daily data --------------------------#
#---------------------------------------------------------------------------------#


# INPUT
# lat: latitud de la estaciÃ³n/sitio de interÃ©s
# lon: longitud de la estaciÃ³n/sitio de interÃ©s
# year_to: actual year
# month_to: actual month

lat <- 4.53
lon <- -76.06	

# In this part we define year_to and month_to...
if (substring(Sys.Date(),6,7) == "01"){
  year_to = as.numeric(format(Sys.Date(),"%Y"))-1
  month_to = 12
} else {
  year_to = format(Sys.Date(),"%Y")
  month_to = as.numeric(format(Sys.Date(),"%m"))-1
}


data <- Cerete
special_data <- tibble(lat, lon, year_to, month_to)

rm(lon, lat, month_to)


# =-=-=-=-=
# Testing this parth 
# data_d <- Cerete

# It could be possible NASA API in some case some times don't work. 
download_data_nasa <- function(data, special_data){
  # data <- Cerete
  # special_data <- tibble(lat, lon, year_to, month_to)
  
  lat <- special_data$lat
  lon <- special_data$lon
  year_to <- special_data$year_to
  month_to <- special_data$month_to
  
  
  json_file <- paste0("https://power.larc.nasa.gov/cgi-bin/v1/DataAccess.py?&request=execute&identifier=SinglePoint&parameters=ALLSKY_SFC_SW_DWN,T2M_MAX,T2M_MIN&startDate=19830101&endDate=",format(Sys.Date(),"%Y%m%d"),"&userCommunity=AG&tempAverage=DAILY&outputList=ASCII&lat=",lat,"&lon=",lon)
  # Esta mostrando un error que no conozco.
  json_data <- jsonlite::fromJSON(json_file)
  
  
  data_nasa <-  tibble(dates = seq(as.Date("1983/1/1"), as.Date(format(Sys.Date(),"%Y/%m/%d")), "days")) %>%  
    mutate(year_n = year(dates), month = month(dates), day = day(dates),
           tmin = json_data$features$properties$parameter$T2M_MIN %>% unlist, 
           tmax = json_data$features$properties$parameter$T2M_MAX %>% unlist, 
           srad = json_data$features$properties$parameter$ALLSKY_SFC_SW_DWN %>% unlist) %>% 
    na_if(-99)
  
  
  
  # Join observed and NASA data. 
  all_data <- right_join( data %>% 
                            filter(year %in% unique(data_nasa$year_n) ) %>% dplyr::select(-precip),
                          data_nasa %>% 
                            filter(year_n %in% unique(data$year)) %>% 
                            set_names('dates', 'year', 'month', 'day', 'tmin_N', 'tmax_N', 'srad_N'))
  
  
  # Bias between observed data and NASA data. 
  mean_less <- all_data %>% 
    summarise(mean_max = mean(tmax-tmax_N, na.rm = TRUE), 
              mean_min = mean(tmin - tmin_N, na.rm = TRUE),
              mean_srad = mean(srad - srad_N, na.rm = TRUE))
  
  # data full with mean NASA. 
  nasa_data_dw <- data_nasa %>% 
    filter(year_n == year_to, month == month_to) %>% 
    mutate(tmax = tmax + pull(mean_less, mean_max), 
           tmin = tmin + pull(mean_less, mean_min),
           srad = srad + pull(mean_less, mean_srad)) %>% 
    mutate(tmax = ifelse(is.na(tmax), mean(tmax, na.rm = TRUE), tmax),
           tmin = ifelse(is.na(tmin), mean(tmin, na.rm = TRUE), tmin),
           srad = ifelse(is.na(srad), mean(srad, na.rm = TRUE), srad))
  
  return(nasa_data_dw)}


tictoc::tic()
nasa_data <- download_data_nasa(Cerete, special_data)
tictoc::toc()



#---------------------------------------------------------------------------------#
#-------------- Function to extract Chirp daily data. ----------------------------#
#---------------------------------------------------------------------------------#


# Por ahora no modificar. 
numberOfDays <- function(date) {
  m <- format(date, format="%m")
  
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  
  return(as.integer(format(date - 1, format="%d")))
}


# Creo que esto es para arreglar diciembre...
if (substring(Sys.Date(),6,7) == "01"){
  substr_month <- "12"
  substr_year <- year(Sys.Date()) - 1
} else {
  substr_month <- str_pad(as.numeric(substring(Sys.Date(),6,7))-1,2,pad = "0")
  substr_year <- year(Sys.Date())
}



ini.date <- paste0(substr_year,"-",substr_month,"-01") %>%  as.Date()
# .... 
end.date <- paste0(substr_year,"-",substr_month,"-",numberOfDays(ini.date)) %>% as.Date()

outDir <- 'C:/Users/AESQUIVEL/Desktop/Resampling/Chirps'

download_data_chirp <- function(ini.date, end.date, year_to, outDir){
  
  fechas <- seq(as.Date(ini.date), as.Date(end.date), "days") %>% str_replace_all("-", ".")  
  
  urls <- paste("ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRP/daily/",year_to,"/chirp.",fechas,".tif",sep="")
  file <- basename(urls)
  outDir_all <- paste0(outDir,"/",file)
  
  tictoc::tic()
  # download.file(url = urls, destfile = outDir_all)
  purrr::map2(.x = urls, .y = outDir_all, .f = download.file)
  tictoc::toc() # 84.12 seg.
  
  return("Datos de CHIRPS descargados!") }


tictoc::tic()
download_data_chirp(ini.date, end.date, year_to, outDir)
tictoc::toc() # 6.8 min



# También hay que revisar la lectura del archivo de datos... preguntarle a ed.

testing <- list.files(outDir, full.names = TRUE) %>%
  stack

plot(testing)



lat <- 4.53
lon <- -76.06	

try_part <- raster::extract(testing, data.frame(x= 76.06,y= 4.53))

# Aqui debo arreglar esto.

jmmm <- try_part %>% 
  t() %>% 
  as_tibble() %>% 
  set_names('precip') %>% 
  mutate(names = list.files(outDir) %>% str_remove('.tif'), 
         day_name = names %>% str_remove(glue::glue('chirp.2019.04.')), 
         day = as.numeric(day_name)) %>% 
  dplyr::select(-names)




jmmm
nasa_data



final_month <- right_join(jmmm, nasa_data) %>% 
  dplyr::select(day, month, year_n, precip, tmax, tmin, srad) %>% 
  rename(year = 'year_n')















