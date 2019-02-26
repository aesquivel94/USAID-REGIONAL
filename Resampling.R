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


resampling <-  function(data, CPT_prob){
 
  data <- Cerete
  
  CPT_prob <- Prob %>% 
    .[,c(1, 4)] %>% 
    set_names('NDJ', 'FMA') %>%
    mutate(Type = c('Below', 'Normal', 'Above')) %>% 
    gather(Season, Prob, -Type)
  
  
  
  Times <- CPT_prob %>% 
    dplyr::select(Season) %>% 
    unique

    
  month.abb
  str_sub(month.abb, 1, 1)

  
  # x <- paste0(str_sub(month.abb, 1, 1), lead(str_sub(month.abb, 1, 1)),
  #                     lead(lead(str_sub(month.abb, 1, 1), n = 1))) 
  
  x <- case_when(x == 'NDNA' ~ 'NDJ', 
            x == 'DNANA' ~ 'DJF', 
            TRUE ~ as.character(x))
  
  
  
  season_to_months <-  function(season){
   
    season <- CPT_prob %>% 
      nest(-Season) %>% 
      dplyr::select(Season)
    
    
  all_seasons <-  paste0(str_sub(month.abb, 1, 1), lead(str_sub(month.abb, 1, 1)),
                lead(lead(str_sub(month.abb, 1, 1), n = 1))) %>% 
     tibble(x = . ) %>% 
     mutate(x = case_when(x == 'NDNA' ~ 'NDJ', 
               x == 'DNANA' ~ 'DJF', 
               TRUE ~ as.character(x)) ) %>% 
     mutate(start_month = 1:12, end_month = c(3:12, 1, 2))
  
  
  
  all_seasons %>% 
    filter(x %in%  season)
  
    
  }
  
  
  
  CPT_prob %>% 
    # nest(-Season) %>% 
    # mutate(data_filter = )
  
  
  
  
  
  # data   
  
  
  
 # do_one_season <- function(season, prob, data){}
  
  
  
  
}








