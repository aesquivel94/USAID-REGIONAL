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
  
  
  
  all_seasons <- all_seasons %>% 
    filter(str_detect(x, as.character(season[1,])) | 
           str_detect(x, as.character(season[2,]) ) ) 
  
  
  return(all_seasons)}
  
  

  Times <- season_to_months(Times)
  
  
  
  xi <- Times[1,]
  
  proof <- data %>% 
    filter(between(month, xi$start_month, xi$end_month)) %>% 
    group_by(year) %>% 
    summarise(precip = sum(precip)) %>% 
    arrange(desc(precip)) 
  
  
  Prob <- CPT_prob %>% 
    filter(Season == xi$x)
  
   
   Type_cat <- tibble( id = 1:100) %>% 
     mutate(sample_cat = purrr::map(.x = id, .f = function(.x){
       sample_n(Prob,  size = 1, weight = Prob) }))
   
   
   
   Type_cat %>% 
     filter(row_number() == 1) %>% 
     unnest
   
  # =-=-=-=-=-=-=-=-=-=-= 
   
   quantile <- quantile(proof$precip, probs = c(0.33, 0.66))
   
  
   proof %>% 
     mutate(condtion = case_when(
       precip < quantile[1] ~  'Below',
       precip > quantile[2] ~ 'Above' ,
       TRUE ~ 'Normal'
     )  ) %>% 
     nest(-condtion)
   
  # =-=-=-=-=-=-=-=-=-=-= 
   
   
     
}








