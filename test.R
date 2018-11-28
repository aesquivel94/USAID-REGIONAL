# rm(list = ls()); gc()

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Made by:     Alejandra Esquivel Arias. 
# Created in:  Date: 28-11-2018
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# CPT predictor area optimization - Resampling.
#
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Packages. 
library(tidyverse)
library(viridis)
library(tictoc)
library(glue)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# main directory path and setwd it.
# path <- 'C:/Users/aesquivel/Desktop/USAID-Regional/CPT_R'
# setwd(path)


# =-=-=-=-=-=-=-=-=-=-=-= Functions.


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= 
# Section 1. 
# By one year, this function organize data set like Lat, Long and SST.

add_long <- function(data){
  data <- SST_filter %>%
    filter(row_number() %in% SST_length[1]:(SST_length[2]-1))
  
  Long <- data %>%
    filter(row_number() == 1) %>% 
    t() %>% 
    .[-1]
  
   data <- data %>%
    filter(row_number() > 1) %>% 
    tbl_df() %>%
    setNames(., c('Lat', Long)) %>%
    gather(Long, SST ,-Lat) %>%
    mutate_if(is.character, as.numeric) %>%
    mutate(Lat = as.numeric(paste(Lat))) %>%
    mutate(SST = ifelse(SST == -999, NA, SST))
  
  return(data)}


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= 
# Section 2. 

# Building one sample layer. 
one_spread_layer <- function(data, percent){
  # 1. First step do the function by one layer... 
  # Example for one layer
  # data <- SST_by_id  %>%
  #   filter(row_number() == 1) %>%
  #   select(new_data) %>%
  #   unnest

  # 2. Which rows are different to NA data. 
  try_do_this <- which(!is.na(data))
  
  # 3. Sampling rows. 
  rows <- sample(try_do_this, length(try_do_this)*percent)
  
  # In this part we do the sampling and spread the date for do one layer. 
  spread_layer <- data %>% 
    mutate(condition = ifelse(row_number() %in% rows, SST, NA)) %>% 
    mutate(condition = ifelse(is.na(condition), -999, condition)) %>% 
    dplyr::select(-SST) %>% 
    spread(Long, condition)  %>%
    arrange(., sort(Lat, decreasing = T)) 
  
  # Extract Longitude from the original data
  Long  <- SST_filter %>%
    filter(row_number() == 1) %>% 
    setNames(names(spread_layer)) %>%
    mutate_if(is.factor, as.character) %>% 
    mutate_if(is.character, as.numeric)
  
  # add like first row Long in spread_SST
  spread_layer <-  spread_layer %>% 
    rbind(Long, .)
  
return(spread_layer)}


# Create SST's CPT file. 









# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Section 1. Reading original SST data set. 

# Read SST...
SST <- read.table("Feb_Mar-Apr-May.tsv",
                  sep="\t",  dec=".", skip =2, 
                  fill=TRUE,na.strings =-999)

# filter dates and rows contains cpt:field...
SST_filter <- SST %>%
  filter(row_number() != 1) %>% 
  filter(!grepl("cpt:field", V1)) 

# Compute Length for one layer (year-raster).  
SST_length <- which(SST_filter$V1 == '')[1:2]

# This object indicate which rows have the first position like white space. 
# partition <- which(SST_filter$V1 == '')

# SST transform original format to tibble (with gather).      
tictoc::tic()
SST_by_id <- SST_filter %>%
  tbl_df()  %>%
  mutate(id = rep(1982:2015, each = SST_length[2] - SST_length[1])) %>% 
  nest(-id) %>%
  mutate(new_data = purrr::map(.x = data, .f = add_long)) %>%
  select(-data)
tictoc::toc() # 4.57 sec.


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Section 2. Sampling SST and convert to CPT format.  
# =-=-= Proof...


# =-=-=-=-=-=-=-=
# First step to do Resampling and CPT files. 

# i guess is necesary to create a Output folder and clean the files... 
# The other thing is necesary errase that files. 
# One posibility is run CPT just after create one file. 
# But in this moment isn't necesary that... it's most important in secction 3. 

percent <- list.files(pattern = '[0-9].txt$') %>%
  str_split(., '_0.') %>% 
  unlist %>% 
  .[2] %>% 
  str_remove('.txt') %>%
  as.numeric() 
             
percent <- 1 - (percent/10)         

# extract dates in CPT format 
CPT_dates <- SST[1, -1][which(!is.na(SST[1, -1]))] %>%  
  t() %>% 
  as.tibble() %>% 
  set_names('date') %>% 
  mutate(id = seq(1982,2015,1))


# Paste real dates from CPT
SST_by_id <- SST_by_id %>% 
  right_join(CPT_dates, ., by = 'id')
  
  
  

tictoc::tic()
test_row_1 <- SST_by_id %>% 
  mutate(spread_data = purrr::map(.x = new_data, .f = one_spread_layer, percent))  %>% 
  dplyr::select(-new_data)
tictoc::toc() # 11.09 


# test_row_1 %>% 
#   filter(row_number() == 1) %>% 
#   dplyr::select(spread_data) %>% 
#   unnest %>% 
#   View




# cpt_field <- SST %>%
#   filter(grepl("cpt:field", V1))  


## x without year
write_cpt_head <- function(x, file){
  
  # x <- test_row_1
  x <- dplyr::select(x, -id, -date) %>% 
    filter(row_number() == 1) %>% 
    unnest
  
  sink(file = "p.txt")
  cat('xmlns:cpt=http://iri.columbia.edu/CPT/v10/', sep = '\n')
  cat('cpt:nfields=1', sep = '\n')
  cat('cpt:field=ssta, cpt:T=1982-03/05, cpt:nrow=61, cpt:ncol=360, cpt:row=Y, cpt:col=X, cpt:units=Kelvin_scale, cpt:missing=-999', sep = '\n')
  cat(write.table(x, sep = '\t', col.names = FALSE, row.names = FALSE, na = ""))
  sink()
   
}

write_cpt_body <- function(x, file){
  
  # x <- test_row_1
  x <- dplyr::select(x, -id) %>%
    filter(row_number() > 1) %>%
    unnest()
  
  x <- split(x,x$date)
  year <- names(x)
  
  purrr::map2(.x = x, .y = year, .f = function(.x, .y){
    
    .x <- dplyr::select(.x, -date)
    sink(file = "p.txt", append = T)
    cat(glue('cpt:T={.y}'), sep = '\n')
    cat(write.table(.x, sep = '\t', col.names = FALSE, row.names = FALSE, na = ""))
    sink()
  })
 
}

write_cpt <- function(x, file){
  
  write_cpt_head(x, file)
  write_cpt_body(x, file)
}

write_cpt(x = test_row_1, file = 'p.txt')




