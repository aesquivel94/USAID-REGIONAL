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
  data <- data %>%
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
# SST sample file. 

# This function do the row sample 
do_sample <- function(data, percent){
  # 1. First step do the function by one layer... 
  # Example for one layer
  # data <- stack_l  %>%
  #   filter(row_number() == 1) %>%
  #   select(new_data) %>%
  #   unnest
  
  # 2. Which rows are different to NA data. 
  try_do_this <- which(!is.na(data))
  
  # 3. Sampling rows. 
  rows <- sample(try_do_this, length(try_do_this)*percent)
  
return(rows)}


# Building one sample layer. 
one_spread_layer <- function(data, row_positions){
  # 1. First step do the function by one layer... 
  # Example for one layer
  # data <- SST_by_id  %>%
  #   filter(row_number() == 1) %>%
  #   select(new_data) %>%
  #   unnest
  
  # In this part we do the sampling and spread the date for do one layer. 
  spread_layer <- data %>% 
    mutate(condition = ifelse(row_number() %in% row_positions, SST, NA)) %>% 
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


## =-=-=-=-=-=-
# Create SST's CPT file, for that we need 3 functions. 


## This function do inctial rows (URL, field, first year), and save the first layer. 
write_cpt_head <- function(x, file){
  
  # x <- SST_sample
  x <- dplyr::select(x, -id, -date) %>% 
    filter(row_number() == 1) %>% 
    unnest
  
  sink(file = file)
  cat('xmlns:cpt=http://iri.columbia.edu/CPT/v10/', sep = '\n')
  cat('cpt:nfields=1', sep = '\n')
  cat('cpt:field=ssta, cpt:T=1982-03/05, cpt:nrow=61, cpt:ncol=360, cpt:row=Y, cpt:col=X, cpt:units=Kelvin_scale, cpt:missing=-999', sep = '\n')
  cat(write.table(x, sep = '\t', col.names = FALSE, row.names = FALSE, na = ""))
  sink()
  
}

## This function open the last file and save the following layers with its own dates and save final file. 
write_cpt_body <- function(x, file){
  
  # x <- SST_sample
  x <- dplyr::select(x, -id) %>%
    filter(row_number() > 1) %>%
    unnest()
  
  x <- split(x,x$date)
  year <- names(x)
  
  purrr::map2(.x = x, .y = year, .f = function(.x, .y){
    
    .x <- dplyr::select(.x, -date)
    sink(file = file, append = T)
    cat(glue('cpt:T={.y}'), sep = '\n')
    cat(write.table(.x, sep = '\t', col.names = FALSE, row.names = FALSE, na = ""))
    sink()
  })
  
}

##  This funtion combine write_cpt head  and body and save the final file. 
write_cpt <- function(x, file){
  
  write_cpt_head(x, file)
  write_cpt_body(x, file)
}





# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= 
# Section 3. 
# Run CPT from R console (in this case for ERSST). 


# x <- SST file with path. 
# y <- Stations file with path. 
# i_fores <- targed season... (really i'm not sure about this parameter). 
# path_run <-  path where the CPT executable is saved.
# path_out <-  path where we will save the results after run CPT. 


# despues de echo 9 y 1
# echo 532
# echo 1982
# echo 2015
# echo N
# echo 2

run_cpt_basic <- function(x, run,  y,  i_fores,  path_run,  path_out){
  
  GI <- paste0(path_out,"GI", run,".txt")
  prob <- paste0(path_out,"prob", run,".txt")
  
  cmd <- "@echo off
  
  (
  echo 611
  echo 1
  echo %path_x% 
  echo 30
  echo -30
  echo 0
  echo 359
  echo 1
  echo 10 
  echo 2
  echo %path_y%
  echo %i_for% 
  echo 3
  echo 3
  echo 17
  echo 13
  echo -90
  echo -83
  echo 1
  echo 10
  echo 1
  echo 5
  echo 9
  echo 1
  echo 7
  echo 34
  echo 554
  echo 2
  echo 541
  echo 112
  echo %path_GI%
  echo 311
  echo 451
  echo 452
  echo 454
  echo 455
  echo 111
  echo 501
  echo %path_prob%
  echo 0
  echo 0
  ) | CPT_batch.exe"


cmd<-gsub("%path_x%",x,cmd)
cmd<-gsub("%path_y%",y,cmd)
cmd<-gsub("%path_GI%",GI,cmd)
cmd<-gsub("%path_prob%",prob,cmd)

cmd<-gsub("%i_for%",i_fores,cmd)

write(cmd, path_run)
system(path_run, ignore.stdout = T, show.output.on.console = T)

#  if(file.exists(paths = paste0(path_run, "text.bat")) == TRUE){
#    file.remove(paste0(path_run, "text.bat")) }

}




# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Section 4. Run CPT 100 times... with diferent files. 
# =-=-=

# This function save one SST sample file. 
masive_runs <- function( run, data_by_id, path){
  
  # data_by_id <- SST_by_id
  
  row_positions <- data_by_id %>%
    dplyr::select(new_data) %>%
    filter(row_number() == 1) %>%
    unnest %>%
    do_sample(., percent = percent)
  
  
  SST_sample <- data_by_id %>%
    mutate(spread_data = purrr::map(.x = new_data, .f = one_spread_layer, row_positions))  %>%
    dplyr::select(-new_data)
  
  
  write_cpt(x = SST_sample, file = paste0(path, 'run_', as.numeric(run) ,'.txt') ) 
}


# This function runs with only one y file... if we have run with several y,  we will need modify it. 
run_cpt_sample <- function(run, y, data_by_id, path, i_fores){
  
  path_out <- paste0(path, 'GI_runs/')
  
  if(dir.exists(path_out) == FALSE){dir.create(path_out)}else{print('ok')}
  
  purrr::map(run, .f = masive_runs, data_by_id = data_by_id)
  
  x <- list.files(path = path,  pattern = '.txt$', full.names = TRUE) 
  
  path_run <- paste0(path ,'test.bat')
  
  purrr::map2(.x = x, .y = run, .f = run_cpt_basic, y = y, i_fores = i_fores, path_run = path_run ,  path_out = path_out)
}





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
tictoc::toc() # 5.65 sec.





# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Section 2. Sampling SST and convert to CPT format.  
# =-=-=


# =-=-=-=-=-=-=-=
# First step to do Resampling and CPT files. 

# i guess is necesary to create a path_out folder and clean the files... 
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


# row_positions <- SST_by_id %>% 
#   dplyr::select(new_data) %>% 
#   filter(row_number() == 1) %>% 
#   unnest %>% 
#   do_sample(., percent = percent)
# 
#   
# tictoc::tic()
# SST_sample <- SST_by_id %>% 
#   mutate(spread_data = purrr::map(.x = new_data, .f = one_spread_layer, row_positions))  %>% 
#   dplyr::select(-new_data)
# tictoc::toc() # 10.39 
# 
# write_cpt(x = SST_sample, file = 'SST_runs/run_1.txt')




# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Section 3. Run CPT by one layer. 
# =-=-=

# x <- 'C:/Users/aesquivel/Desktop/USAID-Regional/USAID-REGIONAL/SST_runs/run_1.txt'
# y <- 'C:/Users/aesquivel/Desktop/USAID-Regional/USAID-REGIONAL/honduras_chirps_data.txt'
# path_out <- 'C:/Users/aesquivel/Desktop/USAID-Regional/USAID-REGIONAL/SST_runs/GI_runs/'
# path_run <- 'C:/Users/aesquivel/Desktop/USAID-Regional/USAID-REGIONAL/SST_runs/test.bat'
# i_fores <- 3
# 
# run_cpt_basic(x,  y,  i_fores,  path_run,  path_out, run )



# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Section 4. Run CPT 100 times... with diferent files. 
# =-=-=

y <- 'C:/Users/aesquivel/Desktop/USAID-Regional/USAID-REGIONAL/honduras_chirps_data.txt'
path <- 'C:/Users/aesquivel/Desktop/USAID-Regional/USAID-REGIONAL/SST_runs/'

tictoc::tic()
run_cpt_sample(run = 1:5, y = y, data_by_id = SST_by_id, path = path, i_fores = 3) 
tictoc::toc()



# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Section 5. Reading G.I. files and construct histogram. 
# =-=-=


list.files(path = paste0(path, 'GI_runs'), pattern = 'GI', full.names =  TRUE)[1]







GI1 <- read_table2("SST_runs/GI_runs/GI1.txt", skip = 5) %>% 
  tail(n = 1L) %>% 
  .[, -(1:4)]




