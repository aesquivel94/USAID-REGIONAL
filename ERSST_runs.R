rm(list = ls()); gc(reset = TRUE)
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Made by:     Alejandra Esquivel Arias. 
# Created in:  Date: 1-2019
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# CPT predictor area optimization - Resampling.


library(tidyverse)
library(viridis)
library(tictoc)
library(glue)

year_f <- 2015
all_runs <- 100



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
write_cpt_head_ER <- function(x, file){
  
  year <- dplyr::select(x, date)  %>% 
    filter(row_number() == 1) %>% 
    as.character()
  
  # x <- SST_sample
  x <- dplyr::select(x, -id, -date) %>% 
    filter(row_number() == 1) %>% 
    unnest
  
  sink(file = file)
  cat('xmlns:cpt=http://iri.columbia.edu/CPT/v10/', sep = '\n')
  # cat('xmlns:cf=http://cf-pcmdi.llnl.gov/documents/cf-conventions/1.4/', sep = '\n')
  cat('cpt:nfields=1', sep = '\n')
  # cat('cpt:field=ssta, cpt:T=1982-03/05, cpt:nrow=31, cpt:ncol=180, cpt:row=Y, cpt:col=X, cpt:units=Celsius_scale, cpt:missing=-999', sep = '\n')
  cat(glue('cpt:field=ssta, cpt:T={year}, cpt:nrow=31, cpt:ncol=180, cpt:row=Y, cpt:col=X, cpt:units=Celsius_scale, cpt:missing=-999'), sep = '\n')
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
  
  write_cpt_head_ER(x, file)
  write_cpt_body(x, file)
}





# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= 
# Section 3. 
# Run CPT from R console (in this case for ERSST). 


# =-
# This function runs it's for run in  a Linux server CPT. 

run_cpt_linux_V <- function(x, run,  y,  path_out){
  
  GI <- paste0(path_out,"GI", run,".txt")
  # prob <- paste0(path_out,"prob", run,".txt")
  
  cmd <- "@echo off
  
  (
  echo 611
  echo 545
  echo 1
  echo %path_x% 
  echo /
  echo /
  echo /
  echo /
  echo 1
  echo 10
  echo 2
  echo %path_y%
  echo /
  echo /
  echo /
  echo /
  echo 1
  echo 10
  echo 1
  echo 5
  echo 9
  echo 1
  echo 532
  echo /
  echo /
  echo N
  echo 2
  echo 554
  echo 2
  echo 541
  echo 112  
  echo %path_GI%
  echo 311
  echo 0
  echo 0
  ) | ./CPT.x"

  
  cmd<-gsub("%path_x%",x,cmd)
  cmd<-gsub("%path_y%",y,cmd)
  cmd<-gsub("%path_GI%",GI,cmd)
  
  path_run <- paste0(path_out, 'O_run_', run, '.sh')
  
  write(cmd, path_run)
  system(paste0( "sh ", path_run))
  
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
  
  
  write_cpt(x = SST_sample, file = paste0(path, '/o_run_', as.numeric(run) ,'.txt') ) 
}


# This function runs with only one y file... if we have run with several y,  we will need modify it. 
run_cpt_sample_linux <- function(run, y, data_by_id, path){
  
  path_out <- paste0(path, '/GI_runs/')
  
  if(dir.exists(path_out) == FALSE){dir.create(path_out)}else{print('ok')}
  
  purrr::map(run, .f = masive_runs, data_by_id = data_by_id, path = path)
  
  x <- list.files(path = path,  pattern = '.txt$', full.names = TRUE) 
  
  # Modificar run_cpt_linux_V
  purrr::map2(.x = x, .y = run, .f = run_cpt_linux_V, y = y,   path_out = path_out)
}


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Section 5. Reading G.I. files and construct histogram. 
# =-=-=

#  This function do a table with de GI  - generated by cross validation. 
GI_read <- function(route){
  GI <- read_table2(route , skip = 5) %>% 
    tail(n = 1L) %>% 
    .[, -(1:4)] 
  return(GI)}




run_by_rsample <- function(path_SST, percent, y, path){
  
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  # Section 1. Reading original SST data set. 
  percent <- as.numeric(percent)
  print(percent)
  # Read SST...
  # SST <- read.table(data_GI$path_SST[3], sep="\t",  dec=".", skip =3, fill=TRUE,na.strings =-999)
  SST <- read.table(path_SST, sep="\t",  dec=".", skip =3, fill=TRUE,na.strings =-999)
  
  #   # filter dates and rows contains cpt:field...
  SST_filter <- SST %>%
    filter(row_number() != 1) %>%
    filter(!grepl("cpt:field", V1))
  
  # Compute Length for one layer (year-raster).
  SST_length <- which(SST_filter$V1 == '')[1:2]
  
  
  # extract dates in CPT format
  CPT_dates <- SST[1, -1][which(!is.na(SST[1, -1]))] %>%
    t() %>%
    as.tibble() %>%
    set_names('date') %>%
    mutate(id = seq(1982,year_f,1))
  
  # tictoc::tic()
  SST_by_id <- SST_filter %>%
    tbl_df()  %>%
    mutate(id = rep(1982:year_f, each = SST_length[2] - SST_length[1])) %>%
    nest(-id) %>%
    mutate(new_data = purrr::map(.x = data, .f = add_long)) %>%
    select(-data)
  # tictoc::toc() # 5.65 sec.
  
  
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  # Section 2. Sampling SST and convert to CPT format.
  # =-=-=
  
  
  # Paste real dates from CPT
  SST_by_id <- SST_by_id %>%
    right_join(CPT_dates, ., by = 'id')
  
  
  # Create a folder results... in this case is useful to use name sst directory.
  # path
  
  path <- paste0(path , gsub('.tsv', '', path_SST %>%  basename()) )
  if(dir.exists(path) == FALSE){dir.create(path)}else{print('ok')}
  
  # for run this function it's necessary run in a server... and is it possible
  run_cpt_sample_linux(run = 1:all_runs, y = y, data_by_id = SST_by_id, path = path)
  
  
  GI <- list.files(path = paste0(path, '/GI_runs') , pattern = 'GI', full.names = TRUE) %>%
    as.tibble() %>%
    mutate(name = list.files(path = path, pattern = 'GI') %>% gsub(('.txt'), '')   ) %>%
    mutate(GI = purrr::map(.x = value, .f = GI_read)) %>%
    dplyr::select(-value) %>%
    unnest()
  
  write_csv(x = GI, path = paste0(path, '/GI.csv'))
  
  return(GI)}

# run_by_rsample(path_SST, percent, y, path)


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= 

# y <- 'D:/OneDrive - CGIAR/Desktop/USAID-Regional/USAID-REGIONAL/honduras_chirps_data.txt'
# path <- 'D:/OneDrive - CGIAR/Desktop/USAID-Regional/USAID-REGIONAL/cluster_sampling_results/'
# path_SST <- 'D:/OneDrive - CGIAR/Desktop/USAID-Regional/USAID-REGIONAL/'


y <- '/home/aesquivel/USAID_Regional/CPT/15.7.2/Inputs/Stations-grid./honduras_chirps_data.txt'
path <- '/home/aesquivel/USAID_Regional/CPT/15.7.2/Inputs/SST_runs/'
path_SST <- '/home/aesquivel/USAID_Regional/CPT/15.7.2/Inputs/SST/'


year_f <- 2015
all_runs <- 2



# Change for list.files, because if you have several SST files. 
data_GI <-  list.files('/home/aesquivel/USAID_Regional/CPT/15.7.2/Inputs/filter_data/') %>% 
  as.tibble() %>% 
  mutate(numeric = value %>% gsub("[^[:digit:]]", "", .) %>%  as.numeric(.)) %>% 
  mutate(percent = 1 - numeric/10) %>% 
  mutate(path_SST = paste0(path_SST, value %>%  substr(., 1, 15), '.tsv'))

rm(path_SST)

# percent <- 0.2
# print(percent)

tictoc::tic()
data_GI <- data_GI %>% 
  mutate(GI =  purrr::map2(.x = path_SST, .y = percent, .f = run_by_rsample, y = y , path = path))
tictoc::toc()

# run_by_rsample(path_SST = path_SST, percent = 0.2, y = y, data_GI )

# GI <- run_by_rsample(path_SST = path_SST, percent = percent, y = y, path = path)


# fix this part... 
real_GI <- read_table2('/home/aesquivel/USAID_Regional/CPT/15.7.2/Inputs/GI_Selection_area/GIsd.txt' , skip = 5) %>% tail(n = 1L) %>%  .[, -(1:4)] 

GI %>% 
  ggplot(aes(Index_1)) +
  geom_density(fill = 'lightskyblue', alpha = 0.4) + 
  geom_vline(xintercept = real_GI$Index_1, 
             colour = 'lightskyblue', linetype="dashed",  size=1.5) + 
  theme_bw() +
  labs(x = 'Goodnes Index', subtitle = paste('Random pixels selected = ', percent))






# data_by_id = SST_by_id

# row_positions <- data_by_id %>%
#   dplyr::select(new_data) %>%
#   filter(row_number() == 1) %>%
#   unnest %>%
#   do_sample(., percent = percent)
# 
# 
# 
# SST_sample <- data_by_id %>%
#   mutate(spread_data = purrr::map(.x = new_data, .f = one_spread_layer, row_positions))  %>%
#   dplyr::select(-new_data)
# 
# path <- 'D:/OneDrive - CGIAR/Desktop/USAID-Regional/USAID-REGIONAL/'
# write_cpt(x = SST_sample, file = paste0(path, '/folder_ersst.txt') ) 







