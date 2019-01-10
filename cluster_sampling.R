# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Made by:     Alejandra Esquivel Arias. 
# Created in:  Date: 1-2019
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# CPT predictor area optimization - Resampling.
#
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Install CPT in Linux (Tropico server) --- Linux commands
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

# mkdir ./USAID_Regional
# cd USAID_Regional/
# tar xvzf CPT.15.7.2.tar.gz
# cd CPT
# cd 15.7.2
# make install CPT.x
# mkdir Inputs
# mkdir ./Inputs/SST
# mkdir ./Inputs/Stations-grid.
# mkdir ./Inputs/filter_data
# mkdir ./Inputs/SST_runs
# mkdir ./Inputs/GI_Selection_area

# Ejecutar R. 
# R


# rm(list = ls()); gc()



# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Packages. 
# install.packages('tictoc')
library(tidyverse)
library(viridis)
library(tictoc)
library(glue)
library(raster)

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

y <- 'D:/OneDrive - CGIAR/Desktop/USAID-Regional/USAID-REGIONAL/honduras_chirps_data.txt'
path <- 'D:/OneDrive - CGIAR/Desktop/USAID-Regional/USAID-REGIONAL/cluster_sampling_results/'
path_SST <- 'D:/OneDrive - CGIAR/Desktop/USAID-Regional/USAID-REGIONAL/'


# Change for list.files, because if you have several SST files. 
data_GI <- 'Feb_Mar-Apr-May_0.8.txt' %>% 
  as.tibble() %>% 
  mutate(numeric = value %>% gsub("[^[:digit:]]", "", .) %>%  as.numeric(.)) %>% 
  mutate(percent = 1 - numeric/10) %>% 
  mutate(path_SST = paste0(path_SST, value %>%  substr(., 1, 15), '.tsv'))






# read data... this it's similar to the last script, but we don't know if that can be change. 


percent <- as.numeric(data_GI$percent)
print(percent)


SST <- read.table(data_GI$path_SST, sep="\t",  dec=".", skip =2, fill=TRUE,na.strings =-999)

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
  mutate(id = seq(1982,2015,1))








SST_by_id <- SST_filter %>%
  tbl_df()  %>%
  mutate(id = rep(1982:2015, each = SST_length[2] - SST_length[1])) %>%
  nest(-id) %>%
  mutate(new_data = purrr::map(.x = data, .f = add_long)) %>%
  select(-data)


# Paste real dates from CPT
SST_by_id <- SST_by_id %>%
  right_join(CPT_dates, ., by = 'id')






idea <- SST_by_id %>% 
  filter(row_number() ==  nrow(.)) %>% 
  dplyr::select(new_data) %>% unnest



spg <- idea
coordinates(spg) <- ~ Long + Lat
# coerce to SpatialPixelsDataFrame
gridded(spg) <- TRUE
# coerce to raster
rasterDF <- raster(spg)

rasterDF %>% plot


plot(MoranLocal(rasterDF) > 0.2)
# plot(GearyLocal(rasterDF) < 1)


Moran(rasterDF)




