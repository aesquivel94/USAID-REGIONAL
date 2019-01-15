rm(list = ls()); gc(reset = TRUE)

library(tidyverse)
library(viridis)
library(tictoc)
library(glue)

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
data_GI <- 'D:/OneDrive - CGIAR/Desktop/USAID-Regional/USAID-REGIONAL/ERSST.tsv' %>% 
  as.tibble() %>% 
  rename(path_SST = value) 

percent <- 0.2
print(percent)

SST <- read.table(data_GI$path_SST, sep="\t",  dec=".", skip =2, fill=TRUE,na.strings =-999)

#   # filter dates and rows contains cpt:field...
SST_filter <- SST %>%
  filter(row_number() != 1) %>%
  filter(!grepl("cpt:field", V1)) %>% 
  .[-1,]


# Compute Length for one layer (year-raster).
SST_length <- which(SST_filter$V1 == '')[1:2]



# extract dates in CPT format
CPT_dates <- SST[2, -1][which(!is.na(SST[2, -1]))] %>%
  t() %>%
  as.tibble() %>%
  set_names('date') %>%
  mutate(id = seq(1982,2016,1)) # Modificar esta parte. 


SST_by_id <- SST_filter %>%
  tbl_df()  %>%
  mutate(id = rep(1982:2016, each = SST_length[2] - SST_length[1])) %>% # modificar esta parte
  nest(-id) %>%
  mutate(new_data = purrr::map(.x = data, .f = add_long)) %>%
  select(-data)


SST_by_id <- SST_by_id %>%
  right_join(CPT_dates, ., by = 'id')








# save_ERSST_new <- function(data){
#   
# }




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




