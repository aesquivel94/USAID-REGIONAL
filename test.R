# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Made by:     Alejandra Esquivel Arias. 
# Created in:  Date: 26-11-2018
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
  # data <- SST_test %>%
  #   filter(row_number() %in% SST_length[1]:SST_length[2])
  
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




# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Section 1. Reading original SST data set. 

# Read SST...
SST <- read.table("Feb_Mar-Apr-May.tsv",
                  sep="\t",  dec=".", skip =2, 
                  fill=TRUE,na.strings =-999)

# extract dates in CPT format 
# CPT_dates <- SST[1, -1][which(!is.na(SST[1, -1]))] 
    
# filter dates and rows contains cpt:field...
SST_test <- SST %>%
  filter(row_number() != 1) %>% 
  filter(!grepl("cpt:field", V1)) 

# Compute Length for one layer (year-raster).  
SST_length <- which(SST_test$V1 == '')[1:2]

# This object indicate which rows have the first position like white space. 
# partition <- which(SST_test$V1 == '')

# SST transform original format to tibble (with gather).      
tictoc::tic()
test <- SST_test %>%
  tbl_df()  %>%
  mutate(id = rep(1982:2015, each = SST_length[2] - SST_length[1])) %>% 
  nest(-id) %>% 
  mutate(new_data = purrr::map(.x = data, .f = add_long)) %>% 
  select(-data) 
tictoc::toc() # 4.57 sec. 

# Option to visualize the original region.
# test %>%
#   filter(row_number() < 5) %>%
#   unnest %>%
#   ggplot(aes(x = Long, y = Lat, fill = SST) ) +
#   geom_tile() +
#   scale_fill_viridis() +
#   coord_fixed() +
#   facet_wrap(~ id) +
#   theme_bw()




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


# Long  <- SST_test %>%
#   filter(row_number() == 1) %>% 
#   setNames(names(spread_layer)) %>%
#   mutate_if(is.factor, as.character) %>% 
#   mutate_if(is.character, as.numeric)


# Building one sample layer. 
one_spread_layer <- function(data, percent){
  # 1. First step do the function by one layer... 
  # Example for one layer
  # data <- test  %>%
  #   filter(row_number() == 1) %>% 
  #   select(new_data) %>% 
  #   unnest
  
  # data %>% 
  #   mutate(condition = ifelse(row_number() %in% rows, SST, NA)) %>% 
  #   mutate(condition = ifelse(is.na(condition), -999, condition)) %>% 
  #   filter(condition > -999) %>% 
  #   ggplot(aes(x = Long, y = Lat, fill =  condition) ) +
  #   geom_tile() +
  #   scale_fill_viridis() + 
  #   coord_fixed() +
  #   theme_bw()
  
  # 2. Which rows are different to NA data. 
  try_do_this <- which(!is.na(data))
  
  # 3. Sampling rows. 
  rows <- sample(try_do_this, length(try_do_this)*percent)
  

  spread_layer <- data %>% 
    mutate(condition = ifelse(row_number() %in% rows, SST, NA)) %>% 
    mutate(condition = ifelse(is.na(condition), -999, condition)) %>% 
    dplyr::select(-SST) %>% 
    spread(Long, condition) #%>% 
  # set_names(NULL) %>% 
   
  
   
   Long  <- SST_test %>%
     filter(row_number() == 1) %>% 
     setNames(names(spread_layer)) %>%
     mutate_if(is.factor, as.character) %>% 
     mutate_if(is.character, as.numeric)
   
   
   spread_layer <-  spread_layer %>% 
     rbind(Long, .)
   
return(spread_layer)}

test_row_1 <- test %>% 
  mutate(spread_data = purrr::map(.x = new_data, .f = one_spread_layer, percent)) %>% 
  dplyr::select(-new_data) %>% 
  filter(row_number() == 1) %>% 
  unnest








cpt_field <- SST %>%
  filter(grepl("cpt:field", V1))  


## x without year
write_cpt_head <- function(x, file){
  
  # x <- test_row_1
  x <- dplyr::select(x, -id) %>% 
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
  x <- x %>%
    filter(row_number() > 1) %>%
    unnest()
  
  x <- split(x,x$id)
  year <- names(x)
  
  purrr::map2(.x = x, .y = year, .f = function(.x, .y){
    
    .x <- dplyr::select(.x, -id)
    sink(file = "p.txt", append = T)
    cat(glue('cpt:T={.y}-03/05'), sep = '\n')
    cat(write.table(.x, sep = '\t', col.names = FALSE, row.names = FALSE, na = ""))
    sink()
  })
 
}

write_cpt <- function(x, file){
  
  write_cpt_head(x, file)
  write_cpt_body(x, file)
}

write_cpt(x = test_row_1, file = 'p.txt')


# cpt:T=





# row_1 <- 'xmlns:cpt=http://iri.columbia.edu/CPT/v10/'
# row_2 <- 'cpt:nfields=1'
# row_3 <- 'cpt:field=ssta, cpt:T=1982-03/05, cpt:nrow=61, cpt:ncol=360, cpt:row=Y, cpt:col=X, cpt:units=Kelvin_scale, cpt:missing=-999'


# cpt:T=1983-03/05

# readLines("Feb_Mar-Apr-May.tsv", n = 5L)






# rbind(Long, spread_layer) %>% 
#   # replace(., is.na(.), '') %>%
#   write.table('p.txt', sep = '\t', col.names = FALSE, row.names = FALSE)



