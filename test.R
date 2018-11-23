library(tidyverse)
library(viridis)

path <- 'C:/Users/aesquivel/Desktop/USAID-Regional/CPT_R'
setwd(path)


# SST <- read_delim("Feb_Mar-Apr-May.tsv",
#            "\t", escape_double = FALSE, col_names = FALSE, 
#            trim_ws = TRUE, skip = 2) 



# SST_test <- SST %>%
#   filter(row_number() != 1) %>% 
#   filter(!grepl("cpt:field",X1)) 
  
  

# idea <- which(is.na(SST_test$X1))[1:2]




# =-=-=-=-=- Esta función sirve para un año. 

# add_long <- function(data){
#   Long <- data[1,] %>%
#     t() %>% 
#     na.omit()
#   
#   data[-1, ] %>%
#     # filter(row_number() %in% (idea[1]+1):(idea[2]-1) ) %>% 
#     setNames(., c('Lat', Long)) %>%
#     gather(Long, SST ,-Lat) %>%
#     mutate_if(is.character, as.numeric) %>%
#     mutate(SST = ifelse(SST == -999, NA, SST)) 
# }




# =-=-=-=-=-=-= 

# parte <- which(is.na(SST_test$X1))

# SST_test %>%
#   mutate(id = rep(1982:2015, each = idea[2] - idea[1])) %>% 
#   nest(-id) %>%
#   mutate(new_data = purrr::map(.x = data, .f = add_long)) %>% 
#   select(-data) %>% 
#   filter(row_number() == 1) %>% 
#   unnest %>% 
#   ggplot(aes(x = Long, y = Lat, fill = SST)) + 
#   geom_tile() + 
#   theme_bw() +  
#   coord_fixed()
  
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Corriendo la mala lectura de las coordenadas long 

# Reading the original data set 
SST <- read.table("Feb_Mar-Apr-May.tsv",
                  sep="\t",  dec=".", skip =2, 
                  fill=TRUE,na.strings =-999)

# dates in CPT format 
CPT_dates <- SST[1, -1][which(!is.na(SST[1, -1]))] 
    
# filter dates and rows contains cpt:field...
SST_test <- SST %>%
  filter(row_number() != 1) %>% 
  filter(!grepl("cpt:field", V1)) 

# Compute Length for one layer (year-raster).  
SST_length <- which(SST_test$V1 == '')[1:2]


# =-=-=-=-=- Esta función sirve para un año. Organiza los datos de forma Lat, Long y SST. 

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

# =-=-=-=-=-=-=  

# posiciones en la que se encuentra las filas para particionar. 
partition <- which(SST_test$V1 == '')


test <- SST_test %>%
  tbl_df()  %>%
  mutate(id = rep(1982:2015, each = SST_length[2] - SST_length[1])) %>% 
  nest(-id) %>% 
  mutate(new_data = purrr::map(.x = data, .f = add_long)) %>% 
  select(-data) 


# test %>% 
#   filter(row_number() < 5) %>% 
#   unnest %>%
#   ggplot(aes(x = Long, y = Lat, fill = SST) ) +
#   geom_tile() +
#   scale_fill_viridis() + 
#   coord_fixed() +
#   facet_wrap(~ id) +
#   theme_bw() 


# test %>% 
#   filter(row_number() == 1) %>% 
#   unnest

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

# Tratando de crear los archivos de CPT. 

data <- test  %>%
  filter(row_number() == 1) %>% 
  select(new_data) %>% 
  unnest

try_do_this <- which(!is.na(data))

rows <- sample(try_do_this, length(try_do_this)*0.2)



data %>% 
  mutate(condition = ifelse(row_number() %in% rows, SST, NA)) %>% 
  mutate(condition = ifelse(is.na(condition), -999, condition)) %>% 
  filter(condition > -999) %>% 
  ggplot(aes(x = Long, y = Lat, fill =  condition) ) +
  geom_tile() +
  scale_fill_viridis() + 
  coord_fixed() +
  theme_bw()



# Construccion del nuevo archivo para CPT. 

spread_layer <- data %>% 
  mutate(condition = ifelse(row_number() %in% rows, SST, NA)) %>% 
  mutate(condition = ifelse(is.na(condition), -999, condition)) %>% 
  dplyr::select(-SST) %>% 
  spread(Long, condition) #%>% 
  # set_names(NULL) %>% 
  # bind_rows(Long, .)


# row_1 <- 'xmlns:cpt=http://iri.columbia.edu/CPT/v10/'
# row_2 <- 'cpt:nfields=1'
# row_3 <- 'cpt:field=ssta, cpt:T=1982-03/05, cpt:nrow=61, cpt:ncol=360, cpt:row=Y, cpt:col=X, cpt:units=Kelvin_scale, cpt:missing=-999'

Long  <- SST_test %>%
         filter(row_number() == 1) %>% 
         setNames(names(spread_layer)) %>%
         mutate_if(is.factor, as.character) %>% 
         mutate_if(is.character, as.numeric)

# cpt:T=1983-03/05

# readLines("Feb_Mar-Apr-May.tsv", n = 5L)






rbind(Long, spread_layer) %>% 
  replace(., is.na(.), '') %>%
  
  write.table('p', sep = '\t', col.names = FALSE, row.names = FALSE)



