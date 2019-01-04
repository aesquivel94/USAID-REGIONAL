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

# This function have a problem with found all sample in the real region.
# sp::spsample
# GSIF::sample.grid()
library(sp)

# sample size 
# n <- 20
# type <- 'clustered'

data(meuse.grid) # This shoud be SST
gridded(meuse.grid) = ~x+y
image(meuse.grid)

test <- 0
for(i in 1:10000){
sample_sp <- spsample(meuse.grid,n=100,type="clustered") 
test[i] <- sample_sp %>% data.frame() %>% dim %>% .[1]
}

hist(test)


points(sample_sp , pch=3, cex=.5)
plot(meuse.grid)



#####################################
#####################################

library(sf)
# st_sample
