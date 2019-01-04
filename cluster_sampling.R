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






