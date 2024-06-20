##############################################################################################################
packs <- c("sf","terra","stringr","dplyr","ggplot2","data.table","stats","readxl","ncdf4","lubridate")

lapply(packs, require, character.only = TRUE)
##############################################################################################################

source("R/test_funcs.R")

i_a <- as.numeric(commandArgs(trailingOnly = TRUE)[1]) # array number

v_poll <- c("nox","sox","nh3","pm25")

species <- v_poll[i_a]


create_data(species, job = i_a)
