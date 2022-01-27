
cat('To load all the libraries\n')
require(pacman)
pacman::p_load(raster, ade4, rgdal, rgeos, stringr, sf, tidyverse, gtools,
               terra, fs, glue, future, furrr, hrbrthemes, ggspatial, extrafont)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)
cat('It was done\n')

setwd('//catalogue/CL02_UCOD_FAO')
