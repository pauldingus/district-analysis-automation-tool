library(dplyr)
library(tidyverse)
library(sf)

precincts <- read_sf('../data/raw/CO_precincts/co_precincts.shp')
dist_1 = precincts %>% filter(SLDUST == "001")
st_write(dist_1, "../data/clean/co_sd_district_1.shp", driver="ESRI Shapefile")

