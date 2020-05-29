library(sf)
library(foreign)
library(shapefiles)
library(tidyverse)
library(ggplot2)
library(rgdal)
library(tigris)
options(tigris_use_cache = TRUE)
library(lwgeom)
library(raster)
library(rmapzen)
library(scales)
library(ggtext)
library(leaflet)
library(mapview)

shp <- st_read('../data/clean/co_sd_district_1.shp')
# dbf <- read.dbf('../data/clean/co_sd_district_1.dbf')
# shx <- read.shx('../data/clean/co_sd_district_1.shx')

shp <- shp %>% 
  mutate(NAME = as.character(NAME))

shp <- st_transform(shp, "+proj=longlat +ellps=WGS84 +datum=WGS84")

bounds <- extent(shp)

lon_min <- as.numeric(bounds@xmin)
lon_max <- as.numeric(bounds@xmax)
lat_min <- as.numeric(bounds@ymin)
lat_max <- as.numeric(bounds@ymax)

bbox <- st_bbox(shp)

mz_set_tile_host_nextzen(key = "B1hyeaB4TdSD9BkzXBCQyA")
# mz_set_tile_host_nextzen(key = "NEXT_ZEN_API_KEY")

get_vector_tiles <- function(bbox) {
  mz_box = mz_rect(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax)
  mz_vector_tiles(mz_box)
}

#Pull vector tiles for bounding box
vector_tiles <- get_vector_tiles(bbox)
places <- as_sf(vector_tiles$places)
water <- as_sf(vector_tiles$water)
roads <- as_sf(vector_tiles$roads)

base_map <- ggplot() +
  geom_sf(
    data = roads %>% filter(kind == "highway"),
    colour = "darkgray",
    size = .3
  ) +
  geom_sf(
    data = roads %>% filter(kind == "major_road"),
    colour = "darkgray",
    size = .15
  ) +
  geom_sf(
    data = shp,
    fill = 'grey',
    color = 'white',
    size = .05
  ) +
  xlim(lon_min, lon_max) +
  ylim(lat_min, lat_max)

base_map + 
  geom_sf(
    data = shp,
    aes(fill = NH_WHITE),
    color = "black",
    size = .1,
  ) +
  guides(fill = guide_legend()) +
  labs(fill = 'Number of White Residents',
       title = "Title",
       subtitle = "Subtitle") +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.direction = 'horizontal',
    legend.text.align = 1
  )
