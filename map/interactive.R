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
library(lazyeval)
library(htmlwidgets)

shp <- st_read('../data/clean/co_sd_district_1.shp')
# dbf <- read.dbf('../data/clean/co_sd_district_1.dbf')
# shx <- read.shx('../data/clean/co_sd_district_1.shx')

shp <- shp %>%
  mutate(NAME = as.character(NAME))

# transform to lat long, not XY
shp <- st_transform(shp, "+proj=longlat +ellps=WGS84 +datum=WGS84")

# create lookup table for names
# from https://github.com/mggg-states/CO-shapefiles
lookup <- readr::read_csv('lookup.csv')
getlongname <- lookup$long_name
names(getlongname) <- lookup$code
# example: unname(getlongname['NH_WHITE'])

create_interactive_viz <- function(shp = shp,
                                   var) {
  var <- enquo(var)
  var_string <- rlang::quo_name(rlang::enquo(var))
  
  min <- shp %>% summarise(min = min(!!var)) %>% pull(min)
  max <- shp %>% summarise(max = max(!!var)) %>% pull(max)
  breakpoints = (max - min) / 9
  
  pal_domain <- shp %>% dplyr::select(!!var) %>% pull(!!var)
  
  bins <- seq(min, max, by = breakpoints)
  pal <- colorBin("YlOrRd", domain = pal_domain, bins = bins)
  
  m <- shp %>%
    # https://leaflet-extras.github.io/leaflet-providers/preview/
    mutate(
      popup = paste0(
        "<span style = 'font-size:1.2em;font-weight:bold;'>",
        NAME,
        "</span>",
        "<span style = 'font-size:1.1em;color:#2E2F34;'> (Precinct ",
        VTDST,
        ")</span>",
        "<br/>",
        unname(getlongname[var_string]),
        ": ",
        !!var
      ) %>%
        map(htmltools::HTML)
    ) %>%
    leaflet()  %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(
      label = ~ popup,
      fillColor = ~ pal(pal_domain),
      color = "#444444",
      weight = 1,
      smoothFactor = 0.5,
      opacity = 1.0,
      fillOpacity = 0.5,
      highlightOptions = highlightOptions(
        color = "white",
        weight = 2,
        bringToFront = TRUE
      )
    )
  
  m
  
  #create html file of the map
  filepath = sprintf("%s.html", unname(getlongname[var_string]))
  saveWidget(m, file=filepath)
  
}

create_interactive_viz(shp = shp, var = NH_ASIAN)
create_interactive_viz(shp = shp, var = NH_WHITE)
create_interactive_viz(shp = shp, var = NH_BLACK)
create_interactive_viz(shp = shp, var = REG18D)
create_interactive_viz(shp = shp, var = REG18R)
