library(shiny)
library(shinythemes)
library(tidyverse)
library(plotly)
library(forcats)
library(sp)
library(rgdal)
library(leaflet)
library(shinycssloaders)
library(magrittr)

source('modules/watershed.R')
source('modules/rearing_survival.R')
source('modules/spawning.R')
source('modules/contact.R')
source('modules/sources.R')
source('modules/about.R')
source('R/juv_survival.R')


pretty_num <- function(num, places = 2) {
  format(round(num, places), big.mark = ',', drop = FALSE)
}

misc_inputs <- readr::read_rds('data/misc.rds')

#CVPIAdata::monthly_reach_data, contains diversion and temperature data
monthly <- readr::read_rds('data/monthly.rds')
inundated <- readr::read_rds('data/inundated_areas.rds')

# # monthly temperature statistics for juve survival
# aveT20 <- ifelse(juv.tmp[, 2 + mnth]> 20, 1, 0)
# maxT25 <- ifelse(juv.tmp[, 2 + mnth]> 25, 1, 0)
shed_with_div <- monthly %>% 
  dplyr::filter(!is.na(diversion)) %>% 
  dplyr::select(watershed) %>% 
  unique() %>% 
  magrittr::extract2(1)


# contact_pts <- rgdal::readOGR('data/CalFishBarrier_clippedCVPIA.shp', stringsAsFactors = FALSE) %>% 
#   spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))
# 
# CVPIAwatersheds <- rgdal::readOGR('data/CVPIAWatersheds.shp', stringsAsFactors = FALSE) %>% 
#   spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs")) 
