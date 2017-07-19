library(shiny)
library(shinythemes)
library(tidyverse)
library(plotly)
library(forcats)
library(sp)
library(rgdal)
library(leaflet)
library(shinycssloaders)
library(shinyjs)
library(magrittr)

source('modules/watershed.R')
source('modules/rearing_survival.R')
source('modules/spawning.R')
source('modules/delta_rearing.R')
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

threshold <- inundated %>% 
  dplyr::select(watershed = Watershed, threshold = threshold_2yr_14d, acres = fp_area_acres)

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
# 
# limit_anadromy <- rgdal::readOGR('data/Main_Rivers.shp', stringsAsFactors = FALSE) %>%
#   spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))

#delta inputs
delta_inflow <- read_rds('data/delta_inflow.rds')
delta_inputs <- read_rds('data/delta_inputs.rds')
delta_prop_div <- read_rds('data/delta_prop_div.rds')
delta_total_div <- read_rds('data/delta_tot_div.rds')
delta_temperature <- read_rds('data/delta_temperature.rds')
