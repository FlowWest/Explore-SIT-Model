library(shiny)
library(tidyverse)
library(plotly)
library(forcats)

source('modules/watershed.R')
source('modules/rearing_survival.R')
source('modules/spawning.R')
source('modules/sources.R')
source('R/juv_survival.R')


misc_inputs <- readr::read_rds('data/misc.rds')

#CVPIAdata::monthly_reach_data, contains diversion and temperature data
monthly <- readr::read_rds('data/monthly.rds')

# # monthly temperature statistics for juve survival
# aveT20 <- ifelse(juv.tmp[, 2 + mnth]> 20, 1, 0)
# maxT25 <- ifelse(juv.tmp[, 2 + mnth]> 25, 1, 0)
