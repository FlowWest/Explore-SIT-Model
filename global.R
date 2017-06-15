library(shiny)

source('modules/watershed.R')
source('modules/rearing_survival.R')
source('R/juv_survival.R')

misc_inputs <- readr::read_rds('data/all_inputs.rds')

# # monthly temperature statistics for juve survival
# aveT20 <- ifelse(juv.tmp[, 2 + mnth]> 20, 1, 0)
# maxT25 <- ifelse(juv.tmp[, 2 + mnth]> 25, 1, 0)



