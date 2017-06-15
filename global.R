library(shiny)

source('modules/watershed.R')
source('modules/rearing_survival.R')
# source('R/juv_survival.R')

misc_inputs <- readr::read_rds('data/all_inputs.rds')

# # monthly temperature statistics for juve survival
# aveT20 <- ifelse(juv.tmp[, 2 + mnth]> 20, 1, 0)
# maxT25 <- ifelse(juv.tmp[, 2 + mnth]> 25, 1, 0)



inv.logit <- function(eta) {
  return(1 / (1 + exp(-eta)))
}

Juv.IC.S <- function(maxT25, aveT20, high.pred, no.con.pts, prop.div, tot.div, strand) {
  
  B0 <- -0.75
  
  s <-inv.logit(B0 + -0.717 * aveT20 + 
                  -0.122 * high.pred + -0.189 * no.con.pts * 0.0358 * high.pred + -3.51 * prop.div * 0.05 + 
                  -0.0021 * tot.div * 0.215 + -1.939 * strand) * (1 - maxT25) + maxT25 * 0.1
  
  m <- inv.logit(B0 + 1.48 + -0.717 * aveT20 + 
                   -0.122 * high.pred + -0.189 * no.con.pts * 0.0358 * high.pred + -3.51 * prop.div * 0.05 + 
                   -0.0021 * tot.div * 0.215) * (1 - maxT25) + maxT25 * 0.1
  
  l <- inv.logit(B0 + 2.223 + -0.717 * aveT20 + 
                   -0.122 * high.pred + -0.189 * no.con.pts * 0.0358 * high.pred + -3.51 * prop.div * 0.05 + 
                   -0.0021 * tot.div * 0.215) * (1 - maxT25) + maxT25 * 0.1  
  cbind(s, m, l)
}

