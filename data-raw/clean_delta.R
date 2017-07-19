library(tidyverse)
library(stringr)

# cleaning input delta data from sit model
delta_inflow <- read_rds('data-raw/delta_inflow.rds')
names(delta_inflow) <- 1970:1990
delta_inflow %>% 
  tidyr::gather(year, flow) %>% 
  dplyr::filter(year != 1990) %>% 
  write_rds('data/delta_inflow.rds')
  

inputs <- read_rds('data-raw/delta_inputs.rds') %>% 
  mutate(watershed = case_when(
    Watershed == 'N.Delta' ~ 'North Delta',
    Watershed == 'SC.Delta' ~ 'South Delta'
  )) 

inputs %>%
  select(watershed, high_pred = High.pred, contact_points = contct.pts) %>% 
  write_rds('data/delta_inputs.rds')

inputs %>% 
  select(watershed, temp.1:temp.12) %>% 
  gather(month, temperature, -watershed) %>% 
  mutate(month = as.numeric(str_replace(month, 'temp.', ''))) %>% 
  arrange(watershed, month) %>% 
  write_rds('data/delta_temperature.rds')

prop_div <- read_rds('data-raw/delta_prop_diversions.rds')
glimpse(prop_div)

# extract prop diverted, north and south delta have same prop diverted
tot_div <- read_rds('data-raw/delta_total_diversions.rds')

total_div <- c(map(1:21, ~tot_div[, ., 1]) %>% unlist() , map(1:21, ~tot_div[, ., 2]) %>% unlist())

tibble(watershed = c(rep('North Delta', 252), rep('South Delta', 252)),
       year = rep(rep(1970:1990, each = 12), 2),
       month = rep(1:12, 42),
       tot_div = total_div) %>%
  write_rds('data/delta_tot_div.rds')

prop_diverted <- c(map(1:21, ~prop_div[, ., 1]) %>% unlist() , map(1:21, ~prop_div[, ., 2]) %>% unlist())

tibble(watershed = c(rep('North Delta', 252), rep('South Delta', 252)),
       year = rep(rep(1970:1990, each = 12), 2),
       month = rep(1:12, 42),
       prop_div = prop_diverted) %>%
  write_rds('data/delta_prop_div.rds')
