# Author: Sadie Date: Description: prep in channel flow to area Flow in cfs, WUA
# in square feet per 1000 feet, need to extract habitat flow relation and reach
# lenght to get habitat in channel
# ----------------------
library(tidyverse)
library(readxl)
library(lubridate)
library(stringr)


# get mark's flow to area realtionships data------------------------------------
metadata_inchannel <- readxl::read_excel('data-raw/inchannel_flow_area.xlsx',
                       sheet = 'Metadata')

metadata_floodplain <- readxl::read_excel('data-raw/floodplain_flow_area.xlsx',
                               sheet = 'Metadata')

metadata_fp <- metadata_floodplain[ , 1:18] %>%
  dplyr::rename(`FP Source / Method` = `Source / Method`) %>%
  dplyr::select(Watershed,`Threshold 2-yr, 14-d Flow (cfs)`,
                `Existing FP Area (acres)`, `FP Source / Method`)

#!(Watershed %in% c('San Joaquin River (Upper)', 'Sutter Bypass', 'Yolo Bypass'))
# rename watershed to conform to jim's namings
metadata <- metadata_inchannel[ , 1:30] %>%
  dplyr::left_join(metadata_fp) %>%
  dplyr::filter(Watershed != 'San Joaquin River (Upper)') %>%
  dplyr::mutate(Watershed = replace(Watershed, Watershed == 'San Joaquin River (Lower)',
                             'San Joaquin River')) %>%
  dplyr::mutate(Watershed = replace(Watershed, Watershed == 'Mid Sacramento River',
                             'Upper-mid Sacramento River')) %>%
  dplyr::mutate(Watershed = replace(Watershed, Watershed == 'Mid South Delta',
                                    'South Delta')) %>%
  dplyr::rename(channel_inundated_area_acres = `Total Inundated Area (acres)`) %>%
  dplyr::mutate(reach_length = as.numeric(`Reach Length (ft)`),
                fp_area_acres = as.numeric(`Existing FP Area (acres)`),
                threshold_2yr_14d = as.numeric(`Threshold 2-yr, 14-d Flow (cfs)`)) %>%
  dplyr::select(Watershed, reach_length,
         `Spawning Source  / Method`:`Parr Source  / Method`, `FP Source / Method`,
         channel_inundated_area_acres, threshold_2yr_14d, fp_area_acres, `Spawning Wet`, 
         `Fry Wet`, `Parr Wet`)

# make a copy of Lower Sac and multiply .67 reach length for low mid, .33 for low
low_mid_sac <- dplyr::filter(metadata, Watershed == 'Lower Mid Sacramento & Lower Sacramento')
low_mid_sac_low_sac <- low_mid_sac %>%
  dplyr::bind_rows(low_mid_sac) %>%
  dplyr::mutate(Watershed = c('Lower Sacramento River', 'Lower-mid Sacramento River')) %>%
  dplyr::mutate(reach_length = reach_length * c(.33, .67),
                fp_area_acres = fp_area_acres * c(.33, .67))

inundated_areas <- metadata %>%
  dplyr::bind_rows(low_mid_sac_low_sac) %>%
  dplyr::filter(Watershed != 'Lower Mid Sacramento & Lower Sacramento',
                Watershed != 'North Delta', Watershed != 'South Delta') 

readr::write_rds(inundated_areas, 'data/inundated_areas.rds')
#TODO(fix with Mark around)
