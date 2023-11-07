# want to fit single model to all LRR at each scale (alpha, gamma)

# LRR has similar assumptions for spatial and temporal scales (i.e., they are ignored)

# the response LRR can be standardised by time (approximately); see Vellend et al. 2013 [log(S_t2/S_t1).time^-1]
# spatial scale will be a post-hoc analysis

rm(list=ls())
library(tidyverse)


load('~/Dropbox/1current/spatial_composition_change/results/bt_LRR-new.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/rft_LRR.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/homog_LRR-new.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/Sonly_LRR.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/invert_LRR.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/mosquito_LRR.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/enc_LRR.Rdata')

# check biotime studies don't include these: ok
bt_local_LR %>% 
  filter(STUDY_ID %in% c(70, 458:465))

# there are some duplicates between Roel's data and the data that Alban has compiled
dupes <- c('countryside_survey_plants_2017_England', 'magnuson_2020_North Temperate Lakes', 
           'schuch_2011_Lower Saxony', 'valtonen_2018_Hungary')

homog_local_LR <- homog_local_LR %>% 
  unite(dsreg, c(dataset_id, regional), remove = F) %>% 
  filter(!dsreg %in% dupes) %>% 
  select(-dsreg)

homog_regional_jknife <- homog_regional_jknife %>% 
  unite(dsreg, c(dataset_id, regional), remove = F) %>% 
  filter(!dsreg %in% dupes) %>% 
  select(-dsreg)

homog_regional_jknife_LR <- homog_regional_jknife_LR %>% 
  unite(dsreg, c(dataset_id, regional), remove = F) %>% 
  filter(!dsreg %in% dupes) %>% 
  select(-dsreg)

homog_regional_LR <- homog_regional_LR %>% 
  unite(dsreg, c(dataset_id, regional), remove = F) %>% 
  filter(!dsreg %in% dupes) %>% 
  select(-dsreg)

# save these for identifying studies
homogenisation_levels <- homog_regional_jknife_LR %>% 
  distinct(dataset_id, regional, regional_level) %>% 
  mutate(regional_level = paste0('h_', regional_level))

save(homogenisation_levels,
     file = '~/Dropbox/1current/spatial_composition_change/data/homogenisation-levels.Rdata')

#  make coding of 'studies' consistent and identifiable
local_LRR <- bind_rows(bt_local_LR %>% 
                         mutate(regional_level = paste0('bt_', STUDY_ID),
                                database = 'BioTIME') %>% 
                         group_by(regional_level) %>% 
                         mutate(local_level = as.character(1:n()),
                                nLocations = n_distinct(local_level)) %>% 
                         ungroup() %>% 
                         rename(dt = deltaT) %>% 
                         select(-STUDY_ID, -loc_plot, -ABUNDANCE_TYPE, -BIOMASS_TYPE),
                       rft_local_LR %>% 
                         rename(dt = deltaT,
                                nLocations = n_sites) %>% 
                         mutate(regional_level = paste0('rft_', SourceID),
                                database = 'RivFishTime',
                                local_level = TimeSeriesID) %>% 
                         select(-SourceID, -TimeSeriesID),
                       homog_local_LR %>% 
                         mutate(database = 'Homogenisation') %>% 
                         group_by(regional_level) %>% 
                         mutate(local_level = paste0('h_', regional_level, '_', 
                                                     as.character(1:n()))) %>% 
                         ungroup() %>% 
                         mutate(regional_level = paste0('h_', regional_level)) %>% 
                         select(-dataset_id, -regional, -local, -alpha_natural, -metric),
                       richness_only_local_LR %>% 
                         mutate(database = 'Sonly',
                                regional_level = paste0('Sonly_', regional_level)) %>% 
                         group_by(regional_level) %>% 
                         mutate(local_level = as.character(1:n())) %>% 
                         ungroup() %>% 
                         select(-dataset_id, -regional, -local, -alpha_natural),
                       invert_local_LR %>% 
                         mutate(database = 'Invertebrates',
                                regional_level = paste0('i_', Datasource_ID),
                                local_level = paste0('i_', loc_plot)) %>% 
                         group_by(regional_level) %>% 
                         mutate(nLocations = n_distinct(local_level)) %>% 
                         ungroup() %>% 
                         rename(dt = deltaT) %>% 
                         select(-Datasource_ID, -loc_plot),
                       mosquito_local_LR %>% 
                         mutate(database = 'Invertebrates',
                                regional_level = paste0('i_', region),
                                local_level = paste0('i_', plot)) %>% 
                         group_by(regional_level) %>% 
                         mutate(nLocations = n_distinct(local_level)) %>% 
                         ungroup() %>% 
                         rename(dt = deltaT) %>% 
                         select(-plot, -region),
                       enc_local_LR %>% 
                         ungroup() %>% 
                         rename(dt = deltaT) %>% 
                         mutate(database = 'Invertebrates',
                                regional_level = paste0('i_', region),
                                local_level = paste0('i_', site_loc)) %>% 
                         select(-site_loc, -region)) 



regional_LRR <- bind_rows(bt_regional_LR %>% 
                            mutate(regional_level = paste0('bt_', STUDY_ID),
                                   database = 'BioTIME',
                                   ES_gamma = ES,
                                   ES_gamma_eH = ES_eH,
                                   ES_gamma_S_PIE = ES_S_PIE,
                                   dt = deltaT) %>% 
                            select(regional_level, gamma_LR, gamma_LR_eH, gamma_LR_S_PIE,
                                   ES_gamma, ES_gamma_eH, ES_gamma_S_PIE, database, dt, 
                                   S_historical, S_modern, t1, t2),
                          rft_regional_LR %>% 
                            mutate(regional_level = paste0('rft_', SourceID),
                                   database = 'RivFishTime',
                                   dt = deltaT) %>% 
                            select(regional_level, gamma_LR, gamma_LR_eH, gamma_LR_S_PIE,
                                   ES_gamma, ES_gamma_eH, ES_gamma_S_PIE, database, dt, 
                                   S_historical, S_modern, t1, t2),
                          homog_regional_LR %>% 
                            rename(gamma_LR = regional_LR,
                                   gamma_LR_eH = regional_LR_eH,
                                   gamma_LR_S_PIE = regional_LR_S_PIE,
                                   ES_gamma = ES,
                                   ES_gamma_eH = ES_eH,
                                   ES_gamma_S_PIE = ES_S_PIE) %>% 
                            mutate(database = 'Homogenisation',
                                   regional_level = paste0('h_', regional_level)) %>% 
                            select(regional_level, gamma_LR, gamma_LR_eH, gamma_LR_S_PIE,
                                   ES_gamma, ES_gamma_eH, ES_gamma_S_PIE, database, dt, 
                                   S_historical, S_modern, t1, t2),
                          richness_only_regional_LR %>% 
                            mutate(database = 'Sonly',
                                   regional_level = paste0('Sonly_', regional_level)) %>% 
                            select(regional_level, gamma_LR, 
                                   ES_gamma, database, dt, 
                                   S_historical, S_modern, t1, t2),
                          invert_regional_LR %>% 
                            rename(ES_gamma = ES,
                                   ES_gamma_eH = ES_eH,
                                   ES_gamma_S_PIE = ES_S_PIE,
                                   dt = deltaT) %>% 
                            mutate(database = 'Invertebrates', 
                                   regional_level = paste0('i_', Datasource_ID)) %>% 
                            select(regional_level, gamma_LR, gamma_LR_eH, gamma_LR_S_PIE,
                                   ES_gamma, ES_gamma_eH, ES_gamma_S_PIE, database, dt, 
                                   S_historical, S_modern, t1, t2),
                          mosquito_regional_LR %>% 
                            rename(ES_gamma = ES,
                                   ES_gamma_eH = ES_eH,
                                   ES_gamma_S_PIE = ES_S_PIE,
                                   dt = deltaT) %>% 
                            mutate(database = 'Invertebrates', 
                                   regional_level = paste0('i_', region)) %>% 
                            select(regional_level, gamma_LR, gamma_LR_eH, gamma_LR_S_PIE,
                                   ES_gamma, ES_gamma_eH, ES_gamma_S_PIE, database, dt, 
                                   S_historical, S_modern, t1, t2),
                          enc_regional_LR %>% 
                            rename(ES_gamma = ES,
                                   ES_gamma_eH = ES_eH,
                                   ES_gamma_S_PIE = ES_S_PIE,
                                   dt = deltaT) %>% 
                            mutate(database = 'Invertebrates', 
                                   regional_level = paste0('i_', region)) %>% 
                            select(regional_level, gamma_LR, gamma_LR_eH, gamma_LR_S_PIE,
                                   ES_gamma, ES_gamma_eH, ES_gamma_S_PIE, database, dt, 
                                   S_historical, S_modern, t1, t2))

# Sonly has no resamples
regional_jknife_LRR <- bind_rows(bt_regional_jknife_LR %>% 
                                   mutate(regional_level = paste0('bt_', STUDY_ID),
                                          database = 'BioTIME',
                                          ES_gamma = ES,
                                          ES_gamma_eH = ES_eH,
                                          ES_gamma_S_PIE = ES_S_PIE,
                                          dt = deltaT) %>% 
                                   select(regional_level, gamma_LR, gamma_LR_eH, gamma_LR_S_PIE,
                                          ES_gamma, ES_gamma_eH, ES_gamma_S_PIE, database, dt, 
                                          S_historical, S_modern, t1, t2),
                                 rft_regional_jknife_LR %>% 
                                   mutate(regional_level = paste0('rft_', SourceID),
                                          database = 'RivFishTime',
                                          ES_gamma = ES,
                                          ES_gamma_eH = ES_eH,
                                          ES_gamma_S_PIE = ES_S_PIE) %>% 
                                   select(regional_level, gamma_LR, gamma_LR_eH, gamma_LR_S_PIE,
                                          ES_gamma, ES_gamma_eH, ES_gamma_S_PIE, database, dt, 
                                          S_historical, S_modern, t1, t2),
                                 homog_regional_jknife_LR %>% 
                                   rename(gamma_LR = regional_LR,
                                          gamma_LR_eH = regional_LR_eH,
                                          gamma_LR_S_PIE = regional_LR_S_PIE,
                                          ES_gamma = ES,
                                          ES_gamma_eH = ES_eH,
                                          ES_gamma_S_PIE = ES_S_PIE) %>% 
                                   mutate(database = 'Homogenisation',
                                          regional_level = paste0('h_', regional_level)) %>% 
                                   select(regional_level, gamma_LR, gamma_LR_eH, gamma_LR_S_PIE,
                                          ES_gamma, ES_gamma_eH, ES_gamma_S_PIE, database, dt, 
                                          S_historical, S_modern, t1, t2),
                                 # NB: no resamples, included for completeness
                                 richness_only_regional_LR %>% 
                                   mutate(database = 'Sonly',
                                          regional_level = paste0('Sonly_', regional_level)) %>% 
                                   select(regional_level, gamma_LR,
                                          ES_gamma, database, dt, 
                                          S_historical, S_modern, t1, t2),
                                 invert_regional_jknife_LR %>% 
                                   rename(ES_gamma = ES,
                                          ES_gamma_eH = ES_eH,
                                          ES_gamma_S_PIE = ES_S_PIE,
                                          dt = deltaT) %>% 
                                   mutate(database = 'Invertebrates', 
                                          regional_level = paste0('i_', Datasource_ID)) %>% 
                                   select(regional_level, gamma_LR, gamma_LR_eH, gamma_LR_S_PIE,
                                          ES_gamma, ES_gamma_eH, ES_gamma_S_PIE, database, dt, 
                                          S_historical, S_modern, t1, t2),
                                 mosquito_regional_jknife_LR %>% 
                                   rename(ES_gamma = ES,
                                          ES_gamma_eH = ES_eH,
                                          ES_gamma_S_PIE = ES_S_PIE,
                                          dt = deltaT) %>% 
                                   mutate(database = 'Invertebrates', 
                                          regional_level = paste0('i_', region)) %>% 
                                   select(regional_level, gamma_LR, gamma_LR_eH, gamma_LR_S_PIE,
                                          ES_gamma, ES_gamma_eH, ES_gamma_S_PIE, database, dt, 
                                          S_historical, S_modern, t1, t2),
                                 enc_regional_jknife_LR %>% 
                                   rename(ES_gamma = ES,
                                          ES_gamma_eH = ES_eH,
                                          ES_gamma_S_PIE = ES_S_PIE,
                                          dt = deltaT) %>% 
                                   mutate(database = 'Invertebrates', 
                                          regional_level = paste0('i_', region)) %>% 
                                   select(regional_level, gamma_LR, gamma_LR_eH, gamma_LR_S_PIE,
                                          ES_gamma, ES_gamma_eH, ES_gamma_S_PIE, database, dt, 
                                          S_historical, S_modern, t1, t2))


save(local_LRR, regional_jknife_LRR, regional_LRR,
     file = '~/Dropbox/1current/spatial_composition_change/results/allLRR-new.Rdata')

