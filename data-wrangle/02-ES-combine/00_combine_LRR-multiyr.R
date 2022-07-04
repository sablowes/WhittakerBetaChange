# combine the various effect size (and log ratio) estimates
# these estimates have averaged over multiple years in the first and second period for 
# regions where the data allowed

rm(list=ls())
library(tidyverse)


load('~/Dropbox/1current/spatial_composition_change/results/bt_multiyr_LRR.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/rft_LRR-multiyr.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/homog_multiyr_LRR.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/Sonly_LRR.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/invert_multiyr_LRR.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/mosquito_LRR_multiyr.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/enc_LRR_multiyr.Rdata')

# check biotime studies don't include problematic ones: ok
bt_local_LR %>% 
  filter(STUDY_ID %in% c(70, 458:465))

# there are some duplicates between Roel's data and the data that Alban has compiled
dupes <- c('countryside_survey_plants_2017_England', 'magnuson_2020_North Temperate Lakes', 
           'schuch_2011_Lower Saxony', 'valtonen_2018_Hungary')

homog_local_LR_multiyr <- homog_local_LR_multiyr %>% 
  unite(dsreg, c(dataset_id, regional), remove = F) %>% 
  filter(!dsreg %in% dupes) %>% 
  select(-dsreg)

homog_regional_jacknife_LR_multiyr <- homog_regional_jacknife_LR_multiyr %>% 
  unite(dsreg, c(dataset_id, regional), remove = F) %>% 
  filter(!dsreg %in% dupes) %>% 
  select(-dsreg)

homog_regional_LR_multiyr <- homog_regional_LR_multiyr %>% 
  unite(dsreg, c(dataset_id, regional), remove = F) %>% 
  filter(!dsreg %in% dupes) %>% 
  select(-dsreg)


#  make coding of 'studies' consistent and identifiable
local_LRR <- bind_rows(bt_local_LR %>% 
                         mutate(regional_level = paste0('bt_', STUDY_ID),
                                database = 'BioTIME') %>% 
                         group_by(regional_level) %>% 
                         mutate(local_level = as.character(1:n()),
                                nLocations = n_distinct(local_level)) %>% 
                         ungroup() %>% 
                         mutate(ES = alpha_LR / dt) %>% 
                         select(-STUDY_ID, -loc_plot),
                       rft_local_LR_multiyr %>% 
                         left_join(rft_regional_jknife_LR_multiyr %>% 
                                     distinct(SourceID, dt)) %>% 
                         group_by(SourceID) %>% 
                         mutate(nLocations = n_distinct(TimeSeriesID)) %>% 
                         ungroup() %>% 
                         mutate(regional_level = paste0('rft_', SourceID),
                                database = 'RivFishTime',
                                local_level = TimeSeriesID,
                                ES = alpha_LR / dt) %>% 
                         select(-SourceID, -TimeSeriesID),
                       homog_local_LR_multiyr %>% 
                         left_join(homog_regional_jacknife_LR_multiyr %>% 
                                     distinct(regional_level, dt)) %>% 
                         mutate(database = 'Homogenisation') %>% 
                         group_by(regional_level) %>% 
                         mutate(local_level = as.character(1:n()),
                                ES = alpha_LR / dt) %>% 
                         ungroup() %>% 
                         select(-dataset_id, -regional, -local),
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
                         mutate(ES = alpha_LR / dt) %>% 
                         select(-Datasource_ID, -loc_plot),
                       mosq_local_LR_multi %>% 
                         mutate(database = 'Invertebrates',
                                regional_level = paste0('i_', region),
                                local_level = paste0('i_', plot),
                                ES = alpha_LR / dt) %>% 
                         group_by(regional_level) %>% 
                         mutate(nLocations = n_distinct(local_level)) %>% 
                         ungroup() %>% 
                         select(-plot, -region),
                       enc_local_LR_multiyr %>% 
                         ungroup() %>% 
                         mutate(database = 'Invertebrates',
                                regional_level = paste0('i_', region),
                                local_level = paste0('i_', site_loc)) %>% 
                         group_by(regional_level) %>% 
                         mutate(nLocations = n_distinct(local_level)) %>% 
                         ungroup() %>% 
                         select(-site_loc, -region)) 



# Sonly has no resamples
# enc data is bootstrap resamples, not jack knife
regional_jknife_LRR <- bind_rows(bt_regional_jacknife_LR %>% 
                                   mutate(regional_level = paste0('bt_', STUDY_ID),
                                          database = 'BioTIME',
                                          ES_gamma = gamma_LR / dt) %>% 
                                   select(regional_level, gamma_LR, ES_gamma, database, dt, S_historical, S_modern, jacknife),
                                 rft_regional_jknife_LR_multiyr %>% 
                                   mutate(regional_level = paste0('rft_', SourceID),
                                          database = 'RivFishTime',
                                          ES_gamma = gamma_LR / dt) %>% 
                                   select(regional_level, gamma_LR, ES_gamma, database, dt, S_historical, S_modern, jacknife),
                                 homog_regional_jacknife_LR_multiyr %>% 
                                   mutate(ES_gamma = gamma_LR / dt) %>% 
                                   mutate(database = 'Homogenisation') %>% 
                                   select(regional_level, gamma_LR, ES_gamma, database, dt, S_historical, S_modern, jacknife),
                                 # NB: not resamples, included for completeness
                                 richness_only_regional_LR %>% 
                                   mutate(database = 'Sonly',
                                          regional_level = paste0('Sonly_', regional_level)) %>% 
                                   select(regional_level, gamma_LR, ES_gamma, database, dt, S_historical, S_modern),
                                 invert_regional_jacknife_LR %>% 
                                   mutate(ES_gamma = gamma_LR / dt) %>% 
                                   mutate(database = 'Invertebrates', 
                                          regional_level = paste0('i_', Datasource_ID)) %>% 
                                   select(regional_level, gamma_LR, ES_gamma, database, dt, S_historical, S_modern),
                                 mosq_regional_jknife_LR_multi %>% 
                                   mutate(ES_gamma = gamma_LR / dt) %>% 
                                   mutate(database = 'Invertebrates', 
                                          regional_level = paste0('i_', region)) %>% 
                                   select(regional_level, gamma_LR, ES_gamma, database, dt, S_historical, S_modern),
                                 # NB: rarefaction resamples, not jacknife
                                 enc_regional_jknife_LR_multiyr %>% 
                                   rename(ES_gamma = ES,
                                          dt = deltaT) %>% 
                                   mutate(database = 'Invertebrates', 
                                          regional_level = paste0('i_', region)) %>% 
                                   select(regional_level, gamma_LR, ES_gamma, database, dt, S_historical, S_modern))

# put the meta data in
load('~/Dropbox/1current/spatial_composition_change/data/all_meta.Rdata')

local_LRR <- left_join(local_LRR, 
                       all_meta)
  
regional_jknife_LRR <- left_join(regional_jknife_LRR,
                                 all_meta)

save(local_LRR, regional_jknife_LRR, 
     file = '~/Dropbox/1current/spatial_composition_change/results/allLRR_multiyr.Rdata')


