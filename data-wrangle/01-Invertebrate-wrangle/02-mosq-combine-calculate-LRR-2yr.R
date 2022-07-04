## combine mosquito data and calculate LRR for analyses
library(tidyverse)
mosquito_alpha <- NULL
mosquito_gamma <- NULL
mosquito_jknife <- NULL

load('~/Dropbox/1current/spatial_composition_change/data/Chicago_clean.Rdata')
mosquito_alpha <- bind_rows(mosquito_alpha, alpha_S)
mosquito_gamma <- bind_rows(mosquito_gamma, gamma_S)
mosquito_jknife <- bind_rows(mosquito_jknife, study_jknife)
rm(alpha_S, gamma_S, study_jknife)

load('~/Dropbox/1current/spatial_composition_change/data/florida_clean.Rdata')
mosquito_alpha <- bind_rows(mosquito_alpha, alpha_S)
mosquito_gamma <- bind_rows(mosquito_gamma, gamma_S)
mosquito_jknife <- bind_rows(mosquito_jknife, study_jknife)
rm(alpha_S, gamma_S, study_jknife)

load('~/Dropbox/1current/spatial_composition_change/data/idaho-n_dakota-montana_clean.Rdata')
mosquito_alpha <- bind_rows(mosquito_alpha, alpha_S)
mosquito_gamma <- bind_rows(mosquito_gamma, gamma_S)
mosquito_jknife <- bind_rows(mosquito_jknife, study_jknife)
rm(alpha_S, gamma_S, study_jknife)

load('~/Dropbox/1current/spatial_composition_change/data/indiana_clean.Rdata')
mosquito_alpha <- bind_rows(mosquito_alpha, alpha_S)
mosquito_gamma <- bind_rows(mosquito_gamma, gamma_S)
mosquito_jknife <- bind_rows(mosquito_jknife, study_jknife)
rm(alpha_S, gamma_S, study_jknife)

load('~/Dropbox/1current/spatial_composition_change/data/iowa_clean.Rdata')
mosquito_alpha <- bind_rows(mosquito_alpha, alpha_S)
mosquito_gamma <- bind_rows(mosquito_gamma, gamma_S)
# fix the region name for jknife
study_jknife <- study_jknife %>% 
  mutate(region = 'iowa')
mosquito_jknife <- bind_rows(mosquito_jknife, study_jknife)
rm(alpha_S, gamma_S, study_jknife)

load('~/Dropbox/1current/spatial_composition_change/data/virginia_clean.Rdata')
mosquito_alpha <- bind_rows(mosquito_alpha, alpha_S)
mosquito_gamma <- bind_rows(mosquito_gamma, gamma_S)
mosquito_jknife <- bind_rows(mosquito_jknife, study_jknife)
rm(alpha_S, gamma_S, study_jknife)

mosquito_alpha <- mosquito_alpha %>% 
  group_by(region, plot) %>% 
  mutate(fYear = case_when(year==min(year) ~ 'start',
                           year==max(year) ~ 'end',
                           (year!=min(year) | year!=max(year)) ~ 'intermediate')) %>% 
  ungroup() %>% 
  # calculate number of locations per region
  group_by(region) %>% 
  mutate(nLocations = n_distinct(plot)) %>% 
  ungroup()

mosquito_gamma <- mosquito_gamma %>% 
  group_by(region) %>% 
  mutate(fYear = case_when(year==min(year) ~ 'start',
                           year==max(year) ~ 'end',
                           (year!=min(year) | year!=max(year)) ~ 'intermediate')) %>% 
  ungroup() 

# calculate local LR
mosquito_local_LR <- left_join(mosquito_alpha %>%
                               filter(fYear=='start') %>% 
                               rename(S_historical = S,
                                      t1 = year) %>% 
                               select(-fYear),
                             mosquito_alpha %>%
                               filter(fYear=='end') %>% 
                               rename(S_modern = S,
                                      t2 = year) %>% 
                               select(-fYear)) %>% 
  mutate(alpha_LR = log(S_modern/S_historical),
         deltaT = t2 - t1 + 1,
         ES = alpha_LR / deltaT)


mosquito_local_mean_LR <- mosquito_local_LR %>% 
  group_by(region) %>% 
  summarise(alpha_LR_mu = mean(alpha_LR),
            check = mean(log(S_modern/S_historical)),
            alpha_LR_sd = sd(alpha_LR),
            ES_mu = mean(ES),
            ES_se = sd(ES)/(sqrt(n_distinct(plot)))) %>% 
  ungroup()

mosquito_regional_LR <- left_join(mosquito_gamma %>% 
                                  filter(fYear=='start') %>% 
                                  rename(S_historical = S, 
                                         t1 = year) %>% 
                                  select(-fYear),
                                  mosquito_gamma %>% 
                                  filter(fYear=='end') %>% 
                                  rename(S_modern = S,
                                         t2 = year) %>% 
                                  select(-fYear)) %>% 
  mutate(gamma_LR = log(S_modern/S_historical),
         deltaT = t2 - t1 + 1,
         ES = gamma_LR / deltaT)

mosquito_regional_jknife_LR <- left_join(mosquito_jknife %>% 
                                         filter(fYear=='start') %>% 
                                         rename(S_historical = S_jk, 
                                                t1 = year) %>% 
                                         select(-fYear),
                                         mosquito_jknife %>% 
                                         filter(fYear=='end') %>% 
                                         rename(S_modern = S_jk,
                                                t2 = year) %>% 
                                         select(-fYear)) %>% 
  mutate(gamma_LR = log(S_modern/S_historical),
         deltaT = t2 - t1 + 1,
         ES = gamma_LR / deltaT)

# subsample down to jacknife size
mosquito_regional_jknife_LR <- mosquito_regional_jknife_LR %>% 
  group_by(region, n_loc_plots) %>% 
  nest(data = c(t1, S_historical, t2, S_modern, gamma_LR, deltaT, ES)) %>% 
  # select(-resamp) %>% 
  sample_n(size = n_loc_plots) %>% 
  ungroup() %>% 
  unnest(data) 

save(mosquito_local_LR,
     mosquito_local_mean_LR,
     mosquito_regional_LR,
     mosquito_regional_jknife_LR,
     file = '~/Dropbox/1current/spatial_composition_change/results/mosquito_LRR.Rdata')

