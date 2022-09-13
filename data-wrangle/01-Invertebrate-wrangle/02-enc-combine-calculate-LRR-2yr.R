## combine ENC data and calculate LRR for analyses
library(tidyverse)
enc_alpha <- NULL
enc_gamma <- NULL
enc_jknife <- NULL

load('~/Dropbox/1current/spatial_composition_change/data/enc_butterflies.Rdata')
enc_alpha <- bind_rows(enc_alpha, localS) %>% 
  mutate(region = 'enc_butterflies')
enc_gamma <- bind_rows(enc_gamma, regionalS) %>% 
  mutate(region = 'enc_butterflies')
enc_jknife <- bind_rows(enc_jknife, regionalS_allresamps) %>% 
  mutate(region = 'enc_butterflies')
rm(localS, regionalS, regionalS_allresamps)

load('~/Dropbox/1current/spatial_composition_change/data/enc_moths.Rdata')

enc_alpha <- bind_rows(enc_alpha, 
                       localS %>% 
                         mutate(region = 'enc_moths')) 
enc_gamma <- bind_rows(enc_gamma, 
                       regionalS %>% 
                         mutate(region = 'enc_moths'))
enc_jknife <- bind_rows(enc_jknife, 
                        regionalS_allresamps  %>% 
                          mutate(region = 'enc_moths'))
rm(localS, regionalS, regionalS_allresamps)

load('~/Dropbox/1current/spatial_composition_change/data/enc_beetles.Rdata')
enc_alpha <- bind_rows(enc_alpha, 
                       localS %>% 
                         mutate(region = 'enc_beetles'))
enc_gamma <- bind_rows(enc_gamma, 
                       regionalS %>% 
                         mutate(region = 'enc_beetles')) 
enc_jknife <- bind_rows(enc_jknife, 
                        regionalS_allresamps %>% 
                          mutate(region = 'enc_beetles'))
rm(localS, regionalS, regionalS_allresamps)


enc_alpha <- enc_alpha %>% 
  group_by(region, site_loc) %>% 
  mutate(fYear = case_when(year==min(year) ~ 'start',
                           year==max(year) ~ 'end',
                           (year!=min(year) | year!=max(year)) ~ 'intermediate')) %>% 
  ungroup() %>% 
  # add number of locations per region
  group_by(region) %>% 
  mutate(nLocations = n_distinct(site_loc)) %>% 
  ungroup()

enc_gamma <- enc_gamma %>% 
  group_by(region) %>% 
  mutate(fYear = case_when(year==min(year) ~ 'start',
                           year==max(year) ~ 'end',
                           (year!=min(year) | year!=max(year)) ~ 'intermediate')) %>% 
  ungroup() 

enc_jknife <- enc_jknife %>% 
  group_by(region) %>% 
  mutate(fYear = case_when(year==min(year) ~ 'start',
                           year==max(year) ~ 'end',
                           (year!=min(year) | year!=max(year)) ~ 'intermediate')) %>% 
  ungroup() 

# calculate local LR
enc_local_LR <- left_join(enc_alpha %>%
                               filter(fYear=='start') %>% 
                               rename(S_historical = Sbar,
                                      eH_historical = eHbar,
                                      S_PIE_historical = S_PIEbar,
                                      t1 = year) %>% 
                               select(-fYear),
                             enc_alpha %>%
                               filter(fYear=='end') %>% 
                               rename(S_modern = Sbar,
                                      eH_modern = eHbar,
                                      S_PIE_modern = S_PIEbar,
                                      t2 = year) %>% 
                               select(-fYear)) %>% 
  mutate(alpha_LR = log(S_modern/S_historical),
         alpha_LR_eH = log(eH_modern/eH_historical),
         alpha_LR_S_PIE = log(S_PIE_modern/S_PIE_historical),
         deltaT = t2 - t1 + 1,
         ES = alpha_LR / deltaT,
         ES_eH = alpha_LR_eH / deltaT,
         ES_S_PIE = alpha_LR_S_PIE / deltaT)


enc_regional_LR <- left_join(enc_gamma %>% 
                                  filter(fYear=='start') %>% 
                                  rename(S_historical = S, 
                                         eH_historical = eH,
                                         S_PIE_historical = S_PIE,
                                         t1 = year) %>% 
                                  select(-fYear),
                                  enc_gamma %>% 
                                  filter(fYear=='end') %>% 
                                  rename(S_modern = S,
                                         eH_modern = eH,
                                         S_PIE_modern = S_PIE,
                                         t2 = year) %>% 
                                  select(-fYear)) %>% 
  mutate(gamma_LR = log(S_modern/S_historical),
         gamma_LR_eH = log(eH_modern / eH_historical),
         gamma_LR_S_PIE = log(S_PIE_modern / S_PIE_historical),
         deltaT = t2 - t1 + 1,
         ES = gamma_LR / deltaT,
         ES_eH = gamma_LR_eH / deltaT,
         ES_S_PIE = gamma_LR_S_PIE / deltaT)

enc_regional_jknife_LR <- left_join(enc_jknife %>% 
                                         filter(fYear=='start') %>% 
                                         rename(S_historical = S, 
                                                eH_historical = eH,
                                                S_PIE_historical = S_PIE,
                                                t1 = year) %>% 
                                         select(-fYear),
                                         enc_jknife %>% 
                                         filter(fYear=='end') %>% 
                                         rename(S_modern = S,
                                                eH_modern = eH,
                                                S_PIE_modern = S_PIE,
                                                t2 = year) %>% 
                                         select(-fYear)) %>% 
  mutate(gamma_LR = log(S_modern/S_historical),
         gamma_LR_eH = log(eH_modern / eH_historical),
         gamma_LR_S_PIE = log(S_PIE_modern / S_PIE_historical),
         deltaT = t2 - t1 + 1,
         ES = gamma_LR / deltaT,
         ES_eH = gamma_LR_eH / deltaT,
         ES_S_PIE = gamma_LR_S_PIE / deltaT)

# reduce the number of resamples to a jacknife size (i.e., nLocations - 1) 
# so as they don't dominate model fitting
set.seed(42)
enc_regional_jknife_LR <- left_join(enc_regional_jknife_LR,
          enc_alpha %>% 
            distinct(region, nLocations)) %>% 
  group_by(region, nLocations) %>% 
  select(resamp, region, nLocations, t1, t2, S_historical, S_modern, gamma_LR, gamma_LR_eH,
         gamma_LR_S_PIE, deltaT, ES, ES_eH, ES_S_PIE) %>% 
  nest(data = c(resamp, t1, t2, S_historical, S_modern, gamma_LR, gamma_LR_eH,
                gamma_LR_S_PIE, deltaT, ES, ES_eH, ES_S_PIE)) %>% 
  ungroup() %>% 
  mutate(jk_subsample = map2(data, nLocations, ~slice_sample(.x, n = .y))) %>% 
  select(-data) %>% 
  unnest(jk_subsample) %>%
  rename(jacknife = resamp)


save(enc_local_LR,
     enc_regional_LR,
     enc_regional_jknife_LR,
     file = '~/Dropbox/1current/spatial_composition_change/results/enc_LRR.Rdata')

