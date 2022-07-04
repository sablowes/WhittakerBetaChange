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
  mutate(nLocations = n_distinct(site_loc))

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
                                      t1 = year) %>% 
                               select(-fYear),
                             enc_alpha %>%
                               filter(fYear=='end') %>% 
                               rename(S_modern = Sbar,
                                      t2 = year) %>% 
                               select(-fYear)) %>% 
  mutate(alpha_LR = log(S_modern/S_historical),
         deltaT = t2 - t1 + 1,
         ES = alpha_LR / deltaT)


enc_local_mean_LR <- enc_local_LR %>% 
  group_by(region) %>% 
  summarise(alpha_LR_mu = mean(alpha_LR),
            check = mean(log(S_modern/S_historical)),
            alpha_LR_sd = sd(alpha_LR),
            ES_mu = mean(ES),
            ES_se = sd(ES)/(sqrt(n_distinct(site_loc)))) %>% 
  ungroup()

enc_regional_LR <- left_join(enc_gamma %>% 
                                  filter(fYear=='start') %>% 
                                  rename(S_historical = S, 
                                         t1 = year) %>% 
                                  select(-fYear),
                                  enc_gamma %>% 
                                  filter(fYear=='end') %>% 
                                  rename(S_modern = S,
                                         t2 = year) %>% 
                                  select(-fYear)) %>% 
  mutate(gamma_LR = log(S_modern/S_historical),
         deltaT = t2 - t1 + 1,
         ES = gamma_LR / deltaT)

enc_regional_jknife_LR <- left_join(enc_jknife %>% 
                                         filter(fYear=='start') %>% 
                                         rename(S_historical = S, 
                                                t1 = year) %>% 
                                         select(-fYear),
                                         enc_jknife %>% 
                                         filter(fYear=='end') %>% 
                                         rename(S_modern = S,
                                                t2 = year) %>% 
                                         select(-fYear)) %>% 
  mutate(gamma_LR = log(S_modern/S_historical),
         deltaT = t2 - t1 + 1,
         ES = gamma_LR / deltaT)

# reduce the number of resamples to a jacknife size (i.e., nLocations - 1) 
# so as they don't dominate model fitting
enc_regional_jknife_LR <- left_join(enc_regional_jknife_LR,
          enc_alpha %>% 
            distinct(region, nLocations)) %>% 
  group_by(region, nLocations) %>% 
  nest(data = c(t1, S_historical, t2, S_modern, gamma_LR, deltaT, ES)) %>% 
  # select(-resamp) %>% 
  sample_n(size = nLocations) %>% 
  ungroup() %>% 
  unnest(data) %>% 
  rename(jacknife = resamp)


save(enc_local_LR,
     enc_local_mean_LR,
     enc_regional_LR,
     enc_regional_jknife_LR,
     file = '~/Dropbox/1current/spatial_composition_change/results/enc_LRR.Rdata')

