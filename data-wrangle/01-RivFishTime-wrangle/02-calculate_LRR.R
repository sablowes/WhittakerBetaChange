# call code to get standardised sample, and calculate LRR at local and regional scales
# visualise locations within regions

library(tidyverse)

load('~/Dropbox/1current/spatial_composition_change/data/rft_filtered_4s_10y_sameQ.Rdata')

# calculate local richness 
local_richness <- ft_filtered %>% 
  group_by(SourceID, TimeSeriesID, Year) %>% 
  summarise(S = n_distinct(Species),
            eH = exp(vegan::diversity(Abundance, index = 'shannon')),
            S_PIE = vegan::diversity(Abundance, index = 'invsimpson')) %>% 
  ungroup() %>% 
  # add indicator for calculating LRR
  group_by(SourceID, TimeSeriesID) %>% 
  mutate(fYear = case_when(Year==min(Year) ~ 'start',
                           Year==max(Year) ~ 'end',
                           (Year!=min(Year) | Year!=max(Year)) ~ 'intermediate')) %>% 
  ungroup()

# regional scale
regional_richness <- ft_filtered %>% 
  group_by(SourceID, Year, Species) %>% 
  summarise(N = sum(Abundance)) %>% 
  ungroup() %>% 
  group_by(SourceID, Year) %>% 
  summarise(S = n_distinct(Species),
            eH = exp(vegan::diversity(N, index = 'shannon')),
            S_PIE = vegan::diversity(N, index = 'invsimpson')) %>% 
  # add indicator for calculating LRR
  group_by(SourceID) %>% 
  mutate(fYear = case_when(Year==min(Year) ~ 'start',
                           Year==max(Year) ~ 'end',
                           (Year!=min(Year) | Year!=max(Year)) ~ 'intermediate')) %>% 
  ungroup()

# regional scale jacknife
prep_regional <- ft_filtered %>% 
  select(SourceID, Year, TimeSeriesID, Species, Abundance) %>% 
  nest(data = c(Species, Abundance)) %>% 
  group_by(SourceID, Year) %>% 
  mutate(n_locations = n_distinct(TimeSeriesID)) %>% 
  ungroup() %>% 
  select(SourceID, TimeSeriesID, Year, data, n_locations) %>% 
  group_by(SourceID) %>% 
  mutate(fYear = case_when(Year==min(Year) ~ 'start',
                           Year==max(Year) ~ 'end',
                           (Year!=min(Year) | Year!=max(Year)) ~ 'intermediate')) %>% 
  ungroup()

# suppress summarise statement (so counter is visible)
options(dplyr.summarise.inform = FALSE)

rft_regional_jknife <- NULL
# calculate jacknife resampled regional S for each study in a loop 
for(i in 1:length(unique(prep_regional$SourceID))){
  print(paste('region ', i, ' in ', length(unique(prep_regional$SourceID))))
  
  # get a study
  study_start = prep_regional %>% 
    filter(SourceID==unique(prep_regional$SourceID)[i] & fYear=='start')
  study_end = prep_regional %>% 
    filter(SourceID==unique(prep_regional$SourceID)[i] & fYear=='end')
  
  # initial temporary storage for each study
  region_jknife = NULL
  for(j in 1:unique(study_start$n_locations)){
    # drop on row and calculate regional richness
    start_temp = study_start %>% 
      slice(-j) %>% 
      unnest(data) %>% 
      group_by(SourceID, Year, fYear, Species) %>% 
      summarise(N = sum(Abundance)) %>% 
      group_by(SourceID, Year, fYear) %>% 
      summarise(S_jk = n_distinct(Species),
                eH_jk = exp(vegan::diversity(N, index = 'shannon')),
                S_PIE_jk = vegan::diversity(N, index = 'invsimpson')) %>% 
      ungroup() %>% 
      mutate(jacknife = j)
    
    end_temp = study_end %>% 
      slice(-j) %>% 
      unnest(data) %>% 
      group_by(SourceID, Year, fYear, Species) %>% 
      summarise(N = sum(Abundance)) %>% 
      group_by(SourceID, Year, fYear) %>% 
      summarise(S_jk = n_distinct(Species),
                eH_jk = exp(vegan::diversity(N, index = 'shannon')),
                S_PIE_jk = vegan::diversity(N, index = 'invsimpson')) %>% 
      ungroup() %>% 
      mutate(jacknife = j)
    
    # join
    region_jknife = bind_rows(region_jknife, 
                              start_temp,
                              end_temp) %>% 
      mutate(n_locations = unique(study_start$n_locations)[1])
  }
  # join studies
  rft_regional_jknife <- bind_rows(rft_regional_jknife,
                                   region_jknife)
}

# visual check: jacknife estimate should be <= regional estimate
left_join(regional_richness,
          rft_regional_jknife %>%
            group_by(SourceID, Year, fYear) %>%
            summarise(S_jk_mu = mean(S_jk))) %>%
  ggplot() +
  geom_point(aes(x = S_jk_mu, y = S)) +
  geom_abline(intercept = 0, slope = 1, lty = 2)


# calculate local log-ratio
rft_local_LR <- left_join(local_richness %>%
                            filter(fYear=='start') %>% 
                            rename(S_historical = S,
                                   eH_historical = eH,
                                   S_PIE_historical = S_PIE,
                                   t1 = Year) %>% 
                            select(-fYear),
                          local_richness %>%
                            filter(fYear=='end') %>% 
                            rename(S_modern = S,
                                   eH_modern = eH,
                                   S_PIE_modern = S_PIE,
                                   t2 = Year) %>% 
                            select(-fYear)) %>% 
  mutate(alpha_LR = log(S_modern/S_historical),
         alpha_LR_eH = log(eH_modern/eH_historical),
         alpha_LR_S_PIE = log(S_PIE_modern / S_PIE_historical),
         deltaT = t2 - t1 + 1,
         ES = alpha_LR / deltaT,
         ES_eH = alpha_LR_eH / deltaT,
         ES_S_PIE = alpha_LR_S_PIE / deltaT) %>% 
  # check length (& remove, because short ones influence number of sites)
  filter(deltaT > 9) %>% 
  group_by(SourceID) %>% 
  mutate(n_sites = n_distinct(TimeSeriesID)) %>% 
  ungroup()


# rft_local_mean_LR <- rft_local_LR %>% 
#   group_by(SourceID) %>% 
#   summarise(LRR_mu = mean(alpha_LR), 
#             ES_mu = mean(ES), 
#             ES_S_PIE_mu = mean(ES_S_PIE),
#             n_sites = unique(n_sites))


rft_regional_LR <- left_join(regional_richness %>%
                               filter(fYear=='start') %>% 
                               rename(S_historical = S,
                                      eH_historical = eH,
                                      S_PIE_historical = S_PIE,
                                      t1 = Year) %>% 
                               select(-fYear),
                             regional_richness %>%
                               filter(fYear=='end') %>% 
                               rename(S_modern = S, 
                                      eH_modern = eH,
                                      S_PIE_modern = S_PIE,
                                      t2 = Year) %>% 
                               select(-fYear)) %>% 
  mutate(gamma_LR = log(S_modern/S_historical),
         gamma_LR_eH = log(eH_modern/eH_historical),
         gamma_LR_S_PIE = log(S_PIE_modern / S_PIE_historical),
         deltaT = t2 - t1 + 1, 
         ES_gamma = gamma_LR / deltaT,
         ES_gamma_eH = gamma_LR_eH / deltaT,
         ES_gamma_S_PIE = gamma_LR_S_PIE / deltaT)


rft_regional_jknife_LR <- left_join(rft_regional_jknife %>% 
                                     filter(fYear=='start') %>% 
                                     rename(S_historical = S_jk, 
                                            eH_historical = eH_jk,
                                            S_PIE_historical = S_PIE_jk,
                                            t1 = Year) %>% 
                                     select(-fYear),
                                   rft_regional_jknife %>% 
                                     filter(fYear=='end') %>% 
                                     rename(S_modern = S_jk,
                                            eH_modern = eH_jk,
                                            S_PIE_modern = S_PIE_jk,
                                            t2 = Year) %>% 
                                     select(-fYear)) %>% 
  mutate(gamma_LR = log(S_modern/S_historical),
         gamma_LR_eH = log(eH_modern/eH_historical),
         gamma_LR_S_PIE = log(S_PIE_modern / S_PIE_historical),
         dt = t2 - t1 + 1,
         ES = gamma_LR / dt,
         ES_eH = gamma_LR_eH / dt,
         ES_S_PIE = gamma_LR_S_PIE / dt)



save(rft_local_LR,
     # rft_local_mean_LR,
     rft_regional_LR,
     rft_regional_jknife_LR,
     file = '~/Dropbox/1current/spatial_composition_change/results/rft_LRR.Rdata')
