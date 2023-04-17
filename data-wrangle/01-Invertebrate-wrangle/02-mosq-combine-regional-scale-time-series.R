# code to wrangle time series mosquito data at the regional scale
library(tidyverse)

# load local scale estimates; these contain the region / site - year combinations we need to 
# get data for
load('~/Dropbox/1current/spatial_composition_change/results/mosquito_alpha_timeSeries.Rdata')

mosq_sites %>% 
  distinct(region)

# chicago data
load('~/Dropbox/1current/spatial_composition_change/data/Chicago_clean.Rdata')

combine_local_resamps <- local_resamps %>% 
  mutate(region = 'Chicago')

rm(local_resamps)

# idaho data
load('~/Dropbox/1current/spatial_composition_change/data/idaho-n_dakota-montana_clean.Rdata')

combine_local_resamps <- bind_rows(combine_local_resamps,
                                   local_resamps %>% 
                                     mutate(region = 'idaho-n_dakota-montana')
                                   )

rm(local_resamps)

# indiana data
load('~/Dropbox/1current/spatial_composition_change/data/indiana_clean.Rdata')

combine_local_resamps <- bind_rows(combine_local_resamps,
                                   local_resamps %>% 
                                     mutate(region = 'indiana')
                                   )

rm(local_resamps)

# iowa data
load('~/Dropbox/1current/spatial_composition_change/data/iowa_clean.Rdata')

combine_local_resamps <- bind_rows(combine_local_resamps,
                                   local_resamps %>% 
                                     mutate(region = 'iowa')
                                   )

rm(local_resamps)

# Lee (Florida (United States)) and Manatee data
load('~/Dropbox/1current/spatial_composition_change/data/florida_clean.Rdata')

local_resamps %>% distinct(Locations)

combine_local_resamps <- bind_rows(combine_local_resamps,
                                   local_resamps %>% 
                                     rename(region = Locations)
)

rm(local_resamps)

# virginia data
load('~/Dropbox/1current/spatial_composition_change/data/virginia_clean.Rdata')

combine_local_resamps <- bind_rows(combine_local_resamps,
                                   local_resamps %>% 
                                     mutate(region = 'virginia')
)

rm(local_resamps)

combine_local_resamps <- combine_local_resamps %>% 
  group_by(region, plot, year) %>% 
  nest(data = c(Species, N, resample)) %>% 
  ungroup()

# filter to region-site-year combinations in the alpha-scale calculations
rsy <- mosq_sites %>% 
  unite(rsy, c(region, plot, year)) %>% 
  pull(rsy)

combine_local_resamps <- combine_local_resamps %>% 
  unite(rpy, c(region, plot, year), remove = FALSE) %>% 
  filter(rpy %in% rsy) %>% 
  select(-rpy) %>% 
  left_join(mosq_sites)

# check number of plots per year
combine_local_resamps %>% 
  group_by(region, year) %>% 
  summarise(n_plots = n_distinct(plot)) %>% 
  filter(length(unique(n_plots)) > 1)

mosq_gamma_S <- combine_local_resamps %>% 
  unnest(data) %>% 
  # first get the richness for each region in each year for each resample
  group_by(region, year, resample, Species) %>% 
  summarise(N = sum(N)) %>% 
  group_by(region, year, resample) %>% 
  summarise(S_resamp = n_distinct(Species),
            eH_resamp = exp(vegan::diversity(N, index = 'shannon')),
            S_PIE_resamp = vegan::diversity(N, index = 'invsimpson')) %>% 
  ungroup() %>% 
  # now want to average richness over all the resamples
  group_by(region, year) %>% 
  summarise(S = median(S_resamp),
            eH = median(eH_resamp),
            S_PIE = median(S_PIE_resamp)) %>% 
  ungroup() 

# also want regional jack knife resample
mosq_gamma_S_resamps <- combine_local_resamps %>% 
  unnest(data) %>% 
  # first get the richness for each region in each year for each resample
  group_by(region, year, resample, Species) %>% 
  summarise(N = sum(N)) %>% 
  group_by(region, year, resample) %>% 
  summarise(S_resamp = n_distinct(Species),
            eH_resamp = exp(vegan::diversity(N, index = 'shannon')),
            S_PIE_resamp = vegan::diversity(N, index = 'invsimpson')) %>% 
  ungroup() 

# use all resamps to calculate beta-metrics (e.g., betaC)
mosq_gamma_wide <- combine_local_resamps %>% 
  unnest(data) %>% 
  group_by(region, plot, year, resample, Species) %>% 
  summarise(N = sum(N)) %>% 
  group_by(region, year, resample) %>% 
  nest(data = c(plot, Species, N)) %>% 
  mutate(wide_data = map(data, ~pivot_wider(data = .,
                                            names_from = Species,
                                            values_from = N,
                                            values_fill = 0))) %>% 
  ungroup()

# calculate target coverage for each year
targetC_annual <- mosq_gamma_wide %>% 
  mutate(target_C = map(wide_data, ~mobr::C_target(x = .[,-1], factor = 2))) 

targetC_region <- targetC_annual %>% 
  unnest(target_C) %>% 
  group_by(region) %>% 
  summarise(minC = min(target_C),
            meanC = mean(target_C),
            medianC = median(target_C))

source('~/Dropbox/1current/R_random/three_scale_coverage_standardisation.R')
library(mobr)

mosq_metrics <- mosq_gamma_wide %>% 
  left_join(targetC_region) %>% 
  nest(targetC = minC) %>% 
  mutate(S_c = map2(.x = wide_data,
                    .y = targetC,
                    .f = possibly(~three_scale_coverage(.x[,-1],
                                                        C = as.numeric(.y),
                                                        extrapolation = TRUE,
                                                        interrupt = FALSE), 
                                  otherwise = NULL)),
         S = map(wide_data, ~mobr::calc_comm_div(abund_mat = .[,-1], 
                                                 index = 'S', 
                                                 scales = c('alpha', 
                                                            'beta',
                                                            'gamma'),
                                                 coverage = FALSE)),
         S_PIE = map(wide_data, possibly(~mobr::calc_comm_div(abund_mat = .[,-1], 
                                                              index = 'S_PIE', 
                                                              scales = c('alpha', 
                                                                         'beta',
                                                                         'gamma'),
                                                              coverage = FALSE),
                                         otherwise = NULL))) 


save(mosq_gamma_S,
     mosq_gamma_S_resamps,
     mosq_metrics,
     mosquito_alpha_timeSeries,
     file = '~/Dropbox/1current/spatial_composition_change/results/mosquito_metric_timeSeries.Rdata')

ggplot() +
  facet_wrap(~region, scales = 'free') + 
  # stat_smooth(data = invert_metrics %>% unnest(c(S_c)),
  #             aes(x = Year, y = alpha_value, colour = 'alpha'),
  #             method = 'lm') +
  # stat_smooth(data = invert_metrics %>% unnest(c(S_c)),
  #             aes(x = Year, y = gamma_value, colour = 'gamma'),
  #             method = 'lm') +
  stat_smooth(data = mosq_metrics %>% 
                unnest(c(S_c)) %>% 
                group_by(region, year) %>% 
                summarise(beta = mean(beta)) %>% 
                ungroup(),
              aes(x = year, y = beta, colour = 'beta_C'),
              method = 'lm') +
  stat_smooth(data = mosq_metrics %>% 
                unnest(c(S)) %>% 
                filter(scale == 'beta') %>% 
                group_by(region, year) %>% 
                summarise(beta_S = mean(value)) %>% 
                ungroup(),
              aes(x = year, y = beta_S, colour = 'beta_S'),
              method = 'lm') +
  stat_smooth(data = mosq_metrics %>% 
                unnest(c(S_PIE)) %>% 
                filter(scale == 'beta') %>% 
                group_by(region, year) %>% 
                summarise(beta_S_PIE = mean(value)) %>% 
                ungroup,
              aes(x = year, y = beta_S_PIE, colour = 'beta_S_PIE'),
              method = 'lm') +
  # geom_point(data = invert_metrics %>% unnest(c(S_c)),
  #             aes(x = Year, y = beta)) +
  scale_colour_manual(values = c('alpha' = '#00353e',
                                 'beta_C' = '#488f31',
                                 'beta_S' = '#9fc08f',
                                 'gamma' = '#ffa600',
                                 'beta_S_PIE' = 'black')) +
  scale_y_continuous(trans = 'log2') +
  theme(axis.text = element_blank(),
        axis.title = element_blank())

