# calculate metrics for time series analysis metacommunity resurvey data
library(tidyverse)

# load the data that have had the years and sites identified
load('~/Dropbox/1current/spatial_composition_change/data/resurvey-time-series-clean.Rdata')
meta <- read_csv('~/Dropbox/BioTimeX/Local-Regional Homogenization/_data_extraction/metacommunity-survey-metadata.csv')

# reduce to count data, and tidy up 
resurvey_timeSeries <- resurvey_timeSeries %>% 
  filter(!unit %in% c('pa', 'percent cover')) %>% 
  select(-c(n_loc, end_year, year_count, start_year, max_rect)) %>% 
  group_by(regional_level) %>% 
  mutate(n_sites = n_distinct(local)) %>% 
  ungroup()

# check number of locations are equal in each year for the regional level
resurvey_timeSeries %>% 
  group_by(regional_level, year) %>% 
  summarise(n_plots = n_distinct(local)) %>% 
  filter(length(unique(n_plots)) > 1)

# check 4 site, 10 year criteria
resurvey_timeSeries %>% 
  group_by(regional_level) %>% 
  summarise(nsites = n_distinct(local),
            duration = max(year) - min(year) + 1) %>% 
  filter(nsites < 4 | duration < 10)

# local scale diversity 
resurvey_alpha <- resurvey_timeSeries %>% 
  group_by(dataset_id, regional_level, local, year) %>% 
  summarise(S = n_distinct(species),
            eH = exp(vegan::diversity(value, index = 'shannon')),
            S_PIE = vegan::diversity(value, index = 'invsimpson'),
            J = sum(value)) %>% 
  ungroup()


# regional richness
resurvey_gamma <- resurvey_timeSeries %>% 
  group_by(dataset_id, regional_level, year, species) %>% 
  summarise(N = sum(value)) %>% 
  group_by(dataset_id, regional_level, year) %>% 
  summarise(S = n_distinct(species),
            eH = exp(vegan::diversity(N, index = 'shannon')),
            S_PIE = vegan::diversity(N, index = 'invsimpson'),
            J = sum(N)) %>% 
  ungroup() 

# regional richness
resurvey_gamma_wide <- resurvey_timeSeries %>% 
  group_by(dataset_id, regional_level, local, year, species) %>% 
  summarise(N = sum(value)) %>% 
  group_by(dataset_id, regional_level, year) %>% 
  nest(data = c(local, species, N)) %>% 
  mutate(wide_data = map(data, ~pivot_wider(data = .,
                                            names_from = species,
                                            values_from = N,
                                            values_fill = 0))) %>% 
  ungroup()

# calculate target coverage for each year
targetC_annual <- resurvey_gamma_wide %>% 
  mutate(target_C = map(wide_data, ~mobr::C_target(x = .[,-1], factor = 2))) 

targetC_region <- targetC_annual %>% 
  unnest(target_C) %>% 
  group_by(dataset_id, regional_level) %>% 
  summarise(minC = min(target_C),
            meanC = mean(target_C),
            medianC = median(target_C))

source('~/Dropbox/1current/R_random/three_scale_coverage_standardisation.R')
library(mobr)

resurvey_metrics <- resurvey_gamma_wide %>% 
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
                                         otherwise = NULL)),
         J = map(wide_data, ~mobr::calc_comm_div(abund_mat = .[,-1], 
                                                 index = 'N', 
                                                 scales = c('alpha',
                                                            'gamma'),
                                                 coverage = FALSE))) 



ggplot() +
  facet_wrap(~regional_level, scales = 'free') +
  geom_line(data = resurvey_alpha,
            aes(x = year, y = S, group = local)) +
  geom_line(data = resurvey_gamma,
            aes(x = year, y = S), colour = 'blue') +
  scale_y_continuous(trans = 'log2')

# regional richness jackknife
resurvey_regional_jacknife_prep <- resurvey_timeSeries %>% 
  select(dataset_id, regional_level, local, year, n_sites, species, metric,
         value, unit) %>% 
  group_by(regional_level, dataset_id, local, year, n_sites) %>% 
  nest(data = c(species, metric, value, unit)) %>% 
  ungroup()

# initialise storage for results
resurvey_regional_jacknife <- NULL
resurvey_jacknife_resamps <- NULL
# suppress summarise statement (so counter is visible)
options(dplyr.summarise.inform = FALSE)
# calculate jacknife resampled regional S for each study in a loop 
for(i in 1:length(unique(resurvey_timeSeries$regional_level))){
  print(paste('study ', i, ' in ', length(unique(resurvey_regional_jacknife_prep$regional_level))))
  
  # get a region
  region = resurvey_regional_jacknife_prep %>% 
    filter(regional_level==unique(resurvey_timeSeries$regional_level)[i]) %>% 
    arrange(-desc(year))
  
  for(t in 1:length(unique(region$year))){
    year_local <- region %>% 
      filter(year == unique(region$year)[t])
    
    for(s in 1:unique(year_local$n_sites)){
      temp = year_local %>% 
        slice(-s) %>% 
        unnest(data) %>% 
        group_by(dataset_id, regional_level, year, species) %>% 
        summarise(N = sum(value)) %>% 
        group_by(dataset_id, regional_level, year) %>% 
        summarise(S_jk = n_distinct(species),
                  eH_jk = exp(vegan::diversity(N, index = 'shannon')),
                  S_PIE_jk = vegan::diversity(N, index = 'invsimpson'),
                  J_jk = sum(N)) %>% 
        ungroup() %>% 
        mutate(jacknife = s)
        
        resurvey_regional_jacknife = bind_rows(resurvey_regional_jacknife,
                                  temp)
        # to calculate beta-diversities for the jackknife, we need to save the 
        # leave-one-out resamples
        jk_resamp = year_local %>% 
          slice(-s) %>% 
          mutate(resamp = s)
        
        resurvey_jacknife_resamps <- bind_rows(resurvey_jacknife_resamps,
                                               jk_resamp)
        
      }
    }
}

# use all jackknife resamps to calculate beta-diversity metrics
resurvey_jk_wide <- resurvey_jacknife_resamps %>% 
  unnest(data) %>% 
  group_by(dataset_id, regional_level, local, year, resamp, species) %>% 
  summarise(N = sum(value)) %>% 
  group_by(dataset_id, regional_level, year, resamp) %>% 
  nest(data = c(local, species, N)) %>% 
  mutate(wide_data = map(data, ~pivot_wider(data = .,
                                            names_from = species,
                                            values_from = N,
                                            values_fill = 0))) %>% 
  ungroup()

# calculate target coverage for each year
targetC_annual <- resurvey_jk_wide %>% 
  mutate(target_C = map(wide_data, ~mobr::C_target(x = .[,-1], factor = 2))) 

targetC_region <- targetC_annual %>% 
  unnest(target_C) %>% 
  group_by(dataset_id, regional_level) %>% 
  summarise(minC = min(target_C),
            meanC = mean(target_C),
            medianC = median(target_C))


resurvey_metrics_jk <- resurvey_jk_wide %>% 
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
                                         otherwise = NULL)),
         J = map(wide_data, ~mobr::calc_comm_div(abund_mat = .[,-1], 
                                                 index = 'N', 
                                                 scales = c('alpha',
                                                            'gamma'),
                                                 coverage = FALSE))) 

save(resurvey_alpha,
     resurvey_gamma,
     resurvey_regional_jacknife,
     resurvey_metrics,
     resurvey_metrics_jk,
     file = '~/Dropbox/1current/spatial_composition_change/results/resurvey_timeSeries_metrics.Rdata')


ggplot() +
  facet_wrap(~regional_level, scales = 'free') + 
  # stat_smooth(data = invert_metrics %>% unnest(c(S_c)),
  #             aes(x = Year, y = alpha_value, colour = 'alpha'),
  #             method = 'lm') +
  # stat_smooth(data = invert_metrics %>% unnest(c(S_c)),
  #             aes(x = Year, y = gamma_value, colour = 'gamma'),
  #             method = 'lm') +
  stat_smooth(data = resurvey_metrics_jk %>% 
                unnest(c(S_c)) %>% 
                group_by(regional_level, year) %>% 
                summarise(beta = mean(beta)),
              aes(x = year, y = beta, colour = 'beta_C'),
              method = 'lm') +
  stat_smooth(data = resurvey_metrics_jk %>% 
                unnest(c(S)) %>% 
                filter(scale == 'beta') %>% 
                group_by(regional_level, year) %>% 
                summarise(beta_S = mean(value)),
              aes(x = year, y = beta_S, colour = 'beta_S'),
              method = 'lm') +
  stat_smooth(data = resurvey_metrics_jk %>% 
                unnest(c(S_PIE)) %>% 
                filter(scale == 'beta') %>% 
                group_by(regional_level, year) %>% 
                summarise(beta_S_PIE = mean(value)),
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

