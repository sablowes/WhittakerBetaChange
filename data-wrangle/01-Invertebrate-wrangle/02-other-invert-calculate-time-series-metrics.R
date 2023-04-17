# create time series of metrics at multiple scales for data from Roel
#
library(tidyverse)
load('~/Dropbox/1current/spatial_composition_change/data/invert-location-plot-time-series-clean.Rdata')

# reduce to count data, and tidy up 
invert_timeSeries <- invert_timeSeries %>% 
  select(-c(end_year, year_count, start_year, max_rect, duration)) %>% 
  group_by(Datasource_ID) %>% 
  mutate(n_sites = n_distinct(loc_plot)) %>% 
  ungroup()

# check number of locations are equal in each Year for the regional level
invert_timeSeries %>% 
  group_by(Datasource_ID, Year) %>% 
  summarise(n_plots = n_distinct(loc_plot)) %>% 
  filter(length(unique(n_plots)) > 1)

# check 4 site, 10 Year criteriea
invert_timeSeries <- invert_timeSeries %>% 
  group_by(Datasource_ID) %>% 
  mutate(nsites = n_distinct(loc_plot),
         duration = max(Year) - min(Year) + 1) %>% 
  filter(nsites >= 4 & duration >= 10)

# loc_plot scale diversity
invert_alpha <- invert_timeSeries %>% 
  group_by(Datasource_ID, loc_plot, Year, Taxon) %>% 
  summarise(N = sum(Number)) %>% 
  group_by(Datasource_ID, loc_plot, Year) %>% 
  summarise(S = n_distinct(Taxon),
            eH = exp(vegan::diversity(N, index = 'shannon')),
            S_PIE = vegan::diversity(N, index = 'invsimpson'),
            J = sum(N)) %>% 
  ungroup()

# regional richness
invert_gamma <- invert_timeSeries %>% 
  group_by(Datasource_ID, Year, Taxon) %>% 
  summarise(N = sum(Number)) %>% 
  group_by(Datasource_ID, Year) %>% 
  summarise(S = n_distinct(Taxon),
            eH = exp(vegan::diversity(N, index = 'shannon')),
            S_PIE = vegan::diversity(N, index = 'invsimpson'),
            J = sum(N)) %>% 
  ungroup()

invert_gamma_wide <- invert_timeSeries %>% 
  group_by(Datasource_ID, loc_plot, Year, Taxon) %>% 
  summarise(N = sum(Number)) %>% 
  group_by(Datasource_ID, Year) %>% 
  nest(data = c(loc_plot, Taxon, N)) %>% 
  mutate(wide_data = map(data, ~pivot_wider(data = .,
                                            names_from = Taxon,
                                            values_from = N,
                                            values_fill = 0))) %>% 
  ungroup()

# calculate target coverage for each year
targetC_annual <- invert_gamma_wide %>% 
  mutate(target_C = map(wide_data, ~mobr::C_target(x = .[,-1], factor = 2))) 

targetC_region <- targetC_annual %>% 
  unnest(target_C) %>% 
  group_by(Datasource_ID) %>% 
  summarise(minC = min(target_C),
            meanC = mean(target_C),
            medianC = median(target_C))

source('~/Dropbox/1current/R_random/three_scale_coverage_standardisation.R')
library(mobr)

invert_metrics <- invert_gamma_wide %>% 
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

# save
save(invert_alpha,
     invert_gamma,
     invert_metrics, file = '~/Dropbox/1current/spatial_composition_change/results/invert_metric-time-series.Rdata')

ggplot() +
  facet_wrap(~Datasource_ID, scales = 'free') + 
  # stat_smooth(data = invert_metrics %>% unnest(c(S_c)),
  #             aes(x = Year, y = alpha_value, colour = 'alpha'),
  #             method = 'lm') +
  # stat_smooth(data = invert_metrics %>% unnest(c(S_c)),
  #             aes(x = Year, y = gamma_value, colour = 'gamma'),
  #             method = 'lm') +
  stat_smooth(data = invert_metrics %>% unnest(c(S_c)),
              aes(x = Year, y = beta, colour = 'beta_C'),
              method = 'lm') +
  stat_smooth(data = invert_metrics %>% unnest(c(S)) %>% 
                filter(scale == 'beta'),
              aes(x = Year, y = value, colour = 'beta_S'),
              method = 'lm') +
  stat_smooth(data = invert_metrics %>% unnest(c(S_PIE)) %>% 
                filter(scale == 'beta'),
              aes(x = Year, y = value, colour = 'beta_S_PIE'),
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
