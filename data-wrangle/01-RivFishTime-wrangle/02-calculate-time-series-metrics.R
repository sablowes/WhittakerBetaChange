# wrangle rivfishtime for estimates of change that include multiple 
# years where possible

library(tidyverse)
library(mobr)
source('~/Dropbox/1current/R_random/three_scale_coverage_standardisation.R')

load('~/Dropbox/1current/spatial_composition_change/data/rft_filtered_4s_10y_sameQ.Rdata')
ft_filtered

# calculate local richness 
rft_alpha <- ft_filtered %>% 
  group_by(SourceID, TimeSeriesID, Year) %>% 
  summarise(S = n_distinct(Species),
            eH = exp(vegan::diversity(Abundance, index = 'shannon')),
            S_PIE = vegan::diversity(Abundance, index = 'invsimpson'),
            J = sum(Abundance)) %>% 
  ungroup() %>% 
  # how many annual observations? 
  group_by(SourceID) %>% 
  mutate(n_yrs = n_distinct(Year)) %>% 
  ungroup()

# time series only
rft_alpha <- rft_alpha %>% 
  filter(n_yrs > 2) %>% 
  # add some meta data
  group_by(SourceID, Year) %>% 
  mutate(nsites = n_distinct(TimeSeriesID)) %>% 
  ungroup()

# are all time series the same length
rft_alpha %>% 
  group_by(SourceID) %>% 
  filter(length(unique(n_yrs)) > 1)

rft_alpha %>% 
  group_by(SourceID) %>% 
  filter(length(unique(nsites)) > 1)

ggplot() +
  facet_wrap(~SourceID, scales = 'free') +
  geom_point(data = rft_alpha %>% filter(nsites > 3),
             aes(x = Year, y = TimeSeriesID))
  
ggplot() +
  facet_wrap(~SourceID, scales = 'free') +
  geom_line(data = rft_alpha,
             aes(x = Year, y = S_PIE, group = TimeSeriesID))

# regional scale
# regions-sites-years in the alpha-scale data
rsy <- rft_alpha %>% 
  unite(rsy, c(SourceID, TimeSeriesID, Year))

rft_gamma <- ft_filtered %>% 
  # only want regions-sites-years in the alpha-scale data
  unite(reg_loc_yr, c(SourceID, TimeSeriesID, Year), remove = FALSE) %>% 
  filter(reg_loc_yr %in% rsy$rsy) %>% 
  select(-reg_loc_yr) %>% 
  group_by(SourceID, Year, Species) %>%
  summarise(N = sum(Abundance)) %>% 
  group_by(SourceID, Year) %>% 
  summarise(S = n_distinct(Species),
            eH = exp(vegan::diversity(N, index = 'shannon')),
            S_PIE = vegan::diversity(N, index = 'invsimpson'),
            J = sum(N)) %>% 
  ungroup()

rft_gamma_wide <- ft_filtered %>% 
  # only want regions-sites-years in the alpha-scale data
  unite(reg_loc_yr, c(SourceID, TimeSeriesID, Year), remove = FALSE) %>% 
  filter(reg_loc_yr %in% rsy$rsy) %>% 
  select(-reg_loc_yr) %>% 
  group_by(SourceID, TimeSeriesID, Year, Species) %>%
  summarise(N = sum(Abundance)) %>% 
  group_by(SourceID, Year) %>% 
  nest(data = c(TimeSeriesID, Species, N)) %>% 
  mutate(wide_data = map(data, ~pivot_wider(data = .,
                                            names_from = Species,
                                            values_from = N,
                                            values_fill = 0))) %>% 
  ungroup()

# calculate target coverage for each year, find minimum per region,
# and calculate coverage-standardised richness at three scales
rft_gamma_wide <- rft_gamma_wide %>% 
  mutate(target_C = map(wide_data, ~mobr::C_target(x = .[,-1], factor = 2))) %>% 
  unnest(target_C) %>% 
  group_by(SourceID) %>% 
  mutate(targetC = min(target_C)) %>% 
  nest(Ctarget = targetC) %>% 
  mutate(S_c = map2(.x = wide_data,
                    .y = Ctarget,
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


# regional scale jacknife
prep_regional <- ft_filtered %>% 
  # only want regions-sites-years in the alpha-scale data
  unite(reg_loc_yr, c(SourceID, TimeSeriesID, Year), remove = FALSE) %>% 
  filter(reg_loc_yr %in% rsy$rsy) %>% 
  select(-reg_loc_yr) %>% 
  select(SourceID, TimeSeriesID, Year, Species, Abundance) %>% 
  nest(data = c(Species, Abundance)) %>% 
  group_by(SourceID, Year) %>% 
  mutate(n_locations = n_distinct(TimeSeriesID)) %>% 
  ungroup() 

# want regional jack knife resample
rft_regional_jknife <- NULL
rft_regional_jknife_resamps <- NULL
# suppress summarise statement (so counter is visible)
options(dplyr.summarise.inform = FALSE)

for(r in 1:length(unique(prep_regional$SourceID))){
  print(paste('region ', r, 'in ', length(unique(prep_regional$SourceID)), 'regions'))
  # get the rth region
  region = prep_regional %>% 
    filter(SourceID == unique(prep_regional$SourceID)[r])
  
  # now want a jackknife resample for each year
  for(yr in 1:length(unique(region$Year))){
    # get all plots for year == yr
    year_local <- region %>% 
      filter(Year == unique(region$Year)[yr])
    
    # do jacknife (leave one out) resampling
    for(s in 1:unique(year_local$n_locations)){
      temp = year_local %>% 
        slice(-s) %>% 
        unnest(data) %>% 
        group_by(SourceID, Year, Species) %>% 
        summarise(N = sum(Abundance)) %>% 
        group_by(SourceID, Year) %>% 
        summarise(S_jk = n_distinct(Species),
                  eH_jk = exp(vegan::diversity(N, index = 'shannon')),
                  S_PIE_jk = vegan::diversity(N, index = 'invsimpson'),
                  J_jk = sum(N)) %>% 
        ungroup() %>% 
        mutate(jacknife = s)
      
      rft_regional_jknife = bind_rows(rft_regional_jknife,
                                             temp)
      
      # to calculate beta-diversities for the jackknife, we need to save the 
      # leave-one-out resamples
      jk_resamp = year_local %>% 
        slice(-s) %>% 
        mutate(resamp = s)
      
      rft_regional_jknife_resamps <- bind_rows(rft_regional_jknife_resamps,
                                             jk_resamp)
    }
  }
}


# use all jackknife resamps to calculate beta-diversity metrics
rft_jk_wide <- rft_regional_jknife_resamps %>% 
  unnest(data) %>% 
  group_by(SourceID, TimeSeriesID, Year, resamp, Species) %>% 
  summarise(N = sum(Abundance)) %>% 
  group_by(SourceID,  Year, resamp, Species) %>% 
  nest(data = c(TimeSeriesID, Species, N)) %>% 
  mutate(wide_data = map(data, ~pivot_wider(data = .,
                                            names_from = Species,
                                            values_from = N,
                                            values_fill = 0))) %>% 
  ungroup()

# calculate target coverage for each year
targetC_annual <- rft_jk_wide %>% 
  mutate(target_C = map(wide_data, ~mobr::C_target(x = .[,-1], factor = 2))) 

targetC_region <- targetC_annual %>% 
  unnest(target_C) %>% 
  group_by(SourceID) %>% 
  summarise(minC = min(target_C),
            meanC = mean(target_C),
            medianC = median(target_C))


rft_metrics_jk <- rft_jk_wide %>% 
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
# rename beta-diversity calculations for consistency with other data sources
rft_metrics = rft_gamma_wide

save(rft_alpha,
     rft_gamma,
     rft_regional_jknife,
     rft_metrics,
     rft_metrics_jk,
     file = '~/Dropbox/1current/spatial_composition_change/results/rft_metric-time-series.Rdata')


ggplot() +
  facet_wrap(~SourceID, scales = 'free') + 
  # stat_smooth(data = invert_metrics %>% unnest(c(S_c)),
  #             aes(x = Year, y = alpha_value, colour = 'alpha'),
  #             method = 'lm') +
  # stat_smooth(data = invert_metrics %>% unnest(c(S_c)),
  #             aes(x = Year, y = gamma_value, colour = 'gamma'),
  #             method = 'lm') +
  stat_smooth(data = rft_gamma_wide %>% 
                unnest(c(S_c)),# %>% 
                # group_by(region, year) %>% 
                # summarise(beta = mean(beta)) %>% 
                # ungroup(),
              aes(x = Year, y = beta, colour = 'beta_C'),
              method = 'lm') +
  stat_smooth(data = rft_gamma_wide %>% 
                unnest(c(S)) %>% 
                filter(scale == 'beta'),
                # group_by(region, year) %>% 
                # summarise(beta_S = mean(value)) %>% 
                # ungroup(),
              aes(x = Year, y = value, colour = 'beta_S'),
              method = 'lm') +
  stat_smooth(data = rft_gamma_wide %>% 
                unnest(c(S_PIE)) %>% 
                filter(scale == 'beta'),# %>% 
                # group_by(region, year) %>% 
                # summarise(beta_S_PIE = mean(value)) %>% 
                # ungroup,
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
