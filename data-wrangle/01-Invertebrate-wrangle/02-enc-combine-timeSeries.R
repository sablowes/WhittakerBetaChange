## combine ENC data and balance alpha-scale for regional scale calculations
library(tidyverse)
enc_alpha <- NULL
enc_allresamps <- NULL

load('~/Dropbox/1current/spatial_composition_change/data/enc_butterflies-multiyr.Rdata')
enc_alpha <- bind_rows(enc_alpha, localS) %>% 
  mutate(region = 'enc_butterflies')

enc_allresamps <- enc_butterflies_resamps

# tidy up  
rm(localS, regionalS, regionalS_allresamps, enc_butterflies_resamps)

load('~/Dropbox/1current/spatial_composition_change/data/enc_moths-multiyr.Rdata')

enc_alpha <- bind_rows(enc_alpha, 
                       localS %>% 
                         mutate(region = 'enc_moths')) 
enc_allresamps <- bind_rows(enc_allresamps,
                            enc_moths_resamps)

rm(localS, regionalS, regionalS_allresamps, enc_moths_resamps)

load('~/Dropbox/1current/spatial_composition_change/data/enc_beetles-multiyr.Rdata')
enc_alpha <- bind_rows(enc_alpha, 
                       localS %>% 
                         mutate(region = 'enc_beetles'))
enc_allresamps <- bind_rows(enc_allresamps,
                            enc_beetle_resamps)

rm(localS, regionalS, regionalS_allresamps, enc_beetle_resamps)

# visual inspection of balance: all regions need some attention
ggplot(enc_alpha) +
  facet_wrap(~region, scales = 'free') +
  geom_point(aes(x = year, y = site_loc))

# beetles
beetles_alpha <- enc_alpha %>% 
  filter(region=='enc_beetles') %>% 
  # remove sites with the most missing years
  group_by(site_loc) %>% 
  mutate(nyrs = n_distinct(year)) %>% 
  ungroup() %>% 
  filter(nyrs > 8) %>% 
  # drop years where some sites were not sampled
  filter(!year %in% c(1997, 2000, 2001, 2007))

ggplot(beetles_alpha) +
  facet_wrap(~region, scales = 'free') +
  geom_point(aes(x = year, y = site_loc))

# butterflies
butterflies_alpha <- enc_alpha %>% 
  filter(region=='enc_butterflies') %>% 
  # remove site with the most missing years
  group_by(site_loc) %>% 
  mutate(nyrs = n_distinct(year)) %>% 
  ungroup() %>% 
  filter(nyrs > min(nyrs)) %>% 
  # drop years where some sites were not sampled
  filter(!year %in% c(2001, 2006:2008))

ggplot(butterflies_alpha) +
  facet_wrap(~region, scales = 'free') +
  geom_point(aes(x = year, y = site_loc))

# moths
moths_alpha <- enc_alpha %>% 
  filter(region=='enc_moths') %>% 
  # remove years with some missing sites
  group_by(year) %>% 
  mutate(nsites = n_distinct(site_loc)) %>% 
  ungroup() %>% 
  filter(nsites == max(nsites))

ggplot(moths_alpha) +
  facet_wrap(~region, scales = 'free') +
  geom_point(aes(x = year, y = site_loc))

# combine alpha-scale time series
enc_alpha_timeSeries <- bind_rows(beetles_alpha,
                                  butterflies_alpha,
                                  moths_alpha) %>% 
  select(-nyrs, -nsites)


# now want to get these same region-site-year combinations for regional calculations
rsy <- enc_alpha_timeSeries %>% 
  distinct(region, site_loc, year) %>% 
  unite(rsy, c(region, site_loc, year))

enc_allresamps_filtered <- enc_allresamps %>% 
  unite(check, c(region, site_loc, year), remove = FALSE) %>% 
  filter(check %in% rsy$rsy) %>% 
  select(-check)

enc_gamma_timeSeries <- enc_allresamps_filtered %>% 
  group_by(region, year, resamp, FIELDNAME) %>% 
  summarise(N = sum(N)) %>% 
  group_by(region, year, resamp) %>% 
  summarise(S_resamp = n_distinct(FIELDNAME),
            eH_resamp = exp(vegan::diversity(N, index = 'shannon')),
            S_PIE_resamp = vegan::diversity(N, index = 'invsimpson'),
            J_resamp = sum(N)) %>% 
  ungroup() %>% 
  # now want to average richness over all the resamples
  group_by(region, year) %>% 
  summarise(S = median(S_resamp),
            eH = median(eH_resamp),
            S_PIE = median(S_PIE_resamp),
            J = median(J_resamp)) %>% 
  ungroup() 

# gamma diversity for all of the resamples
enc_gamma_resamps_timeSeries <- enc_allresamps_filtered %>% 
  group_by(region, year, resamp, FIELDNAME) %>% 
  summarise(N = sum(N)) %>% 
  group_by(region, year, resamp) %>% 
  summarise(S_resamp = n_distinct(FIELDNAME),
            eH_resamp = exp(vegan::diversity(N, index = 'shannon')),
            S_PIE_resamp = vegan::diversity(N, index = 'invsimpson'),
            J_resamp = sum(N)) %>% 
  ungroup() 


# all resamples to wide format for beta-calculations (e.g., betaC)
enc_gamma_wide <- enc_allresamps_filtered %>% 
  group_by(region, year, site_loc, resamp, FIELDNAME) %>% 
  summarise(N = sum(N)) %>% 
  group_by(region, year, resamp) %>% 
  nest(data = c(site_loc, FIELDNAME, N)) %>% 
  mutate(wide_data = map(data, ~pivot_wider(data = .,
                                            names_from = FIELDNAME,
                                            values_from = N,
                                            values_fill = 0))) %>% 
  ungroup()
  
# calculate target coverage for each year
targetC_annual <- enc_gamma_wide %>% 
  mutate(target_C = map(wide_data, ~mobr::C_target(x = .[,-1], factor = 2))) 

targetC_region <- targetC_annual %>% 
  unnest(target_C) %>% 
  group_by(region) %>% 
  summarise(minC = min(target_C),
            meanC = mean(target_C),
            medianC = median(target_C))

source('~/Dropbox/1current/R_random/three_scale_coverage_standardisation.R')
library(mobr)

enc_metrics <- enc_gamma_wide %>% 
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

save(enc_alpha_timeSeries,
     enc_gamma_resamps_timeSeries,
     enc_gamma_timeSeries,
     enc_metrics,
     file = '~/Dropbox/1current/spatial_composition_change/results/enc_timeSeries.Rdata')


ggplot() +
  facet_wrap(~region, scales = 'free') + 
  # stat_smooth(data = invert_metrics %>% unnest(c(S_c)),
  #             aes(x = Year, y = alpha_value, colour = 'alpha'),
  #             method = 'lm') +
  # stat_smooth(data = invert_metrics %>% unnest(c(S_c)),
  #             aes(x = Year, y = gamma_value, colour = 'gamma'),
  #             method = 'lm') +
  stat_smooth(data = enc_metrics %>% 
                unnest(c(S_c)) %>% 
                group_by(region, year) %>% 
                summarise(beta = mean(beta)) %>% 
                ungroup(),
              aes(x = year, y = beta, colour = 'beta_C'),
              method = 'lm') +
  stat_smooth(data = enc_metrics %>% 
                unnest(c(S)) %>% 
                filter(scale == 'beta') %>% 
                group_by(region, year) %>% 
                summarise(beta_S = mean(value)) %>% 
                ungroup(),
              aes(x = year, y = beta_S, colour = 'beta_S'),
              method = 'lm') +
  stat_smooth(data = enc_metrics %>% 
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

