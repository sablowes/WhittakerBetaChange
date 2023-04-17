# calculate metrics for time series analysis biotime data
library(tidyverse)

# load the data that have had the YEARs and sites identified
load('~/Dropbox/1current/spatial_composition_change/data/bt-location-plot-time-series-clean.Rdata')

# tidy up, count sites for checks 
bt_timeSeries <- bt_timeSeries %>% 
  select(-c(end_year, year_count, start_year, max_rect, duration)) %>% 
  group_by(STUDY_ID) %>% 
  mutate(n_sites = n_distinct(loc_plot)) %>% 
  ungroup()

# check number of locations are equal in each YEAR for the regional level
bt_timeSeries %>% 
  group_by(STUDY_ID, YEAR) %>% 
  summarise(n_plots = n_distinct(loc_plot)) %>% 
  filter(length(unique(n_plots)) > 1)

# check 4 site, 10 YEAR criteriea
bt_timeSeries <- bt_timeSeries %>% 
  group_by(STUDY_ID) %>% 
  mutate(nsites = n_distinct(loc_plot),
            duration = max(YEAR) - min(YEAR) + 1) %>% 
  filter(nsites >= 4 & duration >= 10)

# loc_plot scale diversity
bt_alpha <- bt_timeSeries %>% 
  group_by(STUDY_ID, loc_plot, YEAR, GENUS_SPECIES) %>% 
  summarise(N = sum(sum.allrawdata.ABUNDANCE)) %>% 
  group_by(STUDY_ID, loc_plot, YEAR) %>% 
  summarise(S = n_distinct(GENUS_SPECIES),
            eH = exp(vegan::diversity(N, index = 'shannon')),
            S_PIE = vegan::diversity(N, index = 'invsimpson'),
            totalN = sum(N)) %>% 
  ungroup() %>% 
  rename(N = totalN)

# regional richness
bt_gamma <- bt_timeSeries %>% 
  group_by(STUDY_ID, YEAR, GENUS_SPECIES) %>% 
  summarise(N = sum(sum.allrawdata.ABUNDANCE)) %>% 
  group_by(STUDY_ID, YEAR) %>% 
  summarise(S = n_distinct(GENUS_SPECIES),
            eH = exp(vegan::diversity(N, index = 'shannon')),
            S_PIE = vegan::diversity(N, index = 'invsimpson'),
            totalN = sum(N)) %>% 
  ungroup() %>% 
  rename(N = totalN)

# for coverage standardised diversities, we need a target coverage first of all
# create wide data (site x spp matrix) for each year 
bt_gamma_wide <- bt_timeSeries %>% 
  # mutate(N = as.numeric(sum.allrawdata.ABUNDANCE)) %>% 
  group_by(STUDY_ID, loc_plot, YEAR, GENUS_SPECIES) %>% 
  summarise(N = sum(sum.allrawdata.ABUNDANCE)) %>% 
  group_by(STUDY_ID, YEAR) %>% 
  nest(data = c(loc_plot, GENUS_SPECIES, N)) %>% 
  mutate(wide_data = map(data, ~pivot_wider(data = .,
                                            names_from = GENUS_SPECIES,
                                            values_from = N,
                                            values_fill = 0))) %>% 
  ungroup()

# calculate target coverage for each year
targetC_annual <- bt_gamma_wide %>% 
  mutate(target_C = map(wide_data, ~mobr::C_target(x = .[,-1], factor = 2))) 

# what have we got?
# hist(targetC_annual %>% unnest(target_C) %>% pull(target_C))

# we cannot just rarefy to some minimum randomly (it would likely result in
# unbalanced sampling within regions)
# ggplot() +
#   facet_wrap(~STUDY_ID, scales = 'free_y') +
#   geom_density(data = targetC_annual %>% 
#                  unnest(target_C),
#                aes(x = target_C)) +
#   theme_minimal()


targetC_region <- targetC_annual %>% 
  unnest(target_C) %>% 
  group_by(STUDY_ID) %>% 
  summarise(minC = min(target_C),
            meanC = mean(target_C),
            medianC = median(target_C))

# ggplot() +
#   geom_point(data = targetC_region,
#              aes(x = STUDY_ID, y = minC, colour='minC')) +
#   geom_point(data = targetC_region,
#              aes(x = STUDY_ID, y = meanC, colour='meanC')) +
#   geom_point(data = targetC_region,
#              aes(x = STUDY_ID, y = medianC, colour='medianC')) +
#   geom_segment(data = targetC_region,
#                aes(x = STUDY_ID, xend=STUDY_ID, y = meanC, yend = medianC),
#                colour='orange') +
#   scale_colour_manual(values = c('minC' = 'red',
#                                  'meanC' = 'green',
#                                  'medianC' = 'blue')) +
#   scale_x_continuous(minor_breaks = targetC_region$STUDY_ID) +
#   geom_hline(yintercept = 0.66) +
#   coord_flip() +
#   theme_minimal()

# my hack to the betaC function to get all three scales
source('~/Dropbox/1current/R_random/three_scale_coverage_standardisation.R')
library(mobr)

bt_metrics <- bt_gamma_wide %>% 
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
         N = map(wide_data, ~mobr::calc_comm_div(abund_mat = .[,-1], 
                                                 index = 'N', 
                                                 scales = c('alpha', 
                                                            'gamma'),
                                                 coverage = FALSE))) 

ggplot() +
  facet_wrap(~STUDY_ID, scales = 'free') + 
  # stat_smooth(data = invert_metrics %>% unnest(c(S_c)),
  #             aes(x = Year, y = alpha_value, colour = 'alpha'),
  #             method = 'lm') +
  # stat_smooth(data = invert_metrics %>% unnest(c(S_c)),
  #             aes(x = Year, y = gamma_value, colour = 'gamma'),
  #             method = 'lm') +
  stat_smooth(data = bt_metrics %>% 
                unnest(c(S_c)),# %>% 
                # group_by(region, year) %>% 
                # summarise(beta = mean(beta)) %>% 
                # ungroup(),
              aes(x = YEAR, y = beta, colour = 'beta_C'),
              method = 'lm') +
  stat_smooth(data = bt_metrics %>% 
                unnest(c(S)) %>% 
                filter(scale == 'beta'),# %>% 
                # group_by(region, year) %>% 
                # summarise(beta_S = mean(value)) %>% 
                # ungroup(),
              aes(x = YEAR, y = value, colour = 'beta_S'),
              method = 'lm') +
  stat_smooth(data = bt_metrics %>% 
                unnest(c(S_PIE)) %>% 
                filter(scale == 'beta'),# %>% 
                # group_by(region, year) %>% 
                # summarise(beta_S_PIE = mean(value)) %>% 
                # ungroup,
              aes(x = YEAR, y = value, colour = 'beta_S_PIE'),
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



# regional richness jackknife
bt_regional_jacknife_prep <- bt_timeSeries %>%
  select(STUDY_ID, n_sites, loc_plot, YEAR, GENUS_SPECIES, sum.allrawdata.ABUNDANCE) %>%
  group_by(STUDY_ID, n_sites, loc_plot, YEAR, GENUS_SPECIES) %>% 
  summarise(N = sum(sum.allrawdata.ABUNDANCE)) %>% 
  group_by(STUDY_ID, loc_plot, YEAR, n_sites) %>%
  nest(data = c(GENUS_SPECIES, N)) %>%
  ungroup()

# initialise storage for results
bt_regional_jacknife <- NULL
bt_regional_jacknife_resamps <- NULL
# suppress summarise statement (so counter is visible)
options(dplyr.summarise.inform = FALSE)
# calculate jacknife resampled regional S for each study in a loop
for(i in 1:length(unique(bt_timeSeries$STUDY_ID))){
  print(paste('study ', i, ' in ', length(unique(bt_regional_jacknife_prep$STUDY_ID))))

  # get a region
  region = bt_regional_jacknife_prep %>%
    filter(STUDY_ID==unique(bt_timeSeries$STUDY_ID)[i]) %>%
    arrange(-desc(YEAR))

  for(t in 1:length(unique(region$YEAR))){
    YEAR_loc_plot <- region %>%
      filter(YEAR == unique(region$YEAR)[t])

    for(s in 1:unique(YEAR_loc_plot$n_sites)){
      temp = YEAR_loc_plot %>%
        slice(-s) %>%
        unnest(data) %>%
        group_by(STUDY_ID, YEAR, GENUS_SPECIES) %>%
        summarise(N = sum(N)) %>%
        group_by(STUDY_ID, YEAR) %>%
        summarise(S_jk = n_distinct(GENUS_SPECIES),
                  eH_jk = exp(vegan::diversity(N, index = 'shannon')),
                  S_PIE_jk = vegan::diversity(N, index = 'invsimpson'),
                  totalN = sum(N)) %>%
        ungroup() %>%
        mutate(jacknife = s)

      bt_regional_jacknife = bind_rows(bt_regional_jacknife,
                                             temp)
      
      # to calculate beta-diversities for the jackknife, we need to save the 
      # leave-one-out resamples
      jk_resamp = YEAR_loc_plot %>% 
        slice(-s) %>% 
        mutate(resamp = s)
      
      bt_regional_jacknife_resamps <- bind_rows(bt_regional_jacknife_resamps,
                                             jk_resamp)
    }
  }
}

# use all jackknife resamps to calculate beta-diversity metrics
bt_jk_wide <- bt_regional_jacknife_resamps %>% 
  unnest(data) %>% 
  group_by(STUDY_ID, loc_plot, YEAR, resamp, GENUS_SPECIES) %>% 
  summarise(N = sum(N)) %>% 
  group_by(STUDY_ID, YEAR, resamp) %>% 
  nest(data = c(loc_plot, GENUS_SPECIES, N)) %>% 
  mutate(wide_data = map(data, ~pivot_wider(data = .,
                                            names_from = GENUS_SPECIES,
                                            values_from = N,
                                            values_fill = 0))) %>% 
  ungroup()

# calculate target coverage for each year
targetC_annual <- bt_jk_wide %>% 
  mutate(target_C = map(wide_data, ~mobr::C_target(x = .[,-1], factor = 2))) 

targetC_region <- targetC_annual %>% 
  unnest(target_C) %>% 
  group_by(STUDY_ID) %>% 
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
         N = map(wide_data, ~mobr::calc_comm_div(abund_mat = .[,-1], 
                                                 index = 'N', 
                                                 scales = c('alpha', 
                                                            'gamma'),
                                                 coverage = FALSE)),) 

save(bt_alpha,
     bt_gamma,
     bt_metrics,
     bt_regional_jacknife,
     bt_regional_jacknife_resamps,
     file = '~/Dropbox/1current/spatial_composition_change/results/bt_timeSeries_metrics-new.Rdata')
