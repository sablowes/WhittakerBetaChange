# compare results of log-ratio analysis of two-time point data (anti-ts) 
# and data where time series are available (ts)

# results from simplest two-stage analysis: overall intercept-only model fit to
# log-ratio / duration (ES)
# NB: fewer regions in the time series data (e.g., all checklists removed)

library(tidyverse)
library(brms)
library(tidybayes)
library(cowplot)

# anti-ts data (models fit to two time points only)
load('~/Dropbox/1current/spatial_composition_change/results/model_fits/local-ES-norm-sigma-ts-anti-730659.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/model_fits/regional-ES-jk-norm-sigma-ts-anti.Rdata')
# time series data (models fit to data > 2 time points)
load('~/Dropbox/1current/spatial_composition_change/results/model_fits/local-ES-norm-sigma-ts-730645.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/model_fits/regional-ES-jk-norm-sigma-ts.Rdata')
load('~/Dropbox/1current/spatial_composition_change/data/all_meta-new.Rdata')

# fix one name for joining data
all_meta <- all_meta %>% 
  mutate(regional_level = ifelse(regional_level=='i_Lee (Florida (United States))',
                                 'i_Lee', regional_level))

set.seed(101)
anti_ts_local <- gather_draws(local_ES_norm_sigma2_ts_anti, b_Intercept) %>% 
  # 90% credible interval of intercept
  median_qi(.width = 0.9)  

anti_ts_regional <- gather_draws(regional_ES_jk_norm_sigma2_ts_anti, b_Intercept) %>% 
  median_qi(.width = 0.9)

anti_ts_intercept <- bind_cols(anti_ts_local %>% 
                                 rename(local_intercept = .value,
                                        local_Q95 = .upper,
                                        local_Q05 = .lower) %>% 
                                 select(local_intercept, local_Q95, local_Q05),
                               anti_ts_regional %>% 
                                 rename(regional_intercept = .value,
                                        regional_Q95 = .upper,
                                        regional_Q05 = .lower) %>% 
                                 select(regional_intercept, regional_Q95, regional_Q05))



ts_local <- gather_draws(local_ES_norm_sigma2_ts, b_Intercept) %>% 
  # 90% credible interval of intercept
  median_qi(.width = 0.9)  

ts_regional <- gather_draws(regional_ES_jk_norm_sigma2_ts, b_Intercept) %>% 
  median_qi(.width = 0.9)

ts_intercept <- bind_cols(ts_local %>% 
                                 rename(local_intercept = .value,
                                        local_Q95 = .upper,
                                        local_Q05 = .lower) %>% 
                                 select(local_intercept, local_Q95, local_Q05),
                          ts_regional %>% 
                                 rename(regional_intercept = .value,
                                        regional_Q95 = .upper,
                                        regional_Q05 = .lower) %>% 
                                 select(regional_intercept, regional_Q95, regional_Q05))

ts_two_scale <-
ggplot() +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  geom_linerange(data = ts_intercept,
                 aes(x = local_intercept,
                     ymin = regional_Q05, ymax = regional_Q95)) +
  geom_linerange(data = ts_intercept,
                 aes(y = regional_intercept,
                     xmin = local_Q05, xmax = local_Q95)) +
  geom_linerange(data = ts_intercept,
                 aes(x = local_intercept,
                     ymin = regional_Q05, ymax = regional_Q95)) +
  geom_linerange(data = ts_intercept,
                 aes(y = regional_intercept,
                     xmin = local_Q05, xmax = local_Q95)) +
  geom_point(data = ts_intercept,
             aes(x = local_intercept,
                 y = regional_intercept),
             size = 3) +
  scale_fill_grey() +
  # scale_x_continuous(breaks = c(0, 0.002, 0.004, 0.006)) +
  # scale_y_continuous(breaks = c(0, 0.002, 0.004, 0.006)) +
  labs(y = expression(paste(gamma-scale, ' effect size [log(S) / year]')),
       x = expression(paste(alpha-scale, ' effect size [log(S) / year]')),
       # tag = '(b)',
       subtitle = expression(paste('Models fit where \ntime series data available (n = 229)'))
       ) +
  coord_fixed() +
  theme_minimal() +
  theme(plot.background = element_rect(fill = 'white', color = 'white', linetype = 0),
        panel.grid = element_blank(),
        axis.text = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) 

anti_ts_two_scale <-
  ggplot() +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  geom_linerange(data = anti_ts_intercept,
                 aes(x = local_intercept,
                     ymin = regional_Q05, ymax = regional_Q95)) +
  geom_linerange(data = anti_ts_intercept,
                 aes(y = regional_intercept,
                     xmin = local_Q05, xmax = local_Q95)) +
  geom_linerange(data = anti_ts_intercept,
                 aes(x = local_intercept,
                     ymin = regional_Q05, ymax = regional_Q95)) +
  geom_linerange(data = anti_ts_intercept,
                 aes(y = regional_intercept,
                     xmin = local_Q05, xmax = local_Q95)) +
  geom_point(data = anti_ts_intercept,
             aes(x = local_intercept,
                 y = regional_intercept),
             size = 3) +
  labs(y = expression(paste(gamma-scale, ' effect size [log(S) / year]')),
       x = expression(paste(alpha-scale, ' effect size [log(S) / year]')),
       # tag = 'a',
       title = expression(paste('Models fit to data with only\ntwo time points (n = 298)'))
       ) +
  coord_fixed() +
  theme_minimal() +
  theme(plot.background = element_rect(fill = 'white', color = 'white', linetype = 0),
        panel.grid = element_blank(),
        axis.text = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) 

# beta-diversity
# function to calculate distance of a point (x,y) to 1:1 line
source('~/Dropbox/1current/spatial_composition_change/WhittakerBetaChange/analysis/dist2line.R')

# use full posterior of the overall intercept estimates 
ts_d <- bind_cols(gather_draws(local_ES_norm_sigma2_ts, b_Intercept, ndraws = 4000) %>%
                         ungroup() %>%
                         select(x = .value),
                       gather_draws(regional_ES_jk_norm_sigma2_ts, b_Intercept, ndraws = 4000) %>%
                         ungroup() %>%
                         select(y = .value)) %>%
  mutate(d = dist2line(x, y))

anti_ts_d <- bind_cols(gather_draws(local_ES_norm_sigma2_ts_anti, b_Intercept, ndraws = 4000) %>%
                    ungroup() %>%
                    select(x = .value),
                  gather_draws(regional_ES_jk_norm_sigma2_ts_anti, b_Intercept, ndraws = 4000) %>%
                    ungroup() %>%
                    select(y = .value)) %>%
  mutate(d = dist2line(x, y))

fig3_beta_ts <-
  ggplot() +
  stat_halfeye(data = ts_d,
               aes(d),
               point_interval = median_qi,
               .width = c(0.50, 0.9)) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  # geom_label(aes(x = -0.003, y = 0.9, label = 'Homogenisation'),
  #            size = 3.5) +
  # geom_label(aes(x = 0.003, y = 0.9, label = 'Differentiation'),
  #            size = 3.5) +
  labs(x = expression(paste(Delta, beta, ' - diversity . ', year^-1)),
       y = '',
       # tag = 'd'
       # subtitle = expression(paste(Delta, beta, ' estimated using time series data (n = 165)'))
       ) +
  theme_minimal() +
  theme(legend.position = c(0.1,0.9),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = 'white', color = 'white'),
        panel.grid = element_blank(),
        axis.line.x = element_line(colour = 'black'),
        axis.ticks.x = element_line(colour = 'black'),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        plot.margin = unit(c(2,4,2,2), units = 'mm'))

fig3_beta_anti_ts <-
  ggplot() +
  stat_halfeye(data = anti_ts_d,
               aes(d),
               point_interval = median_qi,
               .width = c(0.50, 0.9)) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  # geom_label(aes(x = -0.003, y = 0.95, label = 'Homogenisation'),
  #            size = 3.5) +
  # geom_label(aes(x = 0.003, y = 0.85, label = 'Differentiation'),
  #            size = 3.5) +
  labs(x = expression(paste(Delta, beta, ' - diversity . ', year^-1)),
       y = '',
       # tag = 'c'
       # subtitle = expression(paste(Delta, beta, ' estimated using data with two time points only (n = 172)'))
       ) +
  theme_minimal() +
  theme(legend.position = c(0.1,0.9),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = 'white', color = 'white'),
        panel.grid = element_blank(),
        axis.line.x = element_line(colour = 'black'),
        axis.ticks.x = element_line(colour = 'black'),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        plot.margin = unit(c(2,4,2,2), units = 'mm'))

plot_grid(
  plot_grid(anti_ts_two_scale,
          fig3_beta_anti_ts,
          labels = c('A', 'B')),
  plot_grid(ts_two_scale,
            fig3_beta_ts,
            labels = c('C', 'D')),
  nrow = 2)

# local
ggsave('~/Dropbox/1current/spatial_composition_change/figures/results/FigS3.pdf',
       width = 184,  height = 184, units = 'mm')

# need regional variation to examine relationship with spatial and temporal scale
ts_local_posterior_ES <- local_ES_norm_sigma2_ts$data %>% 
  add_predicted_draws(object = local_ES_norm_sigma2_ts, ndraws = 1000)

anti_ts_local_posterior_ES <- local_ES_norm_sigma2_ts_anti$data %>% 
  add_predicted_draws(object = local_ES_norm_sigma2_ts_anti, ndraws = 1000)

ts_regional_posterior_ES <- regional_ES_jk_norm_sigma2_ts$data %>% 
  add_predicted_draws(object = regional_ES_jk_norm_sigma2_ts, ndraws = 1000)

anti_ts_regional_posterior_ES <- regional_ES_jk_norm_sigma2_ts_anti$data %>% 
  add_predicted_draws(object = regional_ES_jk_norm_sigma2_ts_anti, ndraws = 1000)

ts_regional_d <- bind_cols(ts_local_posterior_ES %>% 
                             ungroup() %>% 
                             rename(local_dS = .prediction) %>% 
                             select(regional_level, local_dS) %>% 
                             group_by(regional_level) %>% 
                             sample_n(1000) %>% 
                             ungroup() %>% 
                             rename(regional.x = regional_level),
                           ts_regional_posterior_ES %>% 
                             ungroup() %>% 
                             rename(regional_dS = .prediction) %>% 
                             select(regional_level, regional_dS) %>% 
                             group_by(regional_level) %>% 
                             sample_n(1000) %>% 
                             ungroup() %>%
                             rename(regional.y = regional_level)) %>%
  filter(regional.x==regional.y) %>% 
  mutate(d = dist2line(local_dS, regional_dS)) %>% 
  rename(regional_level = regional.x) %>% 
  select(-regional.y)

anti_ts_regional_d <- bind_cols(anti_ts_local_posterior_ES %>% 
                                  ungroup() %>% 
                                  rename(local_dS = .prediction) %>% 
                                  select(regional_level, local_dS) %>% 
                                  group_by(regional_level) %>% 
                                  sample_n(1000) %>% 
                                  ungroup() %>% 
                                  rename(regional.x = regional_level),
                                anti_ts_regional_posterior_ES %>% 
                                  ungroup() %>% 
                                  rename(regional_dS = .prediction) %>% 
                                  select(regional_level, regional_dS) %>% 
                                  group_by(regional_level) %>% 
                                  sample_n(1000) %>% 
                                  ungroup() %>%
                                  rename(regional.y = regional_level)) %>%
  filter(regional.x==regional.y) %>% 
  mutate(d = dist2line(local_dS, regional_dS)) %>% 
  rename(regional_level = regional.x) %>% 
  select(-regional.y)

# get pattern summary for ts data
source('~/Dropbox/1current/spatial_composition_change/WhittakerBetaChange/analysis/FigS5-1-wrangle-ts-data.R')

ts_regional_d_summary <- ts_regional_d %>% 
  group_by(regional_level) %>% 
  summarise(d_mu = mean(d),
            lower = quantile(d, probs = 0.05),
            upper = quantile(d, prob = 0.95)) %>% 
  left_join(ts_pattern_summary %>% 
              select(regional_level, concept))

# get pattern summary for anti_ts data
source('~/Dropbox/1current/spatial_composition_change/WhittakerBetaChange/analysis/FigS5-1-wrangle-anti-ts-data.R')

anti_ts_regional_d_summary <- anti_ts_regional_d %>% 
  group_by(regional_level) %>% 
  summarise(d_mu = mean(d),
            lower = quantile(d, probs = 0.05),
            upper = quantile(d, prob = 0.95)) %>% 
  left_join(anti_ts_pattern_summary %>% 
              select(regional_level, concept))


# now plot beta-diversity estimate as function of temporal and spatial extent
concept_colour = c('Gain low occupancy' = '#61CDE0',
                   'Low occupancy replace high' = '#2CABA4',
                   'Lose high occupancy' = '#155F49',
                   'Lose low occupancy' = '#D17538',
                   'High occupancy replace low' = '#E9AE27',
                   'Gain high occupancy' = '#D9D956')

# first, a plot to create the legend
col_shape_leg_plot <- left_join(anti_ts_regional_d_summary,
                                local_ES_norm_sigma2$data %>% 
                                  distinct(regional_level, sample_type, logdt)) %>% 
  mutate(dt = exp(logdt)) %>% 
  left_join(all_meta) %>% 
  ggplot() +
  # facet_wrap(~realm) +
  geom_hline(yintercept = 0, lty = 2) + 
  geom_point(aes(x = dt, y = d_mu, colour = concept,
                 shape = sample_type),
             size = 2) +
  scale_x_continuous(name = 'Temporal duration [years]',
                     trans = 'log10') + 
  labs(y = expression(paste(Delta, beta, ' - diversity . ', year^-1))) +
  scale_color_manual(#guide = 'none',
    name = '',
    values = concept_colour) +
  scale_shape_manual(#guide = 'none',
    name = '', values = c('checklist' = 17,
                                     'resurvey' = 19)) +
  theme_minimal() +
  theme(legend.position = 'top') +
  guides(shape = guide_legend(nrow = 2))

# function to get legend from ggplot object
source('~/Dropbox/1current/R_random/functions/gg_legend.R')
col_shape_leg <- gg_legend(col_shape_leg_plot)


anti_ts_beta_dt <-
left_join(anti_ts_regional_d_summary,
          local_ES_norm_sigma2_ts_anti$data %>% 
            distinct(regional_level, logdt)) %>% 
  mutate(dt = exp(logdt)) %>% 
  left_join(all_meta) %>% 
  # remove four regions with large effect sizes
  filter(abs(d_mu) < 0.05) %>%
  ggplot() +
  geom_hline(yintercept = 0, lty = 2) + 
  geom_point(aes(x = dt, y = d_mu, colour = concept,
                 shape = sample_type,
                 fill = concept,
                 alpha = sample_type),
             size = 2, colour = 'black'
             ) +
  scale_x_continuous(name = 'Temporal duration [years]',
                     trans = 'log10') + 
  labs(y = expression(paste(Delta, beta, ' - diversity . ', year^-1))) +
  scale_color_manual(guide = 'none',
                     name = 'Occupancy change',
                     values = concept_colour) +
  scale_alpha_manual(guide = 'none',
    name = 'Sample type',
    values = c('checklist' = 1,
               'resurvey' = 0.8
    )) +
  scale_fill_manual(guide = 'none',
                    name = 'Occupancy change',
                    values = concept_colour) +
  scale_shape_manual(guide = 'none',
                     name = 'Sample type', values = c('checklist' = 24,
                                                      'resurvey' = 21)) +
  theme_minimal() +
  theme(legend.position = 'top')


anti_ts_beta_extent <-
left_join(anti_ts_regional_d_summary,
          all_meta) %>% 
  mutate(gamma_extent_km2 = case_when(is.na(gamma_bounding_box_km2) ~ gamma_sum_grains_km2,
                                      TRUE ~ as.numeric(gamma_bounding_box_km2))) %>% 
  # nudge zero extents so they plot ok
  mutate(gamma_extent_km2 = case_when(gamma_extent_km2==0 ~ 0.0001,
                                      TRUE ~ as.numeric(gamma_extent_km2))) %>% 
  # remove four regions with large effect sizes
  filter(abs(d_mu) < 0.05) %>%
  ggplot() +
  geom_hline(yintercept = 0, lty = 2) + 
  geom_point(aes(x = gamma_extent_km2, y = d_mu, 
                 colour = concept,
                 fill = concept,
                 shape = sample_type,
                 alpha = sample_type
  ), size = 2, colour = 'black') + 
  scale_x_continuous(trans = 'log10', name = 'Spatial extent [km2]') +
  labs(y = expression(paste(Delta, beta, ' - diversity . ', year^-1))) +
  scale_color_manual(guide = 'none',
                     name = 'Occupancy change',
                     values = concept_colour) +
  scale_alpha_manual(guide = 'none',
                     name = 'Sample type',
                     values = c('checklist' = 1,
                                'resurvey' = 0.8
                     )) +
  scale_fill_manual(guide = 'none',
                    name = 'Occupancy change',
                    values = concept_colour) +
  scale_shape_manual(guide = 'none',
                     name = 'Sample type', values = c('checklist' = 24,
                                                      'resurvey' = 21)) +
  theme_minimal()


ts_beta_dt <-
left_join(ts_regional_d_summary,
          local_ES_norm_sigma2_ts$data %>% 
            distinct(regional_level, logdt)) %>% 
  mutate(dt = exp(logdt)) %>% 
  left_join(all_meta) %>% 
  # remove one region with large ES (has short duration and small extent)
  filter(abs(d_mu) < 0.05) %>%
  ggplot() +
  geom_hline(yintercept = 0, lty = 2) + 
  geom_point(aes(x = dt, y = d_mu, colour = concept,
                 shape = sample_type,
                 fill = concept,
                 alpha = sample_type),
             size = 2, colour = 'black') +
  scale_x_continuous(name = 'Temporal duration [years]',
                     trans = 'log10') + 
  labs(y = expression(paste(Delta, beta, ' - diversity . ', year^-1))) +
  scale_color_manual(guide = 'none',
                     name = 'Occupancy change',
                     values = concept_colour) +
  scale_alpha_manual(guide = 'none',
                     name = 'Sample type',
                     values = c('checklist' = 1,
                                'resurvey' = 0.8
                     )) +
  scale_fill_manual(guide = 'none',
                    name = 'Occupancy change',
                    values = concept_colour) +
  scale_shape_manual(guide = 'none',
                     name = 'Sample type', values = c('checklist' = 24,
                                                      'resurvey' = 21)) +
  theme_minimal() +
  theme(legend.position = 'top')

ts_beta_extent <-
  left_join(ts_regional_d_summary,
            all_meta) %>% 
  mutate(gamma_extent_km2 = case_when(is.na(gamma_bounding_box_km2) ~ gamma_sum_grains_km2,
                                      TRUE ~ as.numeric(gamma_bounding_box_km2))) %>% 
  # nudge zero extents so they plot ok
  mutate(gamma_extent_km2 = case_when(gamma_extent_km2==0 ~ 0.0001,
                                      TRUE ~ as.numeric(gamma_extent_km2))) %>% 
  filter(abs(d_mu) < 0.05) %>%
  ggplot() +
  geom_hline(yintercept = 0, lty = 2) + 
  geom_point(aes(x = gamma_extent_km2, y = d_mu, 
                 colour = concept,
                 fill = concept,
                 shape = sample_type,
                 alpha = sample_type
                 ), size = 2, colour = 'black') + 
  scale_x_continuous(trans = 'log10', name = 'Spatial extent [km2]') +
  labs(y = expression(paste(Delta, beta, ' - diversity . ', year^-1))) +
    scale_color_manual(guide = 'none',
                       name = 'Occupancy change',
                       values = concept_colour) +
    scale_alpha_manual(guide = 'none',
                       name = 'Sample type',
                       values = c('checklist' = 1,
                                  'resurvey' = 0.8
                       )) +
    scale_fill_manual(guide = 'none',
                      name = 'Occupancy change',
                      values = concept_colour) +
    scale_shape_manual(guide = 'none',
                       name = 'Sample type', values = c('checklist' = 24,
                                                        'resurvey' = 21)) +
  theme_minimal()

# combine
anti_ts_combined <-
  plot_grid(col_shape_leg,
          plot_grid(anti_ts_beta_dt, 
                    anti_ts_beta_extent,
                    nrow = 1, labels = c('A', 'B')),
                                 rel_heights = c(0.4, 1),
                                 nrow = 2, align = 'hv') +
  cowplot::draw_label(label = 'Models fit to data with only two time points (n = 298)',
                      x = 0.01, y = 0.99, hjust = 0, vjust = 1)

ts_combined <-
  plot_grid(NULL,
            plot_grid(ts_beta_dt, 
                      ts_beta_extent,
                      nrow = 1, labels = c('C', 'D')),
            rel_heights = c(0.1, 1),
            nrow = 2, align = 'hv') +
  cowplot::draw_label(label = 'Models fit where time series data available (n = 229)',
                      x = 0.01, y = 0.99, hjust = 0, vjust = 1)

plot_grid(anti_ts_combined,
          ts_combined, nrow = 2)

ggsave('~/Dropbox/1current/spatial_composition_change/figures/results/FigS6.pdf',
       width = 184,  height = 180, units = 'mm')

