# standardised by duration (per year change in log(S))
source('~/Dropbox/1current/spatial_composition_change/WhittakerBetaChange/analysis/Fig2i-wrangle.R')

load('~/Dropbox/1current/spatial_composition_change/results/allLRR_meta.Rdata')
dt <- local_LRR %>% 
  distinct(database, sample_type, regional_level, dt)

dt %>% 
  group_by(sample_type) %>% 
  summarise(min_dt = min(dt),
            max_dt = max(dt),
            median_dt = median(dt))

pattern_summary %>% 
  filter(is.na(gamma_sum_grains_km2) & is.na(gamma_bounding_box_km2))

pattern_summary %>% 
  filter(is.na(gamma_bounding_box_km2))

# some checklists have had their extents calculated as the sum of the grains (e.g., islands)
pattern_summary <- pattern_summary %>% 
  mutate(gamma_extent_km2 = case_when(is.na(gamma_bounding_box_km2) ~ gamma_sum_grains_km2,
                                      TRUE ~ as.numeric(gamma_bounding_box_km2)))



pattern_summary$concept_obs <- factor(pattern_summary$concept_obs,
                                      levels = c('Gain high occupancy',
                                                 'High occupancy replace low',
                                                 'Lose low occupancy',
                                                 'Lose high occupancy',
                                                 'Low occupancy replace high',
                                                 'Gain low occupancy'), ordered = T)

concept_colour = c('Gain low occupancy' = '#61CDE0',
                   'Low occupancy replace high' = '#2CABA4',
                   'Lose high occupancy' = '#155F49',
                   'Lose low occupancy' = '#D17538',
                   'High occupancy replace low' = '#E9AE27',
                   'Gain high occupancy' = '#D9D956')


# colour points by empirical observations
beta_spat_temp_scale <-
pattern_summary %>% 
  left_join(dt) %>% 
  filter(gamma_extent_km2 > 0) %>% 
  # remove two cases where observed local and regional scale changes were zero
  filter(spatial_pattern_obs!='no change beta-diversity') %>% 
  ggplot() +
  # vertical line for continental US and germany land area
  geom_vline(xintercept = c(357386, 8080464.3), lty = 2) +
  geom_label(aes(x = 8080464.3, y = 85, label = 'Area of\ncontinental USA'),
             size = 3.5, fill = NA) +
  geom_label(aes(x = 357386, y = 40, label = 'Area of Germany'), 
             size = 3.5) +
  geom_point(aes(x = gamma_extent_km2, y = dt, #fill = concept_obs, 
                 colour = spatial_pattern_obs, shape = sample_type),
             # position = position_jitter(width = 0.01, height = 0),
             size = 3) +
  scale_x_continuous(trans = 'log10', name = 'Spatial extent [km2]') +
  scale_y_continuous(trans = 'log10', name = 'Temporal duration [years]') +
  scale_shape_manual(#guide = F,
                     name = 'Sample type', values = c('checklist' = 17,
                                                      'resurvey' = 19)) +
  scale_colour_manual(name = 'Spatial pattern',
                      values = c('differentiation' = 'grey',
                                   'homogenisation' = 'black')) +#,'no change beta-diversity' = 'light grey'
  theme_bw() +
  theme(legend.position = 'top',
        legend.direction = 'horizontal')



alpha_temp_scale <- pattern_summary %>% 
  left_join(dt) %>% 
  # remove two cases where observed local and regional scale changes were zero
  filter(spatial_pattern_obs!='no change beta-diversity') %>% 
  ggplot() +
  geom_point(aes(x = dt, y = local_mu.i, colour = spatial_pattern_obs,
                 shape = sample_type)) +
  geom_abline(intercept = 0, slope = 0, lty = 2) +
  scale_x_continuous(trans = 'log10', name = 'Temporal duration [years]') +
  # labs(y = expression(paste('log', bgroup('(',frac(S[t2], S[t1]),')'))),#, ' . ', year^-1
  #      subtitle = expression(paste(Delta, S[alpha]))) +
  labs(y = expression(paste('log(S) / year')),#, ' . ', duration^-1
       subtitle = expression(paste(Delta, alpha))) +
  scale_shape_manual(#guide = F,
    name = 'Sample type', values = c('checklist' = 17,
                                     'resurvey' = 19))  +
  scale_colour_manual(name = 'Spatial pattern',
                      values = c('differentiation' = 'grey',
                                 'homogenisation' = 'black',
                                 'no change beta-diversity' = 'light grey')) +
  theme_bw() +
  theme(legend.position = 'none')



gamma_temp_scale <- pattern_summary %>% 
  left_join(dt) %>% 
  # remove two cases where observed local and regional scale changes were zero
  filter(spatial_pattern_obs!='no change beta-diversity') %>% 
  ggplot() +
  geom_point(aes(x = dt, y = regional_mu.i, colour = spatial_pattern_obs,
                 shape = sample_type)) +
  geom_abline(intercept = 0, slope = 0, lty = 2) +
  scale_x_continuous(trans = 'log10', name = 'Temporal duration [years]') +
  # labs(y = expression(paste('log', bgroup('(',frac(S[t2], S[t1]),')'))),#, ' . ', year^-1)
  #      subtitle = expression(paste(Delta, S[gamma]))) +
  labs(y = expression(paste('log(S) / year')),#, ' . ', duration^-1
       subtitle = expression(paste(Delta, gamma))) +
  scale_shape_manual(#guide = F,
    name = 'Sample type', values = c('checklist' = 17,
                                     'resurvey' = 19)) +
  scale_colour_manual(name = 'Spatial pattern',
                      values = c('differentiation' = 'grey',
                                 'homogenisation' = 'black')) + #'no change beta-diversity' = 'light grey'
  theme_bw() +
  theme(legend.position = 'none')

# spatial scale (extent)
alpha_spatial_scale <- pattern_summary %>% 
  # remove two cases where observed local and regional scale changes were zero
  filter(spatial_pattern_obs!='no change beta-diversity' & gamma_extent_km2 > 0) %>% 
  ggplot() +
  geom_point(aes(x = gamma_extent_km2, y = local_mu.i, colour = spatial_pattern_obs,
                 shape = sample_type)) +
  geom_abline(intercept = 0, slope = 0, lty = 2) +
  scale_x_continuous(trans = 'log10', name = 'Spatial extent [km2]') +
  labs(y = expression(paste('log(S) / year')),#, ' . ', duration^-1
       subtitle = expression(paste(Delta, alpha))) +
  scale_shape_manual(#guide = F,
    name = 'Sample type', values = c('checklist' = 17,
                                     'resurvey' = 19))  +
  scale_colour_manual(name = 'Spatial pattern',
                      values = c('differentiation' = 'grey',
                                 'homogenisation' = 'black',
                                 'no change beta-diversity' = 'light grey')) +
  theme_bw() +
  theme(legend.position = 'none')



gamma_spatial_scale <- pattern_summary %>% 
  # remove two cases where observed local and regional scale changes were zero
  filter(spatial_pattern_obs!='no change beta-diversity' & gamma_extent_km2 > 0) %>% 
  ggplot() +
  geom_point(aes(x = gamma_extent_km2, y = regional_mu.i, colour = spatial_pattern_obs,
                 shape = sample_type)) +
  geom_abline(intercept = 0, slope = 0, lty = 2) +
  scale_x_continuous(trans = 'log10', name = 'Spatial extent [km2]') +
  labs(y = expression(paste('log(S) / year')),#, ' . ', duration^-1
       subtitle = expression(paste(Delta, gamma))) +
  scale_shape_manual(#guide = F,
    name = 'Sample type', values = c('checklist' = 17,
                                     'resurvey' = 19)) +
  scale_colour_manual(name = 'Spatial pattern',
                      values = c('differentiation' = 'grey',
                                 'homogenisation' = 'black')) + #'no change beta-diversity' = 'light grey'
  theme_bw() +
  theme(legend.position = 'none')

plot_grid(beta_spat_temp_scale,
          plot_grid(alpha_temp_scale,
                    gamma_temp_scale,
                    ncol = 2, 
                    labels = c('b', 'c')),
          plot_grid(alpha_spatial_scale,
                    gamma_spatial_scale,
                    ncol = 2, 
                    labels = c('d', 'e')),
          ncol = 1,
          align = 'hv', 
          labels = 'a')

# ggsave('~/Dropbox/1current/spatial_composition_change/figures/extent_meta.png',
#        width = 290, height = 200, units = 'mm')

ggsave('~/Dropbox/MoBD (Measurements of Beta diversity)/Homogenization Paper/Draft of Manuscript/figures/ExtendedData-fig3-scale.pdf',
       width = 290, height = 240, units = 'mm')

ggsave('~/Dropbox/MoBD (Measurements of Beta diversity)/Homogenization Paper/Draft of Manuscript/figures/ExtendedData-fig3-scale.png',
       width = 290, height = 240, units = 'mm')

# can we show the full occupancy concept visually?
pattern_summary %>% 
  left_join(dt) %>% 
  filter(gamma_extent_km2 > 0) %>% 
  ggplot() +
  geom_point(aes(x = gamma_extent_km2, y = dt, fill = concept_obs, 
                 colour = spatial_pattern_obs, shape = sample_type),
             size = 3) +
  scale_x_continuous(trans = 'log10', name = 'Spatial extent [km2]') +
  scale_y_continuous(trans = 'log10', name = 'Temporal duration [years]') +
  scale_shape_manual(#guide = F,
    name = 'Sample type', values = c('checklist' = 24,
                                     'resurvey' = 21)) +
  scale_colour_manual(guide = FALSE, values = c('differentiation' = 'white',
                                                'homogenisation' = 'black')) +
  scale_fill_manual(values = concept_colour) +
  theme_minimal() +
  theme(legend.position = 'top',
        legend.direction = 'horizontal') +
  guides(fill = guide_legend(title = 'Occupancy change', override.aes = list(shape = 21, stroke = 0)))
