# vertical three panael
# results from two-stage analysis

source('~/Dropbox/1current/spatial_composition_change/WhittakerBetaChange/analysis/Fig3i-wrangle.R')

sample_type <-
  ggplot() +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  # geom_point(data = pattern_summary,
  #            aes(x = local_mu.hat, y = regional_mu.hat, colour = concept, shape = sample_type)) +
  geom_linerange(data = sample_type_summary,
                 aes(x = local_median,
                     ymin = regional_Q05, ymax = regional_Q95)) +
  geom_linerange(data = sample_type_summary,
                 aes(y = regional_median,
                     xmin = local_Q05, xmax = local_Q95)) +
  geom_linerange(data = sample_type_summary,
                 aes(x = local_median,
                     ymin = regional_Q05, ymax = regional_Q95)) +
  geom_linerange(data = sample_type_summary,
                 aes(y = regional_median,
                     xmin = local_Q05, xmax = local_Q95)) +
  geom_point(data = sample_type_summary,
             aes(x = local_median,
                 y = regional_median, shape = .variable),
             size = 3) +
  scale_shape_manual(guide = 'none',
                     name = 'Sample type', values = c('Checklist' = 17,
                                                      'Resurvey' = 19),
                     labels = c(paste0('Checklist (n =', sample_type_label %>% 
                                         filter(sample_type=='Checklist') %>% pull(n), 
                                       ')'),
                                paste0('Resurvey (n =', sample_type_label %>% 
                                         filter(sample_type=='Resurvey') %>% pull(n), 
                                       ')'))) +
  scale_fill_grey() +
  # scale_x_continuous(breaks = c(0, 0.002, 0.004, 0.006)) +
  # scale_y_continuous(breaks = c(0, 0.002, 0.004, 0.006)) +
  labs(y = expression(paste(gamma-scale, ' effect size [log(S) / year]')),
       x = expression(paste(alpha-scale, ' effect size [log(S) / year]')),
       # tag = '(a)',
       subtitle = expression(paste(bold(c), '   Sample type'))) +
  # xlim(c(-0.025, 0.05)) +
  # ylim(c(-0.025, 0.05)) +
  coord_fixed() +
  theme_minimal() +
  theme(plot.background = element_rect(fill = 'white', color = 'white', linetype = 0),
        panel.grid = element_blank(),
        axis.text = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) 



realm <- ggplot() +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  # geom_point(data = pattern_summary,
  #            aes(x = local_mu.hat, y = regional_mu.hat, colour = concept, shape = sample_type)) +
  geom_linerange(data = realm_summary,
                 aes(x = local_median,
                     ymin = regional_Q05, ymax = regional_Q95, colour = .variable)) +
  geom_linerange(data = realm_summary,
                 aes(y = regional_median,
                     xmin = local_Q05, xmax = local_Q95, colour = .variable)) +
  geom_linerange(data = realm_summary,
                 aes(x = local_median,
                     ymin = regional_Q05, ymax = regional_Q95, colour = .variable)) +
  geom_linerange(data = realm_summary,
                 aes(y = regional_median,
                     xmin = local_Q05, xmax = local_Q95, colour = .variable)) +
  geom_point(data = realm_summary,
             aes(x = local_median,
                 y = regional_median, colour = .variable),
             size = 3) +
  scale_color_manual(name = 'Realm', values = realm_colour, guide = 'none',
                     labels = c(paste0('Freshwater (n =', realm_label %>% 
                                         filter(realm=='Freshwater') %>% pull(n), 
                                       ')'),
                                paste0('Marine (n =', realm_label %>% 
                                         filter(realm=='Marine') %>% pull(n), 
                                       ')'),
                                paste0('Terrestrial (n =', realm_label %>% 
                                         filter(realm=='Terrestrial') %>% pull(n), 
                                       ')'))) +
  # scale_x_continuous(breaks = c(0, 0.005, 0.01)) +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01)) +
  labs(y = expression(paste(gamma-scale, ' effect size [log(S) / year]')),
       x = expression(paste(alpha-scale, ' effect size [log(S) / year]')),
       # tag = 'a',
       subtitle = expression(paste(bold(a), '   Realm'))) +
  # xlim(c(-0.025, 0.05)) +
  # ylim(c(-0.025, 0.05)) +
  coord_fixed() +
  theme_minimal() +
  theme(plot.background = element_rect(fill = 'white', color = 'white', linetype = 0),
        panel.grid = element_blank(),
        axis.text = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) 


taxon <-
ggplot() +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  geom_linerange(data = taxon_summary,
                 aes(x = local_median,
                     ymin = regional_Q05, ymax = regional_Q95, colour = .variable)) +
  geom_linerange(data = taxon_summary,
                 aes(y = regional_median,
                     xmin = local_Q05, xmax = local_Q95, colour = .variable)) +
  geom_linerange(data = taxon_summary,
                 aes(x = local_median,
                     ymin = regional_Q05, ymax = regional_Q95, colour = .variable)) +
  geom_linerange(data = taxon_summary,
                 aes(y = regional_median,
                     xmin = local_Q05, xmax = local_Q95, colour = .variable)) +
  geom_point(data = taxon_summary,
             aes(x = local_median,
                 y = regional_median, colour = .variable),
             size = 2) +
  scale_color_manual(name = 'Taxon group', values = taxon_colour,
                     guide = 'none',
                     labels = c(paste0('Birds (n = ', 
                                       taxon_label %>% filter(taxon_mod=='Birds') %>% pull(n),
                                       ')'),
                                paste0('Fish (n = ', 
                                       taxon_label %>% filter(taxon_mod=='Fish') %>% pull(n),
                                       ')'),
                                paste0('Herpetofauna (n = ', 
                                       taxon_label %>% filter(taxon_mod=='Heerptofauna') %>% pull(n),
                                       ')'),
                                paste0('Invertebrates (n = ', 
                                       taxon_label %>% filter(taxon_mod=='Invertebrates') %>% pull(n),
                                       ')'),
                                paste0('Mammals (n = ', 
                                       taxon_label %>% filter(taxon_mod=='Mammals') %>% pull(n),
                                       ')'),
                                paste0('Multiple taxa (n = ', 
                                       taxon_label %>% filter(taxon_mod=='Multiple taxa') %>% pull(n),
                                       ')'),
                                paste0('Plants (n = ', 
                                       taxon_label %>% filter(taxon_mod=='Plants') %>% pull(n),
                                       ')'))) +
  labs(y = expression(paste(Delta, gamma, '  [log(', S[t2], '/', S[t1],') . ', year^-1, ']')),
       x = expression(paste(Delta, alpha, '  [log(', S[t2], '/', S[t1],') . ', year^-1, ']')),
       # tag = 'd',
       subtitle = expression(paste(bold(d), '   Taxon group'))) +
  # xlim(c(-0.025, 0.05)) +
  # ylim(c(-0.025, 0.05)) +
  coord_fixed() +
  theme_minimal() +
  theme(plot.background = element_rect(fill = 'white', color = 'white', linetype = 0),
        panel.grid = element_blank(),
        axis.text = element_text(size = 8),
        # axis.title.x = element_blank(),
        axis.title.y = element_blank()) 



lat4_summary$.variable <- factor(lat4_summary$.variable,
                                 levels = c('Polar', 'Temperate', 'Subtropical', 'Tropical'))

# combine hemisphere and latitudinal band
lat_4_concept <-
  ggplot() +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  geom_linerange(data = lat4_summary,
                 aes(x = local_median,
                     ymin = regional_Q05, ymax = regional_Q95, colour = .variable)) +
  geom_linerange(data = lat4_summary,
                 aes(y = regional_median,
                     xmin = local_Q05, xmax = local_Q95, colour = .variable)) +
  geom_linerange(data = lat4_summary,
                 aes(x = local_median,
                     ymin = regional_Q05, ymax = regional_Q95, colour = .variable)) +
  geom_linerange(data = lat4_summary,
                 aes(y = regional_median,
                     xmin = local_Q05, xmax = local_Q95, colour = .variable)) +
  geom_point(data = lat4_summary,
             aes(x = local_median,
                 y = regional_median, colour = .variable),
             size = 2, pch = 19) +
  scale_color_manual(name = 'Latitudinal band', 
                     values = lat4_colour,
                     guide = 'none',
                     labels = c(paste0('Polar (n = ',
                                       lat4_label %>% filter(lat_band_4=='Polar') %>% pull(n),
                                       ')'),
                                paste0('Subtropical (n = ',
                                       lat4_label %>% filter(lat_band_4=='Subtropical') %>% pull(n),
                                       ')'),
                                paste0('Temperate (n = ',
                                       lat4_label %>% filter(lat_band_4=='Temperate') %>% pull(n),
                                       ')'),
                                paste0('Tropical (n = ',
                                       lat4_label %>% filter(lat_band_4=='Tropical') %>% pull(n),
                                       ')'))) +
  # scale_x_continuous(breaks = c(-0.008, -0.004, 0, 0.004, 0.008)) +
  # scale_y_continuous(breaks = c(-0.008, -0.004, 0, 0.004, 0.008)) +
  labs(y = expression(paste(gamma-scale, ' effect size [log(S) / year]')),
       x = expression(paste(alpha-scale, ' effect size [log(S) / year]')),
       # tag = 'b',
       subtitle = expression(paste(bold(b), '   Latitudinal band'))) +
  coord_fixed() +
  theme_minimal() +
  theme(plot.background = element_rect(fill = 'white', color = 'white', linetype = 0),
        panel.grid = element_blank(),
        axis.text = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) 


# plots of distance to 1:1 line 
# function to calculate distance from 1:1 line of cartesian (x,y) coords
source('~/Dropbox/1current/spatial_composition_change/WhittakerBetaChange/analysis/dist2line.R')

sampleType_d <- bind_cols(local_sample_type %>% 
                            select(x_var = .variable,
                                   x = .value),
                          regional_sample_type %>% 
                            select(y_var = .variable,
                                   y = .value)) %>%
  filter(x_var==y_var) %>% 
  select(variable = x_var, x, y,
         -y_var) %>% 
  mutate(d = dist2line(x, y),
         variable = case_when(variable=='b_sample_typechecklist' ~ 'Checklist',
                              variable!='b_sample_typechecklist' ~ 'Resurvey'))

sample_d_plot <-
  ggplot() +
  stat_halfeye(data = sampleType_d,
               aes(x = d, y = factor(variable, levels = c( 'Resurvey', 'Checklist')), shape = variable),
               point_interval = median_qi,
               .width = c(0.50, 0.9)) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  # geom_label(aes(x = -0.005, y = 2.8, label = 'Homogenisation'),
  #            size = 2.5) +
  # geom_label(aes(x = 0.005, y = 2.8, label = 'Differentiation'),
  #            size = 2.5) +
  scale_shape_manual(guide = 'none',
                     name = 'Sample type', values = c('Checklist' = 17,
                                                      'Resurvey' = 19),
                     labels = c('Checklist (n = 49)',
                                'Resurvey (n = 166)')) +
  geom_text(data = sample_type_label,
            aes(x = 0.005, y = sample_type, label = paste0('n = ',n)),
            size = 2.5,
            nudge_y = 0.1) +
  labs(x = '',
       #tag = 'a'
       y = '') +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 10),
        plot.tag = element_text(size = 10, face = 'bold'),
        plot.margin = unit(c(0, 5, 0, 0), "mm"),
        panel.border = element_blank(),
        axis.line.x = element_line(colour = 'black'),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(vjust = 0, hjust = 1),
        plot.title = element_blank(),
        plot.subtitle = element_text(size = 7, hjust = -0.5))

realm_d <- bind_cols(local_realm %>% 
                       ungroup() %>% 
                       select(x_var = .variable,
                              x = .value),
                     regional_realm %>% 
                       ungroup() %>% 
                       select(y_var = .variable,
                              y = .value)) %>%
  filter(x_var==y_var) %>% 
  select(variable = x_var, x, y,
         -y_var) %>% 
  mutate(d = dist2line(x, y),
         variable = case_when(variable=='b_realmFreshwater' ~ 'Freshwater',
                              variable=='b_realmMarine' ~ 'Marine',
                              variable=='b_realmTerrestrial' ~ 'Terrestrial'))

realm_d$variable <- factor(realm_d$variable,
                           levels = c('Freshwater', 'Terrestrial',  'Marine'))
realm_d_plot <-
ggplot() +
  stat_halfeye(data = realm_d,
               aes(x = d, y = variable,
                   fill = variable),
               point_interval = median_qi,
               .width = c(0.50, 0.9)) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_label(aes(x = -0.007, y = 3.5, label = 'Homogenisation'),
             size = 2.5) +
  geom_label(aes(x = 0.008, y = 3.5, label = 'Differentiation'),
             size = 2.5) +
  geom_text(data = realm_label,
            aes(x = 0.01, y = realm, label = paste0('n = ',n)),
            size = 2.5,
            nudge_y = 0.1) +
  scale_fill_manual(name = 'Realm', values = realm_colour, guide = 'none') +
  scale_y_discrete(labels = scales::wrap_format(6)) +
  labs(x = '',
       y = '') +
  # coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 10),
        plot.tag = element_text(size = 10, face = 'bold'),
        plot.margin = unit(c(0, 5, 0, 0), "mm"),
        panel.border = element_blank(),
        axis.line.x = element_line(colour = 'black'),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(vjust = 0, hjust = 1),
        axis.text.x = element_text(angle = 0))

taxon_d <- bind_cols(local_taxon %>% 
                       select(x_var = .variable,
                              x = .value),
                     regional_taxon %>% 
                       select(y_var = .variable,
                              y = .value)) %>%
  filter(x_var==y_var) %>% 
  select(variable = x_var, x, y,
         -y_var) %>% 
  mutate(d = dist2line(x, y),
         variable = case_when(variable=='b_taxon_modBenthos' ~ 'Benthos',
                              variable=='b_taxon_modBirds' ~ 'Birds',
                              variable=='b_taxon_modFish' ~ 'Fish',
                              variable=='b_taxon_modHerpetofauna' ~ 'Herpetofauna',
                              variable=='b_taxon_modInvertebrates' ~ 'Invertebrates',
                              variable=='b_taxon_modMammals' ~ 'Mammals',
                              variable=='b_taxon_modMarineinvertebratesDplants' ~ 'Marine invertebrates/plants',
                              variable=='b_taxon_modMultipletaxa' ~ 'Multiple taxa',
                              variable=='b_taxon_modPlants' ~ 'Plants')) %>% 
  mutate(column = case_when(variable=='Benthos' ~ 1,
                            variable=='Multiple taxa' ~ 1,
                            variable=='Invertebrates' ~ 1,
                            variable=='Fish' ~ 1,
                            variable=='Birds' ~ 1,
                            variable=='Benthos' ~ 2,
                            variable=='Benthos' ~ 2,
                            variable=='Benthos' ~ 2,
                            variable=='Benthos' ~ 2))

taxon_d$variable <- factor(taxon_d$variable,
                           levels = c('Mammals',
                                      'Marine invertebrates/plants',
                                      'Herpetofauna',
                                      'Plants',
                                      'Birds',
                                      'Fish',
                                      'Invertebrates',
                                      'Multiple taxa',
                                      'Benthos'))

taxon_label <- taxon_label %>% 
  mutate(column = case_when(taxon_mod=='Benthos' ~ 1,
                            taxon_mod=='Multiple taxa' ~ 1,
                            taxon_mod=='Invertebrates' ~ 1,
                            taxon_mod=='Fish' ~ 1,
                            taxon_mod=='Birds' ~ 1,
                            taxon_mod=='Benthos' ~ 2,
                            taxon_mod=='Benthos' ~ 2,
                            taxon_mod=='Benthos' ~ 2,
                            taxon_mod=='Benthos' ~ 2))
taxon_d_plot <-
  ggplot() +
  facet_wrap(~column, ncol = 2, scale = 'free_y') + 
  stat_halfeye(data = taxon_d,
               aes(x = d, y = variable,
                   fill = variable),
               point_interval = median_qi,
               .width = c(0.50, 0.9)) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_text(data = taxon_label,
            aes(x = 0.025, y = taxon_mod, label = paste0('n = ',n)),
            size = 2.5,
            nudge_y = 0.1) +
  scale_fill_manual(name = 'Taxon group', values = taxon_colour,
                    guide = 'none') +
  scale_y_discrete(labels = scales::wrap_format(9)) +
  labs(x = expression(paste(Delta, beta, ' - diversity . ', year^-1)),
       y = '') +
  # coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 10),
        plot.tag = element_text(size = 10, face = 'bold'),
        plot.margin = unit(c(0, 5, 0, 0), "mm"),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(colour = 'black'),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 8, vjust = -1, hjust = 1, margin = margin(r = -50)),
        axis.text.x = element_text(angle = 0, size = 8))#vjust = grid::unit(c(0,-1,0,-6,-6,-6,0,-1,0), units = 'mm')

latitude_d <- bind_cols(local_lat4 %>% 
                          select(x_var = .variable,
                                 x = .value),
                        regional_lat4 %>% 
                          select(y_var = .variable,
                                 y = .value)) %>%
  filter(x_var==y_var) %>% 
  select(variable = x_var, x, y,
         -y_var) %>% 
  mutate(d = dist2line(x, y),
         variable = case_when(variable=='b_lat_band_4polar' ~ 'Polar',
                              variable=='b_lat_band_4subtropical' ~ 'Subtropical',
                              variable=='b_lat_band_4temperate' ~ 'Temperate',
                              variable=='b_lat_band_4tropical' ~ 'Tropical'))

latitude_d$variable <- factor(latitude_d$variable,
                              levels = c('Tropical',
                                         'Subtropical', 
                                         'Temperate',
                                         'Polar'))
lat4_d_plot <- ggplot() +
  stat_halfeye(data = latitude_d,
               aes(x = d, y = variable,
                   fill = variable),
               point_interval = median_qi,
               .width = c(0.50, 0.9)) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_text(data = lat4_label,
            aes(x = 0.01, y = lat_band_4, label = paste0('n = ',n)),
            size = 2.5,
            nudge_y = 0.1) +
  scale_fill_manual(name = 'Latitudinal band', values = lat4_colour,
                    guide = 'none') +
  scale_y_discrete(labels = scales::wrap_format(6)) +
  labs(x = '',
       y = '') +
  # coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 10),
        plot.tag = element_text(size = 10, face = 'bold'),
        plot.margin = unit(c(0, 5, 0, 0), "mm"),
        panel.border = element_blank(),
        axis.line.x = element_line(colour = 'black'),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(vjust = 0, hjust = 1),
        axis.text.x = element_text(angle = 0))


panel_a <- plot_grid(realm, realm_d_plot,
          nrow = 1)
panel_b <- plot_grid(lat_4_concept, lat4_d_plot,
          nrow = 1)
panel_c <- plot_grid(sample_type, sample_d_plot,
                     nrow = 1)
panel_d <- plot_grid(taxon, taxon_d_plot,
          nrow = 1, rel_widths = c(1,1.5))

plot_grid(panel_a,
          panel_b,
          panel_c,
          panel_d,
          ncol = 1,
          align = 'hv') +
  cowplot::draw_label(label = expression(paste(Delta, gamma, ' [log(', S[t2], '/', S[t1],') . ', year^-1, ']')),
                      x = 0.02, angle = 90) 

ggsave('~/Dropbox/MoBD (Measurements of Beta diversity)/Homogenization Paper/Draft of Manuscript/figures/Fig4.pdf',
       height = 290, width = 200, units = 'mm')

