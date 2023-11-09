source('~/Dropbox/1current/spatial_composition_change/WhittakerBetaChange/analysis/Fig2-1-wrangle.R')

# special concept colour for plotting points on the axes in grey
concept_colour_special = c('Gain low occupancy' = '#61CDE0',
                   'Low occupancy replace high' = '#2CABA4',
                   'Lose high occupancy' = '#155F49',
                   'Lose low occupancy' = '#D17538',
                   'High occupancy replace low' = '#E9AE27',
                   'Gain high occupancy' = '#D9D956',
                   # for points that fall on the boundary
                   'NA' = '#f0f0f0')

# counts of observed homogenisation and differentiation (no change is where empirical delta-alpha==delta-regional==0)
pattern_summary %>% 
  # count of homogenisation, differentiation, boundary cases
  group_by(spatial_pattern_obs) %>% 
  summarise(n = n()) %>% 
  ungroup() 

pattern_summary %>% 
  # count of homogenisation, differentiation, boundary cases
  group_by(sample_type) %>% 
  summarise(n = n()) %>% 
  ungroup() 

pattern_summary %>% 
  group_by(database) %>% 
  summarise(n = n()) %>% 
  ungroup() 


full_concept <-
  ggplot() +
  geom_hline(yintercept = 0, lty = 2, colour = '#969696') +
  geom_vline(xintercept = 0, lty = 2, colour = '#969696') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#969696') +
  geom_hline(data = overall_intercept,
             aes(yintercept = regional_intercept)) + 
  geom_vline(data = overall_intercept,
             aes(xintercept = local_intercept)) + 
  geom_rect(data = overall_intercept,
            aes(xmin = -Inf, xmax = Inf,
                ymin = regional_Q05, ymax = regional_Q95),
            alpha = 0.2) +
  geom_rect(data = overall_intercept,
            aes(ymin = -Inf, ymax = Inf,
                xmin = local_Q05, xmax = local_Q95),
            alpha = 0.2) +
  geom_point(data = pattern_summary %>% 
               filter(local_mu.i < 0.1 & sample_type == 'resurvey'),
             aes(x = local_mu.i, y = regional_mu.i, 
                 colour = concept_obs, 
                 fill = concept_obs,
                 shape = sample_type, 
                 alpha = sample_type),#
             size = 2, colour = 'black'
             ) + #
  geom_point(data = pattern_summary %>% 
               filter(local_mu.i < 0.1 & sample_type == 'checklist'),
             aes(x = local_mu.i, y = regional_mu.i, 
                 colour = concept_obs, 
                 fill = concept_obs,
                 shape = sample_type, 
                 alpha = sample_type),#
             size = 2, colour = 'black'
             ) + 
  scale_color_manual(guide = 'none', values = concept_colour_special) +
  scale_fill_manual(guide = 'none', values = concept_colour_special) +
  scale_alpha_manual(name = 'Sample type',
                     values = c('checklist' = 1,
                                'resurvey' = 0.4
                     )) +
  scale_shape_manual(name = 'Sample type', values = c('checklist' = 24,
                                                      'resurvey' = 21)) +
  labs(y = expression(paste(Delta, gamma, '  [log(', S[t2], '/', S[t1],') . ', year^-1,']')),
       x = expression(paste(Delta, alpha, '  [log(', S[t2], '/', S[t1],') . ', year^-1,']'))) +
  scale_x_continuous(breaks = c(-0.1,  -0.05, 0, 0.05, 0.1)) +
  scale_y_continuous(breaks = c(-0.1,  -0.05, 0, 0.05, 0.1)) +
  # coord_fixed() +
  theme_minimal() +
  theme(plot.background = element_rect(fill = 'white', color = 'white', linetype = 0), ##f4f4f4
        legend.position = c(1,0.1),
        legend.justification = c(1, 0),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_line(size = 0.5),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))


alt_inset_zoom <-
full_concept +
  scale_x_continuous(name = '', breaks = c(-0.01, 0, 0.01),
                     labels = c(-0.01, 0, 0.01)) +
  scale_y_continuous(name = '', breaks = c(-0.01, 0, 0.01),
                     labels = c(-0.01, 0, 0.01)) +
  coord_fixed(xlim = c(-0.01, 0.01),
              ylim = c(-0.01, 0.01)) +
  theme(legend.position = 'none',
        axis.text = element_text(size = 10, colour = 'dark grey'), 
        plot.background = element_blank(),
        # panel.background = element_blank(), 
        panel.border = element_blank(),
        plot.margin = unit(c(0,0,0,0), units = 'mm'))
# combine components
alt_A <-
  full_concept +  
  annotation_custom(ggplotGrob(alt_inset_zoom +
                                 # labs(subtitle = 'zoom inset') +
                                 scale_alpha_manual(name = 'Sample type',
                                                    values = c('checklist' = 0.5,
                                                               'resurvey' = 0.5
                                                    )) +
                                 theme(panel.border = element_rect(linetype = 1,
                                                                   fill = NA,
                                                                   colour = 'dark grey'))), 
                  xmin = -0.115, xmax = 0, ymin = 0.0275, ymax = 0.12)

  
# use full posterior of the overall intercept estimates to calculate average rate of 
# change in beta-diversity (d)
overall_d <- bind_cols(gather_draws(local_ES_norm_sigma2, b_Intercept, ndraws = 4000) %>%
                         ungroup() %>%
                         select(x = .value),
                       gather_draws(regional_ES_jk_norm_sigma2, b_Intercept, ndraws = 4000) %>%
                         ungroup() %>%
                         select(y = .value)) %>%
  mutate(d = y - x)

fig3_beta <-
ggplot() +
  stat_halfeye(data = overall_d,
               aes(d),
               point_interval = median_qi,
               .width = c(0.50, 0.9)) +
  geom_vline(xintercept = 0, lty = 2, colour = '#969696') +
  scale_x_continuous(breaks = seq(from = -0.006, to = 0, by = 0.001),
                     labels = c('-0.006', '', '', '-0.003', '', '', '0')) +
  labs(x = expression(paste(Delta, beta, ' - diversity . ', year^-1)),
       y = '') +
  theme_minimal() +
  theme(legend.position = c(0.1,0.9),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = 'white', color = 'white'),
        panel.grid = element_blank(),
        axis.line.x = element_line(colour = 'black'),
        axis.ticks.x = element_line(colour = 'black'),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.margin = unit(c(2,4,2,2), units = 'mm'))



# with zoom inset, and no density plots of ES (to better show variation in
# scatter plot)
plot_grid(alt_A, fig3_beta, 
          nrow = 1, labels = 'AUTO',
          rel_widths = c(1, 1))

ggsave('~/Dropbox/1current/spatial_composition_change/figures/results/Fig2.pdf',
       width = 184, height = 100, units = 'mm')

overall_d %>% 
  summarise(median(dS),
            quantile(dS, 0.05),
            quantile(dS, 0.95))
