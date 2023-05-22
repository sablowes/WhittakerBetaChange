source('~/Dropbox/1current/spatial_composition_change/WhittakerBetaChange/analysis/Fig2-1-wrangle.R')

concept_colour = c('Gain low occupancy' = '#61CDE0',
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

# Some regions (n = 5) fall on boundary of categories, colour these points
# grey

full_concept <-
  ggplot() +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
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
             size = 2, colour = 'black') + #
  geom_point(data = pattern_summary %>% 
               filter(local_mu.i < 0.1 & sample_type == 'checklist'),
             aes(x = local_mu.i, y = regional_mu.i, 
                 colour = concept_obs, 
                 fill = concept_obs,
                 shape = sample_type, 
                 alpha = sample_type),#
             size = 2, colour = 'black') + #
  # geom_point(data = overall_intercept,
  #            aes(x = local_intercept, y = regional_intercept), size = 2.25) +
  # geom_linerange(data = overall_intercept,
  #                aes(x = local_intercept, ymin = regional_Q05, ymax = regional_Q95)) +
  # geom_linerange(data = overall_intercept,
  #                aes(y = regional_intercept, xmin = local_Q05, xmax = local_Q95)) +
  # 
  scale_color_manual(guide = 'none', values = concept_colour) +
  scale_fill_manual(guide = 'none', values = concept_colour) +
  scale_alpha_manual(name = 'Sample type',
                     values = c('checklist' = 1,
                                'resurvey' = 0.3
                     )) +
  scale_shape_manual(name = 'Sample type', values = c('checklist' = 24,
                                                      'resurvey' = 21)) +
  labs(y = expression(paste(Delta, gamma, '  [log(', S[t2], '/', S[t1],') . ', year^-1,']')),
       x = expression(paste(Delta, alpha, '  [log(', S[t2], '/', S[t1],') . ', year^-1,']'))) +
    # annotate(geom = 'text', 
    #          label = expression(paste(Delta, italic(S)[gamma])),
    #          x = 0, y = Inf, parse = T,
    #          hjust = 1.25, vjust = 1) +
    # annotate(geom = 'text', 
    #          label = expression(paste(Delta, italic(S)[alpha])),
    #          x = Inf, y = 0, parse = T,
    #          hjust = 1,
    #          vjust = 1.25
    # ) + 
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
        panel.grid.minor = element_line(size = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))

x_density <- ggplot() +
  geom_density(data = pattern_summary %>% filter(local_mu.i < 0.1),
               aes(x = local_mu.i), 
               fill = 'grey', colour = 'grey',
               alpha = 0.75) +
  scale_x_continuous(breaks = c(-0.05,  -0.025, 0, 0.025, 0.05)) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.background = element_rect(fill = 'white', color = 'white', linetype = 0),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 11))

y_density <- ggplot() +
  geom_density(data = pattern_summary,
               aes(x = regional_mu.i),
               fill = 'grey', colour = 'grey',
               alpha = 0.75) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  scale_x_continuous(breaks = c(-0.05,  -0.025, 0, 0.025, 0.05)) +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.background = element_rect(fill = 'white', color = 'white', linetype = 0),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10))

combine1 <- insert_xaxis_grob(full_concept, x_density, position = "top")
combine2 <- insert_yaxis_grob(combine1, y_density, position = "right")

# combine components
fig3a <- ggdraw() +
  draw_plot(combine2) +
  draw_image('~/Dropbox/1current/spatial_composition_change/figures/for-publication/concept_only_inset_white.png',
             x = -0.25, y = 0.2, 
             scale = 0.275) 

dist2line = function(x, y, a = 1, b = -1){
  # calculate distance from line: ax + by + c 
  # to vector of points(x, y)
  # NB: here line is 1:1 so c = 0, a = 1, b = -1
  
  numerator = -(a*x + b*y) # negative distance means point below line 
                           # (homogenisation change beta-diversity < 0)
  denom = sqrt(a^2 + b^2)
  return(numerator / denom)
}

# use full posterior of the overall intercept estimates estimates
overall_d <- bind_cols(gather_draws(local_ES_norm_sigma2, b_Intercept) %>%
                         ungroup() %>%
                         select(x = .value),
                       gather_draws(regional_ES_jk_norm_sigma2, b_Intercept, ndraws = 4000) %>%
                         ungroup() %>%
                         select(y = .value)) %>%
  mutate(d = dist2line(x, y))

fig3_beta <-
ggplot() +
  stat_halfeye(data = overall_d,
               aes(d),
               point_interval = median_qi,
               .width = c(0.50, 0.9)) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_label(aes(x = -0.003, y = 0.9, label = 'Homogenisation'),
             size = 3.5) +
  # geom_label(aes(x = 0.00035, y = 0.85, label = 'Differentiation'),
  #            size = 3.5) +
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
        axis.title = element_text(size = 12),
        plot.margin = unit(c(2,4,2,2), units = 'mm'))


# alternate simple version: concept + beta-diversity
top_simple <- plot_grid(fig3a, fig3_beta, 
          nrow = 1, labels = 'auto',
          rel_widths = c(1, 1))

# forest plots (i.e., model estimates at each scale for each region)
# need to calculate distance for each region
regional_d <- bind_cols(local_posterior_ES %>% 
                          select(regional_level, local_dS) %>% 
                          group_by(regional_level) %>% 
                          sample_n(1000) %>% 
                          ungroup() %>% 
                          rename(regional.x = regional_level),
                        regional_posterior %>% 
                          select(regional_level, regional_dS) %>% 
                          group_by(regional_level) %>% 
                          sample_n(1000) %>% 
                          ungroup() %>% 
                          rename(regional.y = regional_level)) %>%
  filter(regional.x==regional.y) %>% 
  mutate(d = dist2line(local_dS, regional_dS)) %>% 
  rename(regional_level = regional.x) %>% 
  select(-regional.y)

alpha_forest <- ggplot() +
  geom_linerange(data = pattern_summary,
                 aes(xmin = l05, xmax = l95, 
                     y = fct_reorder(regional_level, local_mu.hat)),
                 alpha = 0.3) +
  geom_point(data = pattern_summary,
             aes(x = local_mu.hat, 
                 y = fct_reorder(regional_level, local_mu.hat), 
                 shape = sample_type),
             alpha = 0.5,
             colour = 'black') +
  geom_vline(xintercept = 0, lty = 2) +
  geom_vline(data = local_overall_post,
             aes(xintercept = .value)) +
  geom_rect(data = local_overall_post,
            aes(ymin = -Inf, ymax = Inf, xmin = .lower, xmax = .upper),
            alpha = 0.3) +
  scale_color_manual(guide = 'none', values = concept_colour) +
  scale_fill_manual(guide = 'none', values = concept_colour) +
  scale_shape_manual(guide = 'none',
                     name = 'Sample type', values = c('checklist' = 24,
                                                      'resurvey' = 21)) +
  labs(x = expression(paste(Delta, alpha, '  [log(', S[t2], '/', S[t1],') . ', year^-1,']')),
       y = 'Data set',
       subtitle = expression(paste(Delta, alpha))) +
  coord_cartesian(clip = 'off') +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        # axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.subtitle = element_text(size = 12, hjust = 0.38),
        plot.margin = margin(t = 4, l = 2), 
        panel.grid = element_blank(),
        panel.background = element_blank())


gamma_forest <- ggplot() +
  geom_linerange(data = pattern_summary,
                 aes(xmin = r05, xmax = r95, 
                     y = fct_reorder(regional_level, local_mu.hat)),
                 alpha = 0.3) +
  geom_point(data = pattern_summary,
             aes(x = regional_mu.hat, 
                 y = fct_reorder(regional_level, local_mu.hat),
                 shape = sample_type),
             colour = 'black',
             alpha = 0.5) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_vline(data = regional_overall_post,
             aes(xintercept = .value)) +
  geom_rect(data = regional_overall_post,
            aes(ymin = -Inf, ymax = Inf, xmin = .lower, xmax = .upper),
            alpha = 0.3) +
  scale_color_manual(guide = 'none', values = concept_colour) +
  scale_fill_manual(guide = 'none', values = concept_colour) +
  scale_shape_manual(guide = 'none',
                     name = 'Sample type', values = c('checklist' = 24,
                                                      'resurvey' = 21)) +
  labs(x = expression(paste(Delta, gamma, '  [log(', S[t2], '/', S[t1],') . ', year^-1,']')),
       subtitle = expression(paste(Delta, gamma))) +
  coord_cartesian(clip = 'off') +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        plot.margin = margin(t = 4, r = 2),
        panel.grid = element_blank(),
        panel.background = element_blank())

# use full posterior of the overall intercept estimates estimates
overall_d <- bind_cols(gather_draws(local_ES_norm_sigma2, b_Intercept) %>%
                         ungroup() %>%
                         select(x = .value),
                       gather_draws(regional_ES_jk_norm_sigma2, b_Intercept, ndraws = 4000) %>%
                         ungroup() %>%
                         select(y = .value)) %>%
  mutate(d = dist2line(x, y))

regional_d_summary <- regional_d %>% 
  group_by(regional_level) %>% 
  summarise(d_mu = mean(d),
            lower = quantile(d, probs = 0.05),
            upper = quantile(d, prob = 0.95)) %>% 
  left_join(pattern_summary %>% 
              select(regional_level, concept, local_mu.hat, sample_type))

beta_forest <- ggplot() +
  geom_linerange(data = regional_d_summary,
                 aes(xmin = lower, xmax = upper, 
                     y = fct_reorder(regional_level, local_mu.hat)),
                 alpha = 0.3) +
  geom_point(data = regional_d_summary,
             aes(x = d_mu, 
                 y = fct_reorder(regional_level, local_mu.hat),
                 shape = sample_type), 
             alpha = 0.5, 
             colour = 'black') +
  geom_vline(xintercept = 0, lty = 2) +
  geom_vline(data = overall_d,
             aes(xintercept = mean(d))) +
  geom_rect(data = overall_d %>% 
              summarise(lower = quantile(d, probs = 0.05),
                        upper = quantile(d, probs = 0.95)),
            aes(ymin = -Inf, ymax = Inf, xmin = lower, xmax = upper),
            alpha = 0.3) +
  scale_color_manual(guide = 'none', values = concept_colour) +
  scale_fill_manual(guide = 'none', values = concept_colour) +
  scale_alpha_manual(guide = 'none',
                     name = 'Sample type',
                     values = c('checklist' = 1,
                                'resurvey' = 0.3
                     )) +
  scale_shape_manual(guide = 'none',
                     name = 'Sample type', values = c('checklist' = 24,
                                                      'resurvey' = 21)) +
  labs(x = expression(paste(Delta, beta, ' - diversity . ', year^-1)),
       subtitle = expression(paste(Delta, beta))) +
  coord_cartesian(clip = 'off') +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.subtitle = element_text(size = 12, hjust = 0.6),
        plot.margin = margin(t = 4, r = 10),
        panel.grid = element_blank(),
        panel.background = element_blank())

# code for these figures currently in Fig-Sx-y.R
bottom_row <-
plot_grid(alpha_forest,
          gamma_forest,
          beta_forest,
          nrow = 1,
          labels = c('c', 'd', 'e'),
          label_fontface = 'bold',
          label_size = 12)

# top <-
# cowplot::plot_grid(fig3a,
#                    right_panel,
#                    nrow = 1,
#                    rel_heights = c(2, 0.1),
#                    # rel_widths = c(1, 0.5),
#                    label_fontface = 'bold',
#                    label_size = 12,
#                    labels = 'a')

plot_grid(top_simple, bottom_row,
          nrow = 2,
          rel_heights = c(1, 1.5))

# local
ggsave('~/Dropbox/1current/spatial_composition_change/figures/results/Fig2.pdf',
       width = 300,  height = 255, units = 'mm')
