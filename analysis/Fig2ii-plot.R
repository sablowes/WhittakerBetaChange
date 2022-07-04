source('~/Dropbox/1current/spatial_composition_change/WhittakerBetaChange/analysis/Fig2i-wrangle.R')

concept_colour = c('Gain low occupancy' = '#61CDE0',
                   'Low occupancy replace high' = '#2CABA4',
                   'Lose high occupancy' = '#155F49',
                   'Lose low occupancy' = '#D17538',
                   'High occupancy replace low' = '#E9AE27',
                   'Gain high occupancy' = '#D9D956')

# counts of observed homogenisation and differentiation (no change is where empirical delta-alpha==delta-regional==0)
pattern_summary %>% 
  # filter((r05 > local_mu.hat & r95 < local_mu.hat) | (l05 > regional_mu.hat | l95 < regional_mu.hat)) %>%
  group_by(spatial_pattern_obs) %>% 
  summarise(n = n()) %>% 
  ungroup() 

# Some regions (n = 6) fall on boundary of categories:
# use model predictions to determine the colour for these regions in figure 2a
pattern_summary <- pattern_summary %>%
  mutate(concept_obs_plot = case_when(is.na(concept_obs) ~ concept,
                                      !is.na(concept_obs) ~ concept_obs))


full_concept <-
  ggplot() +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  geom_point(data = pattern_summary,# %>% 
             # filter((r05 > local_mu.hat & r95 < local_mu.hat) | (l05 > regional_mu.hat | l95 < regional_mu.hat)),
             aes(x = local_mu.i, y = regional_mu.i, 
                 colour = concept_obs_plot, shape = sample_type),
             size = 1) + #
  geom_point(data = overall_intercept,
             aes(x = local_intercept, y = regional_intercept), size = 1.25) +
  geom_linerange(data = overall_intercept,
                 aes(x = local_intercept, ymin = regional_Q05, ymax = regional_Q95)) +
  geom_linerange(data = overall_intercept,
                 aes(y = regional_intercept, xmin = local_Q05, xmax = local_Q95)) +
  scale_color_manual(guide = 'none', values = concept_colour) +
  scale_shape_manual(name = 'Sample type', values = c('checklist' = 17,
                                                      'resurvey' = 19)) +
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
        panel.grid = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))

x_density <- ggplot() +
  geom_density(data = pattern_summary,
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

# looks wonky
fig3a <- ggdraw() +
  draw_plot(combine2) +
  draw_image('~/Dropbox/1current/spatial_composition_change/figures/for-publication/concept_only_inset_white.png',
             x = -0.25, y = 0.2, 
             scale = 0.275) 

# but saves to file ok
# ggsave('~/Dropbox/1current/spatial_composition_change/figures/main_result.png',
#        width = 120, height = 100, units = 'mm')

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
overall_d <- bind_cols(gather_draws(local_ES_norm, b_Intercept) %>%
                         ungroup() %>%
                         select(x = .value),
                       gather_draws(regional_ES_norm, b_Intercept, n = 4000) %>%
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
  geom_label(aes(x = -0.003, y = 0.85, label = 'Homogenisation'),
             size = 3.5) +
  geom_label(aes(x = 0.00165, y = 0.85, label = 'Differentiation'),
             size = 3.5) +
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


dat4bar <- pattern_summary %>%
  group_by(concept) %>% #
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(group = 'model')

dat4bar_obs <- pattern_summary %>%
  group_by(concept_obs) %>% #
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(group = 'data') %>% 
  rename(concept = concept_obs) %>% 
  # remove NAs, fixed in next step
  filter(!is.na(concept))

# need to fix the six regions on boundary of categories
# here we add use fractions to determine overall count for each category
pattern_summary %>% 
  filter(is.na(concept_obs)) %>% 
  select(regional_level, spatial_pattern_obs, local_mu.i, regional_mu.i) 

dat4bar_obs <- dat4bar_obs %>% 
  mutate(n = case_when(concept=='Low occupancy replace high' ~ n + 1,
                       concept=='Lose high occupancy' ~ n + 1, # there are two cases that fall on the boundary of v and vi
                       concept=='Gain high occupancy' ~ n + 1.5,
                       concept=='High occupancy replace low' ~ n + 1.5, # three cases between i and ii
                       TRUE ~ as.numeric(n)),
         # add 1/6 to count for each category for two cases where deltaSalpha=deltaSgamma=0
         n = n + 2/6)



all_bar_dat <- bind_rows(dat4bar, dat4bar_obs) %>% 
  unite(concept2, c(concept, group), remove = FALSE)

all_bar_dat$concept2 <- factor(all_bar_dat$concept2,
                               levels = c('Gain high occupancy_data',
                                          'Gain high occupancy_model',
                                          'High occupancy replace low_data',
                                          'High occupancy replace low_model',
                                          'Lose low occupancy_data',
                                          'Lose low occupancy_model',
                                          'Lose high occupancy_data',
                                          'Lose high occupancy_model',
                                          'Low occupancy replace high_data',
                                          'Low occupancy replace high_model',
                                          'Gain low occupancy_data',
                                          'Gain low occupancy_model'), ordered = T,
                               labels = c('Gain high occupancy (data)',
                                          'Gain high occupancy (model)',
                                          'High occupancy replace low (data)',
                                          'High occupancy replace low (model)',
                                          'Lose low occupancy (data)',
                                          'Lose low occupancy (model)',
                                          'Lose high occupancy (data)',
                                          'Lose high occupancy (model)',
                                          'Low occupancy replace high (data)',
                                          'Low occupancy replace high (model)',
                                          'Gain low occupancy (data)',
                                          'Gain low occupancy (model)'))

dat4bar$concept <- factor(dat4bar$concept,
                          levels = c('Gain high occupancy',
                                     'High occupancy replace low',
                                     'Lose low occupancy',
                                     'Lose high occupancy',
                                     'Low occupancy replace high',
                                     'Gain low occupancy'), ordered = T)

dat4bar_obs <- dat4bar_obs %>% 
  mutate(concept_label  = case_when(concept == 'Gain high occupancy' ~  'i. Gain high occupancy',
                                    concept == 'High occupancy replace low' ~  'ii. High occupancy replace low',
                                    concept == 'Lose low occupancy' ~  'iii. Lose low occupancy',
                                    concept == 'Lose high occupancy' ~  'iv. Lose high occupancy',
                                    concept == 'Low occupancy replace high' ~  'v. Low occupancy replace high',
                                    concept == 'Gain low occupancy' ~  'vi. Gain low occupancy'))

dat4bar_obs$concept_label <- factor(dat4bar_obs$concept_label,
                              levels = c('i. Gain high occupancy',
                                         'ii. High occupancy replace low',
                                         'iii. Lose low occupancy',
                                         'iv. Lose high occupancy',
                                         'v. Low occupancy replace high',
                                         'vi. Gain low occupancy'), ordered = T)


fig3_concept_obs <-
  ggplot() +
  geom_bar(data = dat4bar_obs,
           aes(x = concept_label, y = n, fill = concept),#, fill = sample_type
           stat = 'identity') +
  geom_label(aes(x = 2, y = 50, label = 'Differentiation'),
             size = 3.5) +
  geom_label(aes(x = 5, y = 50, label = 'Homogenisation'),
             size = 3.5) +
  geom_vline(xintercept = 3.5, lty = 2, colour = '#bdbdbd') +
  scale_x_discrete(labels = label_wrap_gen(width = 19), limits = rev) +
  scale_fill_manual(guide = 'none', values = concept_colour) +
  labs(x = '',
       y = 'Number of regions') +
  coord_flip() +
  theme_minimal() +
  theme(panel.border = element_blank(),
    panel.grid.major.x  = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = 'white', color = 'white'),
    panel.background = element_blank(),
    legend.position = c(1,0),
    legend.justification = c(1,0),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.key.size = unit(8, units = 'pt'))

right_panel <- cowplot::plot_grid(fig3_concept_obs,
                                  fig3_beta, 
                                  ncol = 1,
                                  label_fontface = 'bold',
                                  label_size = 11,
                                  labels = c('b', 'c'))
cowplot::plot_grid(fig3a,
                   right_panel,
                   nrow = 1,
                   rel_heights = c(2, 0.1),
                   rel_widths = c(1, 0.5),
                   label_fontface = 'bold',
                   label_size = 11,
                   labels = 'a')



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
                 aes(xmin = l05, xmax = l95, y = fct_reorder(regional_level, local_mu.hat), colour = concept),
                 alpha = 0.5) +
  geom_point(data = pattern_summary,
             aes(x = local_mu.hat, y = fct_reorder(regional_level, local_mu.hat), 
                 colour = concept,
                 shape = sample_type)) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_vline(data = local_overall_post,
             aes(xintercept = .value)) +
  geom_rect(data = local_overall_post,
              aes(ymin = -Inf, ymax = Inf, xmin = .lower, xmax = .upper),
              alpha = 0.3) +
  scale_color_manual(guide = 'none', values = concept_colour) +
  scale_shape_manual(guide = 'none',
                     name = 'Sample type', values = c('checklist' = 17,
                                                      'resurvey' = 19)) +
  labs(x = expression(paste(Delta, alpha, '  [log(', S[t2], '/', S[t1],') . ', year^-1,']')),
       subtitle = expression(paste(Delta, alpha))) +
  coord_cartesian(clip = 'off') +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.subtitle = element_text(hjust = 0.5),
        plot.margin = margin(), 
        panel.grid = element_blank(),
        panel.background = element_blank())


gamma_forest <- ggplot() +
  geom_point(data = pattern_summary,
             aes(x = regional_mu.hat, y = fct_reorder(regional_level, local_mu.hat), 
                 colour = concept,
                 shape = sample_type)) +
  geom_linerange(data = pattern_summary,
                 aes(xmin = r05, xmax = r95, y = fct_reorder(regional_level, local_mu.hat), colour = concept),
                 alpha = 0.5) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_vline(data = regional_overall_post,
             aes(xintercept = .value)) +
  geom_rect(data = regional_overall_post,
            aes(ymin = -Inf, ymax = Inf, xmin = .lower, xmax = .upper),
            alpha = 0.3) +
  scale_color_manual(guide = 'none', values = concept_colour) +
  scale_shape_manual(guide = 'none',
                     name = 'Sample type', values = c('checklist' = 17,
                                                      'resurvey' = 19)) +
  labs(x = expression(paste(Delta, gamma, '  [log(', S[t2], '/', S[t1],') . ', year^-1,']')),
       subtitle = expression(paste(Delta, gamma))) +
  coord_cartesian(clip = 'off') +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.subtitle = element_text(hjust = 0.55),
        plot.margin = margin(r = 2),
        panel.grid = element_blank(),
        panel.background = element_blank())

regional_d_summary <- regional_d %>% 
  group_by(regional_level) %>% 
  summarise(d_mu = mean(d),
            lower = quantile(d, probs = 0.05),
            upper = quantile(d, prob = 0.95)) %>% 
  left_join(pattern_summary %>% 
            select(regional_level, concept, sample_type, local_mu.hat))

beta_forest <- ggplot() +
  geom_linerange(data = regional_d_summary,
                 aes(xmin = lower, xmax = upper, y = fct_reorder(regional_level, local_mu.hat), colour = concept),
                 alpha = 0.5) +
  geom_point(data = regional_d_summary,
             aes(x = d_mu, y = fct_reorder(regional_level, local_mu.hat), 
                 colour = concept,
                 shape = sample_type)) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_vline(data = overall_d,
             aes(xintercept = mean(d))) +
  geom_rect(data = overall_d %>% 
              summarise(lower = quantile(d, probs = 0.05),
                        upper = quantile(d, probs = 0.95)),
            aes(ymin = -Inf, ymax = Inf, xmin = lower, xmax = upper),
            alpha = 0.3) +
  scale_color_manual(guide = 'none', values = concept_colour) +
  scale_shape_manual(guide = 'none',
                     name = 'Sample type', values = c('checklist' = 17,
                                                      'resurvey' = 19)) +
  labs(x = expression(paste(Delta, beta, ' - diversity . ', year^-1)),
       subtitle = expression(paste(Delta, beta))) +
  coord_cartesian(clip = 'off') +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.subtitle = element_text(hjust = 0.52),
        plot.margin = margin(r = 2),
        panel.grid = element_blank(),
        panel.background = element_blank())

bottom_row <- plot_grid(alpha_forest,
          gamma_forest,
          beta_forest,
          nrow = 1,
          labels = c('d', 'e', 'f'),
          label_fontface = 'bold',
          label_size = 12)

top <- cowplot::plot_grid(fig3a,
                   right_panel,
                   nrow = 1,
                   rel_heights = c(2, 0.1),
                   rel_widths = c(1, 0.5),
                   label_fontface = 'bold',
                   label_size = 12,
                   labels = 'a')

plot_grid(top, bottom_row,
          nrow = 2,
          rel_heights = c(1, 0.66))


ggsave('~/Dropbox/MoBD (Measurements of Beta diversity)/Homogenization Paper/MS-Nature/figures/Fig3_white.pdf',
       width = 300,  height = 255, units = 'mm')
