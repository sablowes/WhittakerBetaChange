# code to plot Fig. S3
# wrangle posterior
source('~/Dropbox/1current/spatial_composition_change/WhittakerBetaChange/analysis/Fig2-1-wrangle.R')
# function to calculate distance from 1:1 line
source('~/Dropbox/1current/spatial_composition_change/WhittakerBetaChange/analysis/dist2line.R')

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
        plot.subtitle = element_text(size = 14, hjust = 0.38),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.margin = margin(t = 4, l = 2), 
        # panel.grid.major.x = element_blank(),
        # panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
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
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.margin = margin(t = 4, r = 2),
        # panel.grid.major.x = element_blank(),
        # panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank())

# use full posterior of the overall average rates of changes at the alpha- 
# and gamma-scales to estimate average beta-scale change
overall_d <- bind_cols(gather_draws(local_ES_norm_sigma2, b_Intercept, ndraws = 4000) %>%
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
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.subtitle = element_text(size = 14, hjust = 0.6),
        plot.margin = margin(t = 4, r = 10),
        # panel.grid.major.x = element_blank(),
        # panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank())

# combine
plot_grid(alpha_forest,
            gamma_forest,
            beta_forest,
            nrow = 1,
            labels = 'AUTO',
            label_fontface = 'bold',
            label_size = 12)

ggsave('~/Dropbox/1current/spatial_composition_change/figures/results/FigS3.pdf',
       width = 184,  height = 184, units = 'mm')
