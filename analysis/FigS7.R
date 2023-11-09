## figure S7: relationships between richness and diversity change
## across scales (alpha, gamma, beta)
source('~/Dropbox/1current/spatial_composition_change/WhittakerBetaChange/analysis/Fig3-1-wrangle.R')

# alpha scale
alpha_metrics <- ggplot() + 
  geom_hline(yintercept = 0, lty = 2, colour = '#969696') +
  geom_vline(xintercept = 0, lty = 2, colour = '#969696') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#969696') +
  geom_vline(xintercept = 0, lty = 2) + 
  mapply(function(level) {
    stat_ellipse(data = two_scales,
                 aes(x = local_S_PIE + local_S_PIE_global,
                     y = local_S + local_S_global,
                     fill = 'alpha-scale'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .25,
                 level = level)
  }, 
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.45)) +
  geom_point(data = slopes_regional_variation %>% 
               unnest(cols = c(local_S, local_S_PIE)) %>% 
               bind_cols(slopes_global_level %>% 
                           slice(rep(1:n(), times = nrow(regional_levels)))) %>% 
               group_by(regional_level) %>% 
               summarise(S_PIE = median(local_S_PIE + local_S_PIE_global),
                         S = median(local_S + local_S_global)),
             aes(x = S_PIE, 
                 y = S, 
                 colour = 'alpha-scale'#, shape = 'alpha-scale'
             )) +
  scale_colour_manual(guide = 'none',
                      name = 'Scale',
                      values = c('alpha-scale' = '#001c6f',
                                 'beta-scale' = '#ffa600',
                                 'gamma-scale' = '#cf1762'),
                      labels = c(expression(paste(alpha-scale)),
                                 expression(paste(beta-scale)),
                                 expression(paste(gamma-scale)))) +
  scale_fill_manual(guide = 'none',
                    name = 'Scale',
                    values = c('alpha-scale' = '#001c6f',
                               'beta-scale' = '#ffa600',
                               'gamma-scale' = '#cf1762'),
                    labels = c(expression(paste(alpha-scale)),
                               expression(paste(beta-scale)),
                               expression(paste(gamma-scale)))) +
  scale_x_continuous(breaks = c(-0.06, -0.04, -0.02, 0, 0.02, 0.04, 0.06),
                     labels = c(-0.06, '', -0.02, 0, 0.02, '', 0.06)) +
  scale_y_continuous(breaks = c(-0.06, -0.04, -0.02, 0, 0.02, 0.04, 0.06),
                     labels = c(-0.06, -0.04, -0.02, 0, 0.02, 0.04, 0.06)) +
  labs(subtitle = expression(paste(alpha, '-scale')),
       x = '',
       y = 'Change in richness') +
  theme_minimal()

beta_metrics <- ggplot() + 
  geom_hline(yintercept = 0, lty = 2, colour = '#969696') +
  geom_vline(xintercept = 0, lty = 2, colour = '#969696') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#969696') +
  geom_vline(xintercept = 0, lty = 2) + 
  mapply(function(level) {
    stat_ellipse(data = regional_d,
                 aes(x = d_S_PIE,
                     y = dS, fill = 'beta-scale'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .25,
                 level = level)
  }, 
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.45)) +
  geom_point(data = regional_d %>% 
               group_by(regional_level) %>% 
               summarise(S_PIE = median(d_S_PIE),
                         S = median(dS)),
             aes(x = S_PIE, 
                 y = S, 
                 colour = 'beta-scale'#, shape = 'alpha-scale'
             )) +
  scale_colour_manual(guide = 'none',
                      name = 'Scale',
                      values = c('alpha-scale' = '#001c6f',
                                 'beta-scale' = '#ffa600',
                                 'gamma-scale' = '#cf1762'),
                      labels = c(expression(paste(alpha-scale)),
                                 expression(paste(beta-scale)),
                                 expression(paste(gamma-scale)))) +
  scale_fill_manual(guide = 'none',
                    name = 'Scale',
                    values = c('alpha-scale' = '#001c6f',
                               'beta-scale' = '#ffa600',
                               'gamma-scale' = '#cf1762'),
                    labels = c(expression(paste(alpha-scale)),
                               expression(paste(beta-scale)),
                               expression(paste(gamma-scale)))) +
  labs(subtitle = expression(paste(beta, '-scale')),
       x = 'Change in diversity',
       y = '') +
  theme_minimal()


gamma_metrics <- ggplot() + 
  geom_hline(yintercept = 0, lty = 2, colour = '#969696') +
  geom_vline(xintercept = 0, lty = 2, colour = '#969696') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#969696') +
  geom_vline(xintercept = 0, lty = 2) + 
  mapply(function(level) {
    stat_ellipse(data = two_scales,
                 aes(x = regional_S_PIE + regional_S_PIE_global,
                     y = regional_S + regional_S_global, 
                     fill = 'gamma-scale'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .25,
                 level = level)
  }, 
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.45)) +
  geom_point(data = slopes_regional_variation %>% 
               unnest(cols = c(regional_S, regional_S_PIE)) %>% 
               bind_cols(slopes_global_level %>% 
                           slice(rep(1:n(), times = nrow(regional_levels)))) %>% 
               group_by(regional_level) %>% 
               summarise(S_PIE = median(regional_S_PIE + regional_S_PIE_global),
                         S = median(regional_S + regional_S_global)),
             aes(x = S_PIE, 
                 y = S, 
                 colour = 'gamma-scale'#, shape = 'alpha-scale'
             )) +
  scale_colour_manual(guide = 'none',
                      name = 'Scale',
                      values = c('alpha-scale' = '#001c6f',
                                 'beta-scale' = '#ffa600',
                                 'gamma-scale' = '#cf1762'),
                      labels = c(expression(paste(alpha-scale)),
                                 expression(paste(beta-scale)),
                                 expression(paste(gamma-scale)))) + 
  scale_fill_manual(guide = 'none',
                    name = 'Scale',
                    values = c('alpha-scale' = '#001c6f',
                               'beta-scale' = '#ffa600',
                               'gamma-scale' = '#cf1762'),
                    labels = c(expression(paste(alpha-scale)),
                               expression(paste(beta-scale)),
                               expression(paste(gamma-scale)))) +
  scale_x_continuous(breaks = c(-0.06, -0.04, -0.02, 0, 0.02, 0.04, 0.06),
                     labels = c(-0.06, '', -0.02, 0, 0.02, '', 0.06)) +
  labs(subtitle = expression(paste(gamma, '-scale')),
       x = '',
       y = '') +
  theme_minimal()

# combine
cowplot::plot_grid(alpha_metrics + 
                     labs(x = 'Change in effective number\nof species (common species)') +
                     theme(axis.title = element_text(size = 8)),
                   gamma_metrics + 
                     labs(x = 'Change in effective number\nof species (common species)',
                          y = 'Change in richness') +
                     theme(axis.title = element_text(size = 8)),
                   beta_metrics + 
                     labs(x = 'Change in effective number\nof communities (common species)',
                          y = 'Change in effective number of communities (all species)') +
                     theme(axis.title = element_text(size = 8)),
                   nrow = 1,
                   labels = 'AUTO', align = 'hv')
# save
ggsave('~/Dropbox/1current/spatial_composition_change/figures/results/FigS7.pdf',
       width = 184, height = 88, units = 'mm')
