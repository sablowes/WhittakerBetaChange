# Whitakker concept plot for models fit to time series data
# need to run Fig3-1-wrangle.R before plotting with code below

whitS <-
ggplot() + 
  # facet_wrap(~taxon_mod) +
  # posterior density
  mapply(function(level) {
    stat_ellipse(data = two_scales,
                 aes(x = local_S + local_S_global,
                     y = regional_S + regional_S_global),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .25,
                 level = level)
  }, 
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.45)) +
  geom_point(data = two_scales %>% 
               group_by(regional_level) %>% 
               summarise(local_dS = median(local_S + local_S_global),
                         regional_dS = median(regional_S + regional_S_global)) %>% 
               left_join(ts_meta),
             aes(x = local_dS,
                 y = regional_dS)) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  scale_x_continuous(breaks = seq(-0.06, 0.08, by = 0.02)) +
  scale_y_continuous(breaks = seq(-0.06, 0.08, by = 0.02)) +
  scale_fill_manual(guide = 'none') +
  labs(x = expression(paste(alpha-scale, ' richness change [log(S) / year]')),
       y = expression(paste(gamma-scale, ' richness change [log(S) / year]'))) +
  coord_fixed()+
  theme_minimal()

whitSPIE <-
ggplot() + 
  # posterior density including variation among regions (datasets)
  mapply(function(level) {
    stat_ellipse(data = two_scales,
                 aes(x = local_S_PIE + local_S_PIE_global,
                     y = regional_S_PIE + regional_S_PIE_global),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .25,
                 level = level)
  }, 
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.45)) +
  geom_point(data = two_scales %>% 
               group_by(regional_level) %>% 
               summarise(local_dSPIE = median(local_S_PIE + local_S_PIE_global),
                         regional_dSPIE = median(regional_S_PIE + regional_S_PIE_global)) %>% 
               left_join(ts_meta),
             aes(x = local_dSPIE,
                 y = regional_dSPIE)) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  # scale_x_continuous(
  #                    breaks = seq(-0.06, 0.06, by = 0.02)) +
  # scale_y_continuous(breaks = seq(-0.06, 0.06, by = 0.02)) +
  scale_fill_manual(guide = 'none') +
  labs(x = expression(paste(alpha-scale, ' diversity change [log(D) / year]')),
       y = expression(paste(gamma-scale, ' diversity change [log(D) / year]'))) +
  coord_fixed()+
  theme_minimal()

cowplot::plot_grid(whitS,
                   whitSPIE,
                   # align= 'v',
                   nrow = 1)

ggsave('~/Dropbox/1current/spatial_composition_change/figures/results/FigS6.pdf',
       width = 290, height = 200, units = 'mm')

