library(cowplot)
# plot beta-diversity from time series models fit at local and regional scales

dist2line = function(x, y, a = 1, b = -1){
  # calculate distance from line: ax + by + c 
  # to vector of points(x, y)
  # NB: here line is 1:1 so c = 0, a = 1, b = -1
  
  numerator = -(a*x + b*y) # negative distance means point below line 
  # (homogenisation change beta-diversity < 0)
  denom = sqrt(a^2 + b^2)
  return(numerator / denom)
}

# use full posterior of the overall slope estimates
overall_d <-slopes_global_level %>% 
  mutate(dS = dist2line(local_S_global, regional_S_global),
         # d_eH = dist2line(local_eH_global, regional_eH_global),
         d_S_PIE = dist2line(local_S_PIE_global, regional_S_PIE_global))

order_q_beta <-
  ggplot() +
    stat_slabinterval(data = overall_d,
               aes(dS, fill = 'q = 0', shape = 'q = 0', colour = 'q = 0'),
               point_interval = median_qi,
               .width = c(0.50, 0.9), 
               alpha = 0.35, point_alpha = 1, interval_alpha = 1,
               position = position_nudge(y = 0.1)) +
    # stat_slabinterval(data = overall_d,
    #              aes(d_eH, fill = 'q = 1', shape = 'q = 1', colour = 'q = 1'),
    #              point_interval = median_qi,
    #              .width = c(0.50, 0.9),
    #              alpha = 0.35, point_alpha = 1, interval_alpha = 1) +
    stat_slabinterval(data = overall_d,
                 aes(d_S_PIE, fill = 'q = 2', shape = 'q = 2', colour = 'q = 2'),
                 point_interval = median_qi,
                 .width = c(0.50, 0.9),
                 alpha = 0.35, point_alpha = 1, interval_alpha = 1,
                 position = position_nudge(y = -0.1)) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  scale_fill_manual(name = 'Diversity order',
                    values = c('q = 0' = '#424242',
                               'q = 1' = '#8e8e8e',
                               'q = 2' = '#e2e2e2')) +
  scale_colour_manual(name = 'Diversity order',
                    values = c('q = 0' = '#424242',
                               'q = 1' = '#8e8e8e',
                               'q = 2' = '#e2e2e2')) +
  scale_shape_manual(name = 'Diversity order',
                     values = c('q = 0' = 15,
                                'q = 1' = 19,
                                'q = 2' = 17)) +
  labs(x = expression(paste(Delta, beta, ' - diversity . ', year^-1)),
       y = '') +
  theme_minimal() +
  theme(legend.position = 'none', #c(0.1,0.9),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = 'white', color = 'white'),
        panel.grid = element_blank(),
        axis.line.x = element_line(colour = 'black'),
        axis.ticks.x = element_line(colour = 'black'),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        plot.margin = unit(c(2,4,2,2), units = 'mm')) 

slope_summary_two_scales <- slopes_global_level %>% 
  summarise(local_S = median(local_S_global),
            # local_eH = median(local_eH_global),
            local_S_PIE = median(local_S_PIE_global),
            local_S_Q05 = quantile(local_S_global, probs = 0.05),
            local_S_Q95 = quantile(local_S_global, probs = 0.95),
            # local_eH_Q05 = quantile(local_eH_global, probs = 0.05),
            # local_eH_Q95 = quantile(local_eH_global, probs = 0.95),
            local_S_PIE_Q05 = quantile(local_S_PIE_global, probs = 0.05),
            local_S_PIE_Q95 = quantile(local_S_PIE_global, probs = 0.95),
            # regional
            regional_S = median(regional_S_global),
            # regional_eH = median(regional_eH_global),
            regional_S_PIE = median(regional_S_PIE_global),
            regional_S_Q05 = quantile(regional_S_global, probs = 0.05),
            regional_S_Q95 = quantile(regional_S_global, probs = 0.95),
            # regional_eH_Q05 = quantile(regional_eH_global, probs = 0.05),
            # regional_eH_Q95 = quantile(regional_eH_global, probs = 0.95),
            regional_S_PIE_Q05 = quantile(regional_S_PIE_global, probs = 0.05),
            regional_S_PIE_Q95 = quantile(regional_S_PIE_global, probs = 0.95))
            
order_q_two_scales <-
ggplot() + 
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  geom_linerange(data = slope_summary_two_scales,
                 aes(x = local_S,
                     ymin = regional_S_Q05, ymax = regional_S_Q95,
                     colour = 'q = 0'),
                 # alpha = 0.35
                 ) +
  geom_linerange(data = slope_summary_two_scales,
                 aes(y = regional_S,
                     xmin = local_S_Q05, xmax = local_S_Q95,
                     colour = 'q = 0'),
                 # alpha = 0.35
                 ) +
  geom_point(data = slope_summary_two_scales,
             aes(x = local_S,
                 y = regional_S, colour = 'q = 0', shape = 'q = 0'),
             size = 3) +
  # q = 1
  # geom_linerange(data = slope_summary_two_scales,
  #                aes(x = local_eH,
  #                    ymin = regional_eH_Q05, ymax = regional_eH_Q95,
  #                    colour = 'q = 1')) +
  # geom_linerange(data = slope_summary_two_scales,
  #                aes(y = regional_eH,
  #                    xmin = local_eH_Q05, xmax = local_eH_Q95,
  #                    colour = 'q = 1')) +
  # geom_point(data = slope_summary_two_scales,
  #            aes(x = local_eH,
  #                y = regional_eH, colour = 'q = 1', shape = 'q = 1'),
  #            size = 3) +
  # q = 2
  geom_linerange(data = slope_summary_two_scales,
                 aes(x = local_S_PIE,
                     ymin = regional_S_PIE_Q05, ymax = regional_S_PIE_Q95,
                     colour = 'q = 2')) +
  geom_linerange(data = slope_summary_two_scales,
                 aes(y = regional_S_PIE,
                     xmin = local_S_PIE_Q05, xmax = local_S_PIE_Q95,
                     colour = 'q = 2')) +
  geom_point(data = slope_summary_two_scales,
             aes(x = local_S_PIE,
                 y = regional_S_PIE, colour = 'q = 2', shape = 'q = 2'),
             size = 3) +
  scale_colour_manual(guide = 'none',
                      name = 'Diversity order',
                      values = c('q = 0' = '#8e8e8e',
                                 # 'q = 1' = '#8e8e8e',
                                 'q = 2' = '#e2e2e2')) +
  scale_shape_manual(guide = 'none',
                     name = 'Diversity order',
                     values = c('q = 0' = 15,
                                # 'q = 1' = 19,
                                'q = 2' = 17)) +
  labs(y = expression(paste(gamma-scale, ' [log(', phantom()^q*D, ') / year]')),
       x = expression(paste(alpha-scale, ' [log(', phantom()^q*D, ') / year]'))) + 
  coord_fixed() +
  theme_minimal() +
  theme(plot.background = element_rect(fill = 'white', color = 'white', linetype = 0),
        panel.grid = element_blank(),
        axis.text = element_text(size = 8)) 

# horizontal legend for top of figure
hleg <- ggplot() + 
  geom_point(data = slope_summary_two_scales,
             aes(x = local_S,
                 y = regional_S, colour = 'q = 0', shape = 'q = 0')) +
  # q = 1
  # geom_point(data = slope_summary_two_scales,
  #            aes(x = local_eH,
  #                y = regional_eH, colour = 'q = 1', shape = 'q = 0')) +
  # q = 2
  geom_point(data = slope_summary_two_scales,
             aes(x = local_S_PIE,
                 y = regional_S_PIE, colour = 'q = 2', shape = 'q = 0'),
             size = 3) +
  scale_colour_manual(name = 'Diversity order',
                      values = c('q = 0' = '#8e8e8e',
                                 # 'q = 1' = '#8e8e8e',
                                 'q = 2' = '#e2e2e2')) +
  scale_shape_manual(name = 'Diversity order',
                      values = c('q = 0' = 15,
                                 # 'q = 1' = 19,
                                 'q = 2' = 17)) +
  theme(legend.direction = 'horizontal',
        legend.position = 'top', 
        legend.key = element_blank()) +
  guides(colour = guide_legend(alpha = 0.35))
source('~/Dropbox/1current/R_random/functions/gg_legend.R')
legend <- gg_legend(hleg)


plot_grid(legend,
          plot_grid(order_q_two_scales, order_q_beta,
                    nrow = 1),
          nrow = 2,
          rel_heights = c(0.05, 1))

# use regional-level posteriors to examine potential covariates
regional_d <-slopes_regional_variation %>% 
  unnest(cols = c(local_S, local_S_PIE, #local_eH
                  regional_S, regional_S_PIE)) %>%
  # need to add global slope estimate to departures
  bind_cols(slopes_global_level %>% 
              slice(rep(1:n(), times = 165))) %>% #regional_eH, 
  mutate(dS = dist2line((local_S + local_S_global), 
                        (regional_S + regional_S_global)),
         # d_eH = dist2line(local_eH, regional_eH),
         d_S_PIE = dist2line((local_S_PIE + local_S_PIE_global), 
                             (regional_S_PIE + regional_S_PIE_global))) 



forest_q0_3scales <- ggplot() +
  # facet_wrap(~realm) + 
  geom_vline(xintercept = 0, lty = 2) + 
  stat_pointinterval(data = slopes_regional_variation %>% 
                       unnest(cols = c(local_S, regional_S)) %>% 
                       # need to add global slope estimate
                       bind_cols(slopes_global_level %>% 
                                   slice(rep(1:n(), times = 165))),
                     aes(x = local_S + local_S_global, 
                         y = fct_reorder(regional_level, local_S + local_S_global), 
                         colour = 'alpha-scale'#, shape = 'alpha-scale'
                     ),
                     size = 0.5,
                     alpha = 0.5,
                     .width = c(0.90)) +
  stat_pointinterval(data = slopes_regional_variation %>% 
                       unnest(cols = c(local_S, regional_S)) %>% 
                       # need to add global slope estimate
                       bind_cols(slopes_global_level %>% 
                                   slice(rep(1:n(), times = 165))),
                     aes(x = regional_S + regional_S_global, 
                         y = fct_reorder(regional_level, local_S + local_S_global), 
                         colour = 'gamma-scale'#, shape = 'gamma-scale'
                     ),
                     size = 0.5,
                     alpha = 0.5,
                     .width = c(0.90),
                     # position = position_nudge(y = -0.33)
  ) +
  stat_pointinterval(data = regional_d,
                     aes(x = dS, 
                         y = fct_reorder(regional_level, local_S + local_S_global), 
                         colour = 'beta-scale'#, shape = 'beta-scale'
                     ),
                     size = 0.5,
                     alpha = 0.5,
                     .width = c(0.90),
                     # position = position_nudge(y = -0.66)
  ) +
  scale_colour_manual(guide = 'none',
                      name = 'Scale',
                      values = c('alpha-scale' = '#001c6f',
                                 'beta-scale' = '#ffa600',
                                 'gamma-scale' = '#cf1762'),
                      labels = c(expression(paste(alpha-scale)),
                                 expression(paste(beta-scale)),
                                 expression(paste(gamma-scale)))) +
  # scale_shape_manual(guide = 'none',
  #                    name = 'Scale',
  #                    values = c('alpha-scale' = 15,
  #                               'beta-scale' = 19,
  #                               'gamma-scale' = 17),
  #                    labels = c(expression(paste(alpha-scale)),
  #                               expression(paste(beta-scale)),
  #                               expression(paste(gamma-scale)))) +
  labs(x = expression(paste('Change in diversity', ' [log(', phantom()^0*D, ') / year]')),
       y = 'Data set') +
  theme_minimal() + 
  theme(axis.text.y = element_blank(),
        legend.position = c(0.9,0.5),
        legend.justification = c(0.9,0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())


forest_q2_3scales <- ggplot() +
  # facet_wrap(~realm) +
  geom_vline(xintercept = 0, lty = 2) + 
  stat_pointinterval(data = slopes_regional_variation %>% 
                       unnest(cols = c(local_S, local_S_PIE)) %>% 
                       bind_cols(slopes_global_level %>% 
                                   slice(rep(1:n(), times = 165))),
                     aes(x = local_S_PIE + local_S_PIE_global, 
                         y = fct_reorder(regional_level, local_S_PIE + local_S_PIE_global), 
                         colour = 'alpha-scale'#, shape = 'alpha-scale'
                     ),
                     size = 0.5,
                     alpha = 0.5,
                     .width = c(0.90)) +
  stat_pointinterval(data = slopes_regional_variation %>% 
                       unnest(cols = c(local_S, local_S_PIE, regional_S_PIE)) %>% 
                       bind_cols(slopes_global_level %>% 
                                   slice(rep(1:n(), times = 165))),
                     aes(x = regional_S_PIE + regional_S_PIE_global, 
                         y = fct_reorder(regional_level, local_S_PIE + local_S_PIE_global), 
                         colour = 'gamma-scale'#, shape = 'gamma-scale'
                     ),
                     size = 0.5,
                     alpha = 0.5,
                     .width = c(0.90),
                     position = position_nudge(y = -0.33)) +
  stat_pointinterval(data = regional_d,
                     aes(x = d_S_PIE, 
                         y = fct_reorder(regional_level, local_S_PIE + local_S_PIE_global), 
                         colour = 'beta-scale'#, shape = 'beta-scale'
                     ),
                     size = 0.5,
                     alpha = 0.5,
                     .width = c(0.90),
                     position = position_nudge(y = -0.66)) +
  scale_colour_manual(guide = 'none',
                      name = 'Scale',
                      values = c('alpha-scale' = '#001c6f',
                                 'beta-scale' = '#ffa600',
                                 'gamma-scale' = '#cf1762'),
                      labels = c(expression(paste(alpha-scale)),
                                 expression(paste(beta-scale)),
                                 expression(paste(gamma-scale)))) +
  # scale_shape_manual(guide = 'none',
  #   name = 'Scale',
  #   values = c('alpha-scale' = 15,
  #              'beta-scale' = 19,
  #              'gamma-scale' = 17),
  #   labels = c(expression(paste(alpha-scale)),
  #              expression(paste(beta-scale)),
  #              expression(paste(gamma-scale)))) +
  labs(x = expression(paste('Change in diversity', ' [log(', phantom()^2*D, ') / year]')),
       y = 'Region') +
  theme_minimal() + 
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(1,0),
        legend.justification = c(1,0),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())

three_scale_leg <- ggplot() +
  geom_point(data = regional_d,
             aes(x = dS, y = regional_level,
                 colour = 'beta-scale'#, shape = 'beta-scale'
             )) + 
  geom_point(data = slopes_regional_variation %>%
               unnest(local_S),
             aes(x = local_S, y = regional_level,
                 colour = 'alpha-scale'#, shape = 'alpha-scale'
             )) +
  geom_point(data = slopes_regional_variation %>% 
               unnest(regional_S),
             aes(x = regional_S, y = regional_level, 
                 colour = 'gamma-scale'#, shape = 'gamma-scale'
             )) +
  scale_colour_manual(#guide = 'none',
    name = 'Scale',
    values = c('alpha-scale' = '#001c6f',
               'beta-scale' = '#ffa600',
               'gamma-scale' = '#cf1762'),
    labels = c(expression(paste(alpha-scale)),
               expression(paste(beta-scale)),
               expression(paste(gamma-scale)))) +
  theme(legend.position = 'top',
        legend.key = element_blank()) +
  guides(colour = guide_legend(title.position = 'left', nrow = 1, 
                               label.hjust = 0, title.hjust = 0))

scale_leg <- gg_legend(three_scale_leg)


plot_grid(scale_leg,
          plot_grid(forest_q0_3scales, 
                    # forest_q1_3scales,
                    forest_q2_3scales,
                    nrow = 1),
          nrow = 2, rel_heights = c(0.05, 1))



left <- plot_grid(scale_leg,
          plot_grid(forest_q0_3scales,
                    forest_q2_3scales,
                    nrow = 1,
                    labels = c('a', 'b')),
         ncol = 1, rel_heights = c(0.05, 1))

right <- plot_grid(NULL,
                   legend,
                   plot_grid(order_q_two_scales, order_q_beta,
                             nrow = 2,
                             labels = c('c', 'd')),
                   NULL,
                   nrow = 4,
                   rel_heights = c(0.2,0.1, 1, 0.2))




plot_grid(left, right,
          rel_widths = c(1.75, 1))

ggsave('~/Dropbox/1current/spatial_composition_change/figures/results/Fig3.pdf',
       width = 250, height = 210, units = 'mm')