source('~/Dropbox/1current/spatial_composition_change/WhittakerBetaChange/analysis/Fig2i-wrangle.R')

# counts of observed homogenisation and differentiation (no change is where empirical delta-alpha==delta-regional==0)
pattern_summary %>% 
  # count of homogenisation, differentiation, boundary cases
  group_by(spatial_pattern_obs) %>% 
  summarise(n = n()) %>% 
  ungroup() 

dat4bar_obs <- pattern_summary %>%
  group_by(concept_obs) %>% #
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(group = 'data') %>% 
  rename(concept = concept_obs) %>% 
  # remove NAs, fixed in next step
  filter(!is.na(concept))

# need to fix regions on boundary of categories
# here we add use fractions to determine overall count for each category
pattern_summary %>% 
  filter(is.na(concept_obs)) %>% 
  select(regional_level, spatial_pattern_obs, local_mu.i, regional_mu.i) %>% 
  data.frame

dat4bar_obs <- dat4bar_obs %>%
  mutate(n = case_when(# there are 13 cases that fall on the boundary of iv-v
    concept=='Low occupancy replace high' ~ n + 6.5,
    concept=='Lose high occupancy' ~ n + 6.5,
    # 1 case between v-vi
    concept=='Low occupancy replace high' ~ n + 0.5,
    concept=='Gain low occupancy' ~ n + 0.5,
    # 17 cases between i and ii
    concept=='Gain high occupancy' ~ n + 8.5,
    concept=='High occupancy replace low' ~ n + 8.5,
    TRUE ~ as.numeric(n)),
    # five cases where deltaSalpha=deltaSgamma=0
    n = n + 5/6)


dat4bar_obs$concept <- factor(dat4bar_obs$concept,
                               levels = c('Gain high occupancy',
                                          'High occupancy replace low',
                                          'Lose low occupancy',
                                          'Lose high occupancy',
                                          'Low occupancy replace high',
                                          'Gain low occupancy'), ordered = T,
                               labels = c('Gain high occupancy',
                                          'High occupancy replace low',
                                          'Lose low occupancy',
                                          'Lose high occupancy',
                                          'Low occupancy replace high',
                                          'Gain low occupancy'))

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


# fig3_concept_obs <-
  ggplot() +
  geom_bar(data = dat4bar_obs,
           aes(x = concept_label, y = n, fill = concept),#, fill = sample_type
           stat = 'identity') +
  geom_label(aes(x = 2, y = 70, label = 'Differentiation'),
             size = 3.5) +
  geom_label(aes(x = 5, y = 70, label = 'Homogenisation'),
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


# local
ggsave('~/Dropbox/1current/spatial_composition_change/figures/results/FigS7.pdf',
       width = 200,  height = 138, units = 'mm')
