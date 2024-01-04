
# simple map
world <- map_data('world')

# taxon group colour scheme
taxon_colour = c('Plants' = '#a6cee3',
                 'Mammals' = '#e31a1c',
                 'Birds' = '#b2df8a',
                 'Invertebrates' = '#fb9a99',
                 'Fish' = '#1f78b4',
                 'Herptiles' = '#cab2d6')

load('~/Dropbox/1current/spatial_composition_change/results/allLRR_meta-new.Rdata')
sample_type_count <- as_tibble(all_meta) %>% 
  group_by(sample_type) %>% 
  summarise(n = n())

database_count <- all_meta %>% 
  group_by(database) %>% 
  summarise(n = n())

# maybe want a plot with points scaled by number of locations in region
all_meta <- left_join(all_meta,
                      local_LRR %>% 
                        distinct(database, regional_level, nLocations))

ggplot() +
  geom_polygon(data=world, 
               aes(long, lat, group = group), colour=NA, fill='#f0f0f0', size=0) +
  geom_point(data = all_meta,
             # filter(sample_type=='resurvey'),
             # filter(database %in% c('BioTIME', 'Invertebrates', 'RivFishTime', 'Sonly', 'Homogenisation')),
             aes(x = longitude, y = latitude, colour = taxon_mod,  shape = sample_type),#,shape = taxa),, size = nLocations
             # alpha = 0.6,
             #size = 1.5
  ) +
  coord_map('mollweide', ylim = c(-60, 90), xlim = c(-180, 180)) +
  scale_x_continuous(name = 'Longitude', breaks = seq(-180, 180, by = 30)) +
  scale_y_continuous(name = 'Latitude', breaks = c(0, -23.5, 23.5, -60, 60)) +
  scale_size_area(name = 'Number of locations',
                  trans = 'log10') +# name = 'Duration (years)', breaks = c(10,40,160)
  scale_colour_manual(name = 'Taxon group', values = taxon_colour) +
  scale_shape_manual(#guide = 'none',
    name = 'Sample type', values = c('checklist' = 17,
                                     'resurvey' = 19),
    labels = c(paste0('Checklist (n = ', 
                      sample_type_count %>% 
                        filter(sample_type=='checklist') %>% pull(n), ')'),
               paste0('Resurvey (n = ', 
                      sample_type_count %>% 
                        filter(sample_type=='resurvey') %>% pull(n), ')'))) +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = 'black', size = 0.1), 
        panel.border = element_blank(),
        axis.ticks = element_blank(), 
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = 'top',
        # plot.tag = element_text(size = 8, face = "bold"),
        plot.margin = unit(c(0,0,0,0), units = 'mm'),
        legend.margin = margin(),
        legend.box.spacing = unit(c(0,0,0,0), units = 'mm'),
        # weird that the face argument is not working for plot.tag, 
        # it inherits from title, so...
        # title = element_text(face = 'bold', size = 8),
        legend.text = element_text(size = 10, face = 'plain'),
        legend.title = element_text(size = 11, face = 'bold')) +
  guides(colour = guide_legend(title.position = 'top', title.hjust = 0.5),
         shape = guide_legend(title.position = 'top', title.hjust = 0.5, size = 3),
         size = guide_legend(title.position = 'top', title.hjust = 0.5))


# two column fig Sci Adv
ggsave('~/Dropbox/1current/spatial_composition_change/figures/results/FigS2.pdf',
       width = 184, height = 150, units = 'mm')
