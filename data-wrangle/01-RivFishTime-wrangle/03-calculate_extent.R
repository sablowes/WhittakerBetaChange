# calculate extent for locations used in RivFishTime regions (1 extent per region)


library(sf)
library(tidyverse)
load('~/Dropbox/1current/spatial_composition_change/results/rft_LRR.Rdata')
meta <- read_csv('~/Dropbox/1current/data/RivFishTime/1873_10_1873_2_RivFishTIME_TimeseriesTable.csv')

locations <- meta %>%
  mutate(SourceID = as.character(SourceID)) %>% 
  filter(SourceID %in% rft_regional_LR$SourceID) %>% 
  filter(TimeSeriesID %in% rft_local_LR$TimeSeriesID) 


rft_sf <- locations %>% 
  # create geom column
  st_as_sf(coords = c('Longitude', 'Latitude'))  %>% 
  # set geographic crs
  st_set_crs(4326) %>% 
  # combine geometry within SourceID (==regions)
  group_by(SourceID) %>% 
  summarise(geometry = st_combine(geometry)) %>% 
  ungroup()

# calculate the convex hulls for these studies and apply the
rft_extent_hull <- rft_sf %>% 
  st_convex_hull() 

# centroid of hulls for map
rft_extent_centroid <- rft_sf %>% 
  st_centroid() 

rft_extent_centroid_coords <- rft_extent_centroid %>% 
  st_coordinates()

# and calculate the area of the hulls
rft_extent_temp <- rft_extent_hull %>% 
  group_by(SourceID) %>% 
  st_area() 

#  join area with SourceID and Year
rft_extent <- bind_cols(
  tibble(rft_extent_hull, 
         extent_area = rft_extent_temp,
         extent_km2 = as.numeric(extent_area) / 1e6,
         centroid_X = as.numeric(rft_extent_centroid_coords[,'X']),
         centroid_Y = as.numeric(rft_extent_centroid_coords[,'Y'])))

save(rft_extent, 
     file = '~/Dropbox/1current/spatial_composition_change/results/rft_extent.Rdata')

ggplot() +
  facet_wrap(~SourceID, scales = 'free') +
  geom_point(data = locations %>% 
               filter(n_distinct(HydroBasin) > 1), 
             aes(x = Longitude, y = Latitude,
                 colour = as.factor(HydroBasin))) +
  theme(legend.position = 'none')

ggsave('~/Dropbox/1current/spatial_composition_change/figures/rft_locations.png',
       width = 290, height = 200, units = 'mm')
