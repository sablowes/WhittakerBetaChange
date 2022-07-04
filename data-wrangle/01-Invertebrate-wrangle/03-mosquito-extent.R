# calculate extent as bounding box for mosiquto data

library(sf)
library(tidyverse)

load('~/Dropbox/1current/spatial_composition_change/results/mosquito_LRR.Rdata')

mosq_sf <- mosquito_local_LR %>% 
  select(region, plot) %>% 
  tidyr::separate(plot, c('Longitude', 'Latitude'), remove = FALSE, sep = '_') %>% 
  st_as_sf(coords = c('Longitude', 'Latitude'))  %>% 
  # set geographic crs
  st_set_crs(4326) %>% 
  # combine geometry within SourceID (==regions)
  group_by(region) %>% 
  summarise(geometry = st_combine(geometry)) %>% 
  ungroup()

# calculate the convex hulls for these studies and apply the
mosq_extent_hull <- mosq_sf %>% 
  st_convex_hull() 


# and calculate the area of the hulls
mosq_extent_temp <- mosq_extent_hull %>% 
  group_by(region) %>% 
  st_area() 

# centroid of hulls for map
mosq_extent_centroid <- mosq_sf %>% 
  st_centroid() 

mosq_extent_centroid_coords <- mosq_extent_centroid %>% 
  st_coordinates()


#  join area with SourceID and Year
mosq_extent <- bind_cols(
  tibble(mosq_extent_hull, 
         extent_area = mosq_extent_temp,
         extent_km2 = as.numeric(extent_area) / 1e6,
         grain_m2 = 0.0625,
         centroid_X = as.numeric(mosq_extent_centroid_coords[,'X']),
         centroid_Y = as.numeric(mosq_extent_centroid_coords[,'Y'])))


save(mosq_extent, 
     file = '~/Dropbox/1current/spatial_composition_change/results/mosq_extent.Rdata')

mosq_extent <- mosq_extent %>% 
  st_as_sf()

pdf('~/Desktop/mosq_extents', width = 12, height = 9)
for(i in 1:length(unique(mosq_extent$region))){
  print(paste('region', i, 'of', length(unique(mosq_extent$region)), 'region'))
  
  p = ggplot() +
    facet_wrap(~region) +
    geom_sf(data = mosq_extent %>%
              st_as_sf() %>% 
              filter(region==unique(mosq_extent$region)[i]),
            colour = 'dark red',
            fill = NA,
            alpha = 0.5) +
    geom_sf(data = mosq_sf %>% 
              filter(region==unique(mosq_extent$region)[i]), 
            fill = NA,
            pch = 1) +
    labs(title = paste(unique(mosq_extent$region)[i]#, ', taxa = ',
                       # mosq_4space_10YR_combine$TAXA[mosq_4space_10YR_combine$STUDY_ID==unique(mosq_4space_10YR_combine$STUDY_ID)[i]],
                       # 'Realm = ', mosq_4space_10YR_combine$REALM[mosq_4space_10YR_combine$STUDY_ID==unique(mosq_4space_10YR_combine$STUDY_ID)[i]]
    )
    ) 
  print(p)
}
dev.off()
