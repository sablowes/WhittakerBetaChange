# calculate extent for locations used in BioTIME (1 extent per study==region)


library(sf)
library(tidyverse)

load('~/Dropbox/1current/spatial_composition_change/results/bt_LRR.Rdata')

bt_meta <- read_csv('~/Dropbox/BioTIMELatest/bioTIMEMetadataJune2021.csv')

bt_sf <- bt_local_LR %>% 
  select(STUDY_ID, loc_plot) %>% 
  tidyr::separate(loc_plot, c('Longitude', 'Latitude', 'Plot'), remove = FALSE, sep = '_') %>% 
  st_as_sf(coords = c('Longitude', 'Latitude'))  %>% 
  # set geographic crs
  st_set_crs(4326) %>% 
  # combine geometry within SourceID (==regions)
  group_by(STUDY_ID) %>% 
  summarise(geometry = st_combine(geometry)) %>% 
  ungroup()

# calculate the convex hulls for these studies and apply the
bt_extent_hull <- bt_sf %>% 
  st_wrap_dateline(options = c("WRAPDATELINE=YES")) %>% 
  st_convex_hull() 
  

# and calculate the area of the hulls
bt_extent_temp <- bt_extent_hull %>% 
  group_by(STUDY_ID) %>% 
  st_area() 

# centroid of hulls for map
bt_extent_centroid <- bt_sf %>% 
  st_centroid() 

bt_extent_centroid_coords <- bt_extent_centroid %>% 
  st_coordinates()


#  join area with SourceID and Year
bt_extent <- bind_cols(
  tibble(bt_extent_hull, 
         extent_area = bt_extent_temp,
         extent_km2 = as.numeric(extent_area) / 1e6,
         centroid_X = as.numeric(bt_extent_centroid_coords[,'X']),
         centroid_Y = as.numeric(bt_extent_centroid_coords[,'Y'])))

# one regional returns zero extent due to locations on a single longitude (i.e., a line)
# use distance instead.
study_152_extent = bt_extent_hull %>% 
  filter(STUDY_ID==152) %>% 
  st_length()

bt_extent <- bt_extent %>% 
  mutate(extent_area = ifelse(STUDY_ID==152, study_152_extent, extent_area),
         extent_km2 = ifelse(STUDY_ID==152, as.numeric(study_152_extent)/1e6, extent_km2))

# date line wrap did not work for 163
s163_fix <- bt_local_LR %>% 
  filter(STUDY_ID==163) %>% 
  separate(loc_plot, c('Longitude', 'Latitude', 'plot'), sep = '_', convert = TRUE) %>% 
  mutate(Longitude = ifelse((max(Longitude) - min(Longitude)) >180 & Longitude > 0, Longitude - 360, Longitude)) %>%
  st_as_sf(coords = c('Longitude', 'Latitude'))  %>% 
  # set geographic crs
  st_set_crs(4326) %>% 
  # combine geometry within SourceID (==regions)
  group_by(STUDY_ID) %>% 
  summarise(geometry = st_combine(geometry)) %>% 
  ungroup()

# calculate the convex hulls for these studies and apply the
bt_extent_hull_s163 <- s163_fix %>% 
  st_wrap_dateline(options = c("WRAPDATELINE=YES")) %>% 
  st_convex_hull() 

# and calculate the area of the hulls
bt_extent_temp_s163 <- bt_extent_hull_s163 %>% 
  group_by(STUDY_ID) %>% 
  st_area() 

# centroid of hulls for map
bt_extent_centroid_s163 <- s163_fix %>% 
  st_centroid() 

bt_extent_centroid_coords_163 <- bt_extent_centroid_s163 %>% 
  st_coordinates()

bt_extent <- bt_extent %>% 
  mutate(extent_area = ifelse(STUDY_ID==163, bt_extent_temp_s163, extent_area),
         extent_km2 = ifelse(STUDY_ID==163, as.numeric(bt_extent_temp_s163)/1e6, extent_km2),
         centroid_X = ifelse(STUDY_ID==163, as.numeric(bt_extent_centroid_coords_163[,'X']), centroid_X),
         centroid_Y = ifelse(STUDY_ID==163, as.numeric(bt_extent_centroid_coords_163[,'Y']), centroid_Y))

# put some other metadata in...
bt_extent <- left_join(bt_extent,
                       bt_meta %>% 
                         select(STUDY_ID, TAXA, REALM, GRAIN_SQ_KM))

save(bt_extent, 
     file = '~/Dropbox/1current/spatial_composition_change/results/bt_extent.Rdata')

# tidy up categories
bt_extent <- bt_extent %>% 
  mutate(TAXA = case_when(TAXA=='All' ~ 'Multiple taxa', 
                          TRUE ~ as.character(TAXA)))
bt_extent <- bt_extent %>% 
  st_as_sf()


pdf('~/Dropbox/1current/spatial_composition_change/figures/data-visualisation/bt-extents.pdf', width = 12, height = 9)

for(i in 1:length(unique(bt_extent$STUDY_ID))){
  print(paste('STUDY', i, 'of', length(unique(bt_extent$STUDY_ID)), 'studies'))
    
    dat = bt_extent %>%
    filter(STUDY_ID==unique(bt_extent$STUDY_ID)[i])
    
    if(dat$STUDY_ID==163){dat$geometry = bt_extent_hull_s163$geometry}
  
    p =ggplot() +
        # facet_wrap(~YEAR) +
        geom_sf(data = dat %>%
                  st_wrap_dateline(options = c("WRAPDATELINE=YES")),
                                   colour = 'dark red',
                alpha = 0.25) +
        geom_sf(data = bt_sf %>%
                  filter(STUDY_ID==unique(bt_extent$STUDY_ID)[i]) %>% 
                  st_cast(to = 'MULTIPOINT'),
                pch = 1) +
        labs(title = paste0(dat$STUDY_ID, ', ',
                           dat$REALM, ', ', 
                           dat$TAXA
                           ))
      print(p)
}
dev.off()


