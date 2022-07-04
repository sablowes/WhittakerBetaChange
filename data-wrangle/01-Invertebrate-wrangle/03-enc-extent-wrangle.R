# metadata for ENC data

path2wd <- '~/Dropbox/1current/spatial_composition_change/code/invertebrate_data/code_from_Roel/metacommunity files for Shane/'

sites <- read_csv(paste0(path2wd, 'ECN 3 datasets for metacommunities/sites.csv')) %>% 
  rename(SITECODE = `Site code`)


sites <- sites %>% 
  mutate(lat = parzer::parse_lat(latitude),
         long = parzer::parse_lon(longitude))

# check which sites are used for each data set
load('~/Dropbox/1current/spatial_composition_change/results/enc_LRR.Rdata')

sites_used <- enc_local_LR %>% 
  separate(site_loc, c('SITECODE', 'LCODE'), remove = F) %>% 
  distinct(SITECODE, region)

enc_sf <- left_join(sites_used, sites) %>% 
  sf::st_as_sf(coords = c('long', 'lat'))  %>% 
  # set geographic crs
  sf::st_set_crs(4326) %>% 
  # combine geometry within regions
  group_by(region) %>% 
  summarise(geometry = sf::st_combine(geometry)) %>% 
  ungroup()

# calculate the convex hulls for these studies and apply the
enc_extent_hull <- enc_sf %>% 
  sf::st_convex_hull() 


# and calculate the area of the hulls
enc_extent_temp <- enc_extent_hull %>% 
  group_by(region) %>% 
  sf::st_area() 

# centroid of hulls for map
enc_extent_centroid <- enc_sf %>% 
  sf::st_centroid() 

enc_extent_centroid_coords <- enc_extent_centroid %>% 
  sf::st_coordinates()


#  join area with SourceID and Year
enc_extent <- bind_cols(
  tibble(enc_extent_hull, 
         extent_area = enc_extent_temp,
         extent_km2 = as.numeric(extent_area) / 1e6,
         centroid_X = as.numeric(enc_extent_centroid_coords[,'X']),
         centroid_Y = as.numeric(enc_extent_centroid_coords[,'Y']))) %>% 
  # add grain data
  mutate(grain_m2 = case_when(region=='enc_butterflies' ~ 1500 * 5, # median length of transect x transect width
                              region=='enc_moths' ~ 0.56 * 0.56, # dimensions of Rothamsted light trap box ( * 0.46 deep)
                              region=='enc_beetles' ~ pi * 0.0375 ^ 2)) # area of top of pitfall trap

save(enc_extent, 
     file = '~/Dropbox/1current/spatial_composition_change/results/enc_extent.Rdata')
