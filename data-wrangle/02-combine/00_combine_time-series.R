# compile time series to fit separate models for alpha- and gamma-scales

rm(list=ls())
library(tidyverse)


load('~/Dropbox/1current/spatial_composition_change/results/bt_timeSeries_metrics-new.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/rft_metric-time-series.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/resurvey_timeSeries_metrics.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/invert_metric-time-series.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/mosquito_metric_timeSeries.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/enc_timeSeries.Rdata')

# tidy up regional levels in Resurvey database
rs_rl <- resurvey_alpha %>% 
  distinct(regional_level) %>% 
  mutate(rl2 = paste0('resurvey_', 1:n()))

rs_ll <- resurvey_alpha %>% 
  distinct(regional_level, local) %>% 
  group_by(regional_level) %>% 
  mutate(local_level = 1:n()) %>% 
  ungroup()

resurvey_levels <- left_join(rs_rl,
                             rs_ll) %>% 
  unite(ll, c(rl2, local_level), remove = FALSE)

resurvey_levels2 <- left_join(resurvey_levels,
          resurvey_alpha %>% 
            distinct(dataset_id, regional_level))

# save these for identifying studies 
save(resurvey_levels2, 
     file = '~/Dropbox/1current/spatial_composition_change/data/resurvey-levels.Rdata')

resurvey_alpha <- left_join(resurvey_alpha,
                            resurvey_levels)

resurvey_gamma <- left_join(resurvey_gamma,
                            resurvey_levels %>% 
                              distinct(regional_level, rl2))

# there are some duplicates between Roel's data and the data that Alban has compiled
dupes <- c('countryside_survey_plants_2017_England', 'magnuson_2020_North Temperate Lakes', 
           'schuch_2011_Lower Saxony', 'valtonen_2018_Hungary')

resurvey_alpha <- resurvey_alpha %>% 
  filter(!regional_level %in% dupes)

resurvey_gamma <- resurvey_gamma %>% 
  filter(!regional_level %in% dupes)

# first let's create time series using the two scale calculations (S, eH, S_PIE (insimspon))
alpha_ts <- bind_rows(bt_alpha %>% 
                        mutate(database = 'BioTIME',
                               regional_level = paste0('bt_', STUDY_ID),
                               local_level = paste0('bt_', loc_plot)) %>% 
                        rename(year = YEAR,
                               J = N) %>% 
                        select(-c(STUDY_ID, loc_plot)),
                      rft_alpha %>% 
                        mutate(database = 'RivFishTime') %>% 
                        mutate(regional_level = paste0('rft_', SourceID),
                               local_level = paste0('rft', TimeSeriesID), 
                               year = Year) %>% 
                        mutate(regional_level = as.character(regional_level)) %>% 
                        select(-c(nsites, n_yrs, SourceID, TimeSeriesID, Year)),
                      resurvey_alpha %>% 
                        select(-c(dataset_id, regional_level, local, local_level)) %>% 
                        rename(regional_level = rl2, 
                               local_level = ll) %>% 
                        mutate(database = 'Resurvey'),
                      invert_alpha %>% 
                        rename(year = Year) %>% 
                        mutate(database = 'Invertebrates',
                               regional_level = paste0('i_', Datasource_ID),
                               local_level = paste0('i_', loc_plot)) %>% 
                        select(database, regional_level, local_level, year, S, eH, S_PIE, J),
                      mosquito_alpha_timeSeries %>% 
                        mutate(regional_level = paste0('i_', region),
                               local_level = paste0('i_', plot),
                               database = 'Invertebrates') %>% 
                        # tidy one regional_level
                        mutate(regional_level = ifelse(regional_level=='i_Lee (Florida (United States))',
                                                       'i_Lee', regional_level)) %>% 
                        select(-c(region, plot)),
                      enc_alpha_timeSeries %>% 
                        mutate(database = 'Invertebrates') %>% 
                        rename(regional_level = region,
                               local_level = site_loc,
                               S = Sbar,
                               eH = eHbar,
                               S_PIE = S_PIEbar,
                               J = Jbar) %>% 
                        mutate(regional_level = paste0('i_', regional_level))) 

gamma_ts <- bind_rows(bt_gamma %>% 
                        mutate(database = 'BioTIME',
                               regional_level = paste0('bt_', STUDY_ID)) %>% 
                        rename(year = YEAR,
                               J = N) %>% 
                        select(-c(STUDY_ID)),
                      rft_gamma %>% 
                        mutate(database = 'RivFishTime',
                               regional_level = paste0('rft_', SourceID),
                               year = Year) %>% 
                        select(-c(SourceID, Year)),
                      resurvey_gamma %>% 
                        select(-c(dataset_id, regional_level)) %>% 
                        rename(regional_level = rl2) %>% 
                        mutate(database = 'Resurvey'),
                      invert_gamma %>% 
                        mutate(regional_level = paste0('i_', Datasource_ID),
                               year = Year,
                               database = 'Invertebrates') %>% 
                        select(-c(Datasource_ID, Year)),
                      mosq_gamma_S %>% 
                        mutate(regional_level = paste0('i_', region),
                               database = 'Invertebrates',
                               # tidy one level
                               regional_level = ifelse(regional_level=='i_Lee (Florida (United States))',
                                                       'i_Lee', regional_level)) %>% 
                        select(-c(region)),
                      enc_gamma_timeSeries %>% 
                        mutate(database = 'Invertebrates') %>% 
                        mutate(regional_level = paste0('i_', region)) %>% 
                        select(-region))

save(alpha_ts,
     gamma_ts,
     file = '~/Dropbox/1current/spatial_composition_change/results/all_timeSeries.Rdata')

# visual inspection of time series
r <- alpha_ts %>% 
  distinct(regional_level) %>% 
  pull()

pdf('~/Dropbox/1current/spatial_composition_change/figures/data-visualisation/alpha-ts-inspection.pdf', width = 12, height = 9)

for(i in 1:length(r)){
  print(paste('region', i, 'of', length(r), 'regions'))
  
  p = ggplot() +
    # facet_wrap(~YEAR) +
    geom_point(data = alpha_ts %>% 
                 filter(regional_level == r[i]),
               aes(x = year, y = S)) +
    stat_smooth(data = alpha_ts %>% 
                  filter(regional_level == r[i]),
                aes(x = year, y = S, group = local_level), 
                lty = 2, size = 0.5,
                method = 'lm', se = FALSE) +
    stat_smooth(data = alpha_ts %>% 
                  filter(regional_level == r[i]),
                aes(x = year, y = S), 
                lty = 1,
                method = 'lm', se = FALSE) +
    scale_y_continuous(trans = 'log2') +
    labs(subtitle = paste0(r[i])) +
    theme(legend.position = 'none')
  
  
  print(p)
}
dev.off()

# number of individuals in assemblage (all species combined) (J) 
pdf('~/Dropbox/1current/spatial_composition_change/figures/data-visualisation/alpha-J-ts-inspection.pdf', width = 12, height = 9)

for(i in 1:length(r)){
  print(paste('region', i, 'of', length(r), 'regions'))
  
  p = ggplot() +
    # facet_wrap(~YEAR) +
    geom_point(data = alpha_ts %>% 
                 filter(regional_level == r[i]),
               aes(x = year, y = J)) +
    stat_smooth(data = alpha_ts %>% 
                  filter(regional_level == r[i]),
                aes(x = year, y = J, group = local_level), 
                lty = 2, size = 0.5,
                method = 'lm', se = FALSE) +
    stat_smooth(data = alpha_ts %>% 
                  filter(regional_level == r[i]),
                aes(x = year, y = J), 
                lty = 1,
                method = 'lm', se = FALSE) +
    scale_y_continuous(trans = 'log2') +
    labs(subtitle = paste0(r[i])) +
    theme(legend.position = 'none')
  
  
  print(p)
}
dev.off()


pdf('~/Dropbox/1current/spatial_composition_change/figures/data-visualisation/gamma-ts-inspection.pdf', width = 12, height = 9)

for(i in 1:length(r)){
  print(paste('region', i, 'of', length(r), 'regions'))
  
  p = ggplot() +
    # facet_wrap(~YEAR) +
    geom_point(data = gamma_ts %>% 
                 filter(regional_level == r[i]),
               aes(x = year, y = S)) +
    stat_smooth(data = gamma_ts %>% 
                  filter(regional_level == r[i]),
                aes(x = year, y = S), 
                lty = 1,
                method = 'lm', se = FALSE) +
    scale_y_continuous(trans = 'log2') +
    labs(subtitle = paste0(r[i])) +
    theme(legend.position = 'none')
  
  
  print(p)
}
dev.off()


# meta-data wrangle
library(sf)
# BioTIME
bt_meta <- read_csv('~/Dropbox/BioTIMELatest/bioTIMEMetadataJune2021.csv') %>% 
  filter(STUDY_ID %in% unique(bt_gamma$STUDY_ID))

bt_sf <- bt_alpha %>% 
  select(STUDY_ID, loc_plot) %>% 
  tidyr::separate(loc_plot, c('Longitude', 'Latitude', 'Plot'), remove = FALSE, sep = '_') %>% 
  st_as_sf(coords = c('Longitude', 'Latitude'))  %>% 
  # set geographic crs
  st_set_crs(4326) %>% 
  # combine geometry within SourceID (==regions)
  group_by(STUDY_ID) %>% 
  summarise(geometry = st_combine(geometry)) %>% 
  ungroup()

# todo use spherical geometry
sf_use_s2(use_s2 = FALSE)

# calculate the convex hulls for these studies and apply the
bt_extent_hull <- bt_sf %>% 
  # st_wrap_dateline(options = c("WRAPDATELINE=YES")) %>% 
  st_convex_hull() 

# some are points (zero extent), but some are lines
bt_line <- bt_extent_hull %>% 
  mutate(type = st_geometry_type(geometry)) %>% 
  filter(type == 'LINESTRING')

# and calculate the area of the hulls
bt_extent_temp <- bt_extent_hull %>% 
  group_by(STUDY_ID) %>% 
  st_area() 

# calculate length of lines
bt_extent_line <- bt_line %>% 
  group_by(STUDY_ID) %>% 
  mutate(length = st_length(geometry))
# centroid of hulls for map
bt_extent_centroid <- bt_sf %>% 
  st_centroid() 

bt_extent_centroid_coords <- bt_extent_centroid %>% 
  st_coordinates()

#  
bt_extent <- bind_cols(
  tibble(bt_extent_hull, 
         extent_area = bt_extent_temp,
         extent_km2 = as.numeric(extent_area) / 1e6,
         centroid_X = as.numeric(bt_extent_centroid_coords[,'X']),
         centroid_Y = as.numeric(bt_extent_centroid_coords[,'Y']))) %>% 
  mutate(extent_km2 = ifelse(STUDY_ID==152, bt_extent_line %>% 
                         filter(STUDY_ID==152) %>% pull(length) %>% as.numeric / 1000,
                         extent_km2),
         extent_km2 = ifelse(STUDY_ID==420, bt_extent_line %>% 
                         filter(STUDY_ID==420) %>% pull(length) %>% as.numeric / 1000,
                         extent_km2))

# put some other metadata in...
bt_meta2 <- left_join(bt_extent %>% 
                         select(-geometry) %>% 
                         rename(longitude = centroid_X,
                                latitude = centroid_Y),
                       bt_meta %>% 
                         select(STUDY_ID, TAXA, REALM, GRAIN_SQ_KM))


# RivFishTime
library(sf)
load('~/Dropbox/1current/spatial_composition_change/results/rft_extent.Rdata')
rft_extent <- rft_extent %>% 
  filter(SourceID %in% unique(rft_gamma$SourceID))

rft_grain <- read_csv('~/Dropbox/1current/data/RivFishTime/1873_10_1873_2_RivFishTIME_SurveyTable.csv') %>% 
  distinct(TimeSeriesID, UnitAbundance)

rft_meta0 <- read_csv('~/Dropbox/1current/data/RivFishTime/1873_10_1873_2_RivFishTIME_TimeseriesTable.csv') 

rft_grain <- left_join(rft_grain, 
                       rft_meta0 %>% distinct(SourceID, TimeSeriesID)) %>% 
  distinct(SourceID, UnitAbundance)

# put taxa and realm into rft; create regional level and database covariates for joining with LRR
rft_meta <- rft_extent %>% 
  rename(gamma_bounding_box_km2 = extent_km2,
         longitude = centroid_X,
         latitude = centroid_Y) %>% 
  mutate(taxon = 'Fish',
         realm = 'Freshwater',
         regional_level = paste0('rft_', SourceID),
         database = 'RivFishTime',
         sample_type = 'resurvey',
         gamma_sum_grains_km2 = NA)

# resurvey meta
resurvey_meta <- read_csv('~/Dropbox/BioTimeX/Local-Regional Homogenization/_data_extraction/metacommunity-survey_metadata-standardised.csv') %>% 
  unite(regional_level, c(dataset_id, regional), remove = FALSE) 

# filter to regional levels
rs_meta2 <- resurvey_meta %>% 
  filter(regional_level %in% unique(resurvey_gamma$regional_level))

# filter to correct local sites
rs_local <- resurvey_alpha %>% 
  distinct(regional_level, local) %>% 
  unite(ll, c(regional_level, local))

rs_geogr <- rs_meta2 %>% 
  unite(ll2, c(regional_level, local), remove = FALSE) %>% 
  filter(ll2 %in% rs_local$ll) %>% 
  distinct(regional_level, local, latitude, longitude)

rs_sf <- rs_geogr %>% 
  filter(!is.na(latitude)) %>% 
  st_as_sf(coords = c('longitude', 'latitude'))  %>% 
  # set geographic crs
  st_set_crs(4326) %>% 
  # combine geometry within Datasource_ID (==regions)
  group_by(regional_level) %>% 
  summarise(geometry = st_combine(geometry)) %>% 
  ungroup()

resurvey_extent_hull <- rs_sf %>% 
  st_wrap_dateline(options = c("WRAPDATELINE=YES")) %>% 
  st_convex_hull() %>% 
  mutate(type = st_geometry_type(geometry))

# and calculate the area of the hulls
resurvey_extent_temp <- resurvey_extent_hull %>% 
  group_by(regional_level) %>% 
  st_area() 

# centroid of hulls for map
resurvey_extent_centroid <- rs_sf %>% 
  st_centroid() %>% 
  st_coordinates(.)

#  join area with SourceID and Year
resurvey_extent <- bind_cols(
  tibble(resurvey_extent_hull, 
         extent_area = resurvey_extent_temp,
         extent_km2 = as.numeric(extent_area) / 1e6,
         centroid_X = as.numeric(resurvey_extent_centroid[,'X']),
         centroid_Y = as.numeric(resurvey_extent_centroid[,'Y'])))

resurvey_meta <- rs_geogr %>% 
  distinct(regional_level, local) %>% 
  mutate(database = 'Resurvey',
         sample_type = 'resurvey') %>% 
  left_join(rs_meta2 %>% 
              distinct(regional_level, realm, taxon)) %>% 
  mutate(realm = case_when(realm=='terrestrial' ~ 'Terrestrial',
                           realm=='marine' ~ 'Marine',
                           realm=='freshwater' ~ 'Freshwater',
                           TRUE ~ realm)) %>% 
  left_join(resurvey_extent) %>% 
  rename(gamma_bounding_box_km2 = extent_km2,
         longitude = centroid_X,
         latitude = centroid_Y) %>%
  select(-geometry) %>% 
  distinct(regional_level, .keep_all = TRUE)


# Invertbrate metadata
load('~/Dropbox/1current/spatial_composition_change/data/invert-location-plots.Rdata')

invert_yr_loc <- invert_alpha %>% 
  distinct(Datasource_ID, loc_plot, Year) %>% 
  unite(loc_yr, c(Datasource_ID, loc_plot, Year))

# filter to reduce to the years that maximise number of sites
invert_geogr <- invert_filtered %>% 
  unite(loc_yr2, c(Datasource_ID, loc_plot, Year), remove = FALSE) %>% 
  filter(loc_yr2 %in% invert_yr_loc$loc_yr) %>% 
  distinct(Datasource_ID, loc_plot, Longitude, Latitude) 

invert_sf <- invert_geogr %>% 
  st_as_sf(coords = c('Longitude', 'Latitude'))  %>% 
  # set geographic crs
  st_set_crs(4326) %>% 
  # combine geometry within Datasource_ID (==regions)
  group_by(Datasource_ID) %>% 
  summarise(geometry = st_combine(geometry)) %>% 
  ungroup()

invert_extent_hull <- invert_sf %>% 
  st_wrap_dateline(options = c("WRAPDATELINE=YES")) %>% 
  st_convex_hull() %>% 
  mutate(type = st_geometry_type(geometry))

# and calculate the area of the hulls
invert_extent_temp <- invert_extent_hull %>% 
  group_by(Datasource_ID) %>% 
  st_area() 

# length of linestrings
invert_line_length <- invert_extent_hull %>% 
  filter(type == 'LINESTRING') %>% 
  mutate(length = st_length(geometry))

# centroid of hulls for map
invert_extent_centroid <- invert_sf %>% 
  st_centroid() %>% 
  st_coordinates(.)


#  join area with SourceID and Year
invert_extent <- bind_cols(
  tibble(invert_extent_hull, 
         extent_area = invert_extent_temp,
         extent_km2 = as.numeric(extent_area) / 1e6,
         centroid_X = as.numeric(invert_extent_centroid[,'X']),
         centroid_Y = as.numeric(invert_extent_centroid[,'Y']))) %>% 
  mutate(extent_km2 = ifelse(Datasource_ID == 1347, 
                             invert_line_length %>% pull(length) %>% as.numeric / 1000,
                             extent_km2))


invert_meta <- invert_filtered %>% 
  distinct(Datasource_ID, Realm) %>% 
  filter(Datasource_ID %in% unique(invert_alpha$Datasource_ID)) %>% 
  mutate(database = 'Invertebrates',
         sample_type = 'resurvey', 
         taxon = 'Invertebrates',
         gamma_sum_grains_km2 = NA) %>% 
  left_join(invert_extent) %>% 
  rename(realm = Realm,
         gamma_bounding_box_km2 = extent_km2,
         longitude = centroid_X,
         latitude = centroid_Y) %>% 
  mutate(regional_level = paste0('i_', Datasource_ID))

# mosquito meta
load('~/Dropbox/1current/spatial_composition_change/results/mosq_extent.Rdata')

mosq_meta <- tibble(
  database = 'Invertebrates',
  regional_level = paste0('i_', mosq_extent$region),
  sample_type = 'resurvey',
  taxon = 'Invertebrates',
  realm = 'Freshwater',
  gamma_bounding_box_km2 = mosq_extent$extent_km2,
  gamma_sum_grains_km2 = NA,
  longitude = mosq_extent$centroid_X,
  latitude = mosq_extent$centroid_Y
) %>% 
  mutate(regional_level = ifelse(regional_level=='i_Lee (Florida (United States))', 'i_Lee', 
                                 regional_level))

# enc meta
load('~/Dropbox/1current/spatial_composition_change/results/enc_extent.Rdata')

enc_meta <- tibble(
  database = 'Invertebrates',
  regional_level = paste0('i_', enc_extent$region),
  sample_type = 'resurvey',
  taxon = 'Invertebrates',
  realm = 'Terrestrial',
  gamma_bounding_box_km2 = enc_extent$extent_km2,
  gamma_sum_grains_km2 = NA,
  grain_m2 = enc_extent$grain_m2,
  longitude = enc_extent$centroid_X,
  latitude = enc_extent$centroid_Y
)

ts_meta <- bind_rows(bt_meta2 %>% 
                        mutate(database = "BioTIME", 
                               regional_level = paste0('bt_', STUDY_ID),
                               sample_type = 'resurvey',
                               gamma_sum_grains_km2 = NA) %>% 
                       rename(realm = REALM, 
                              taxon = TAXA,
                              gamma_bounding_box_km2 = extent_km2) %>% 
                       select(database, regional_level, sample_type, realm, taxon, 
                              gamma_bounding_box_km2, gamma_sum_grains_km2, longitude, latitude),
                      rft_meta %>% 
                        select(database, regional_level, sample_type, realm, taxon, 
                               gamma_bounding_box_km2, gamma_sum_grains_km2, longitude, latitude),
                      resurvey_meta %>% 
                       mutate(gamma_sum_grains_km2 = NA) %>% 
                       left_join(resurvey_levels) %>% 
                       select(-regional_level) %>% 
                       rename(regional_level = rl2) %>% 
                        select(database, regional_level, sample_type, realm, taxon, 
                               gamma_bounding_box_km2, gamma_sum_grains_km2, longitude, latitude),
                      invert_meta %>% 
                        select(database, regional_level, sample_type, realm, taxon, 
                               gamma_bounding_box_km2, gamma_sum_grains_km2, longitude, latitude),
                      mosq_meta %>% 
                        select(database, regional_level, sample_type, realm, taxon, 
                               gamma_bounding_box_km2, gamma_sum_grains_km2, longitude, latitude),
                      enc_meta %>% 
                        select(database, regional_level, sample_type, realm, taxon, 
                               gamma_bounding_box_km2, gamma_sum_grains_km2, longitude, latitude))

# some tidying before merging and saving
ts_meta <- ts_meta %>% 
  mutate(taxon_mod = case_when((taxon=='amphibians'|taxon=='Amphibians'|taxon=='herpetofauna'|taxon=='Herpetofauna'|taxon=='Reptiles') ~ 'Herptiles',
                               taxon=='All' ~ 'Multiple taxa',
                               (taxon=='Marine invertebrates' | taxon=='Terrestrial invertebrates' | taxon=='Freshwater invertebrates') ~
                                 'Invertebrates',
                               (taxon=='Marine plants' | taxon=='Terrestrial plants' | taxon=='Freshwater plants') ~ 'Plants',
                               TRUE ~ as.character(taxon))) 

# reclassify some studies with taxa==Benthos or taxon=='Multiple taxa' based on dominant taxon group
load('~/Dropbox/1current/spatial_composition_change/data/taxon_reclassification.RDATA')
taxon <- taxon %>% as_tibble()

taxon %>% 
  filter(taxon=='benthos')

ts_meta %>% 
  filter(taxon_mod=='Benthos')

taxon %>% 
  filter(taxon=='all' | taxon=='multiple taxa')

ts_meta %>% 
  filter(taxon_mod=='Multiple taxa')

ts_meta <- ts_meta %>% 
  mutate(taxon_mod = case_when(
    # benthos
    regional_level=='bt_78' ~ 'Invertebrates',
    regional_level=='bt_163' ~ 'Fish',
    regional_level=='bt_196' ~ 'Invertebrates',
    regional_level=='bt_469' ~ 'Invertebrates',
    # all or multiple taxa
    regional_level=='bt_428' ~ 'Fish',
    regional_level=='bt_521' ~ 'Mammals',
    regional_level=='bt_527' ~ 'Birds',
    TRUE ~ taxon_mod)
  )


ts_meta <- ts_meta %>% 
  mutate(lat_band_3 = case_when(abs(latitude) > 60 ~ 'polar',
                                abs(latitude) < 23.5 ~ 'tropical',
                                TRUE ~ 'temperate'),
         lat_band_4 = case_when(abs(latitude) > 60 ~ 'polar',
                                abs(latitude) < 23.5 ~ 'tropical',
                                (abs(latitude) > 23.5 & abs(latitude) < 35) ~ 'subtropical',
                                TRUE ~ 'temperate'),
         # east / west hemisphere
         hemisphere = case_when((longitude > 0 & longitude < 180) ~ 'eastern',
                                TRUE ~ 'western'))

alpha_ts %>% distinct(regional_level)
gamma_ts %>% distinct(regional_level)
ts_meta %>% distinct(regional_level)

save(alpha_ts, 
     gamma_ts,
     ts_meta,
     file = '~/Dropbox/1current/spatial_composition_change/results/two-scale-time-series-plus-meta.Rdata')

save(ts_meta, file = '~/Dropbox/1current/spatial_composition_change/data/ts_meta.Rdata')

# some alpha-scale time series have a high proportion (here > 50%) of samples 
# with only a single individual; remove these regions from analyses
regions2remove <- alpha_ts %>% 
  group_by(regional_level, local_level) %>% 
  summarise(J1 = sum(J==1),
            J1S1 = sum(J==1 & S==1),
            n = n(),
            prop_J1 = J1 / n,
            prop_J1S1 = J1S1 / n) %>% 
  ungroup() %>% 
  filter(prop_J1S1 > 0.5) %>% 
  distinct(regional_level)

save(regions2remove, file='~/Dropbox/1current/spatial_composition_change/results/regions2remove.Rdata')
