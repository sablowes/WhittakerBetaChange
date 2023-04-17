# combine meta data (regional-scale covariates) with LRR
library(tidyverse)
library(sf)
# these are the data we want to join the meta data to:
load('~/Dropbox/1current/spatial_composition_change/results/allLRR.Rdata')

# but, first we got some small wrangling to do...and we need 
load('~/Dropbox/1current/spatial_composition_change/results/bt_LRR-new.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/rft_LRR.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/homog_LRR.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/Sonly_LRR.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/invert_LRR.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/mosquito_LRR.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/enc_LRR.Rdata')

# need to tidy up the names in the homogenisation regional column (they blow up post-processing models),
# and there are some regions with the same name (that belong to different dataset_ids)
# we need these to join with the LRR dataframe
homog_regional_level <- homog_local_LR %>% 
  distinct(dataset_id, regional, regional_level)


load('~/Dropbox/1current/spatial_composition_change/results/bt_extent.Rdata')
# create the regional_level covariate used in the LRR data for joining
bt_meta <- bt_extent %>% 
  mutate(regional_level = paste0('bt_', STUDY_ID),
         database = 'BioTIME',
         sample_type = 'resurvey',
         gamma_sum_grains_km2 = NA) %>% 
  # rename for consistency
  rename(taxon = TAXA,
         realm = REALM,
         grain_km2 = GRAIN_SQ_KM,
         gamma_bounding_box_km2 = extent_km2,
         longitude = centroid_X,
         latitude = centroid_Y)

load('~/Dropbox/1current/spatial_composition_change/results/rft_extent.Rdata')

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

resurvey_meta <- read_csv('~/Dropbox/BioTimeX/Local-Regional Homogenization/_data_extraction/metacommunity-survey-metadata.csv')
checklist_meta <- read_csv('~/Dropbox/BioTimeX/Local-Regional Homogenization/_data_extraction/checklist_change_metadata.csv')

homog_meta <- bind_rows(resurvey_meta %>% 
                          mutate(effort = as.character(effort)),
                        checklist_meta)

# join with regional labels
homog_meta2 <- left_join(homog_meta,
                        homog_regional_level) %>% 
  # throw out unused data
  filter(!is.na(regional_level)) %>% 
  # create sample_type covariate
  mutate(sample_type = ifelse(is.na(study_type), 'checklist', 'resurvey'))

# reduce to years, sites of interest
reg_loc <- homog_local_LR %>% 
  unite(reg_loc, c(dataset_id, regional, local), remove = F) %>% 
  distinct(reg_loc)

# there are some duplicates between Roel's data and the data that Alban has compiled
dupes <- c('countryside_survey_plants_2017_England', 'magnuson_2020_North Temperate Lakes', 
           'schuch_2011_Lower Saxony', 'valtonen_2018_Hungary')

homog_meta2 <- homog_meta2 %>% 
  unite(dsreg, c(dataset_id, regional), remove = F) %>% 
  filter(!dsreg %in% dupes) %>% 
  select(-dsreg) %>% 
  unite(rl, c(dataset_id, regional, local), remove = F) %>% 
  filter(rl %in% reg_loc$reg_loc)

# need to calculate centroid for plotting regions as a single point
homog_centroid <- homog_meta2 %>% 
  distinct(dataset_id, regional, regional_level, longitude, latitude) %>%
  # still have a few coords to get
  filter(!is.na(longitude) & !is.na(latitude)) %>%
  st_as_sf(coords = c('longitude', 'latitude'))  %>% 
  # set geographic crs
  st_set_crs(4326) %>% 
  # combine geometry within SourceID (==regions)
  group_by(dataset_id, regional_level) %>% 
  summarise(geometry = st_combine(geometry)) %>% 
  ungroup() %>% 
  st_centroid() %>% 
  mutate(longitude = st_coordinates(.)[,'X'],
         latitude = st_coordinates(.)[,'Y'])

# there is at least one extent estimate for every region
homog_meta2 %>% 
  filter(is.na(gamma_bounding_box_km2) & is.na(gamma_sum_grains_km2))
# these are the studies missing bounding boxes; they are all checklists
check <- homog_meta2 %>% 
  filter(is.na(gamma_bounding_box_km2)) %>% 
  distinct(dataset_id)

check %>% 
  filter(!dataset_id %in% checklist_meta$dataset_id)

homog_meta2 %>% 
  filter(is.na(gamma_bounding_box_km2)) %>% 
  distinct(dataset_id, gamma_sum_grains_km2)

# todo fix extents for studies where I have not used all sites
# reduce to essentials before joining
homog_meta3 <- homog_meta2 %>% 
  # some studies have multiple extents (for different years), the number of sites are consistent in my analysis, here are the corresponding sum of grains
  # mutate(gamma_sum_grains_km2 = case_when((dataset_id=='alber_2022' & regional=='GCE1') ~ 0.5*7, # 
  #                                         (dataset_id=='alber_2022' & regional=='GCE10') ~ 0.5*11, # 
  #                                         (dataset_id=='alber_2022' & regional=='GCE2') ~ 0.5*7, # 
  #                                         (dataset_id=='alber_2022' & regional=='GCE3') ~ 0.5*4, # 
  #                                         (dataset_id=='alber_2022' & regional=='GCE4') ~ 0.5*8, # 
  #                                         dataset_id=='anderson_2019b' ~ 0.0625*5, # 0.25m x 0.25m quadrat (X5)
  #                                         dataset_id=='arntzen_2017' ~ 0,
  #                                         dataset_id=='burlakova_2021' ~ 0,
  #                                         dataset_id=='christensen_2021' ~ 56 * 1e-6,
  #                                         dataset_id=='closset-kopp_2018' ~ 0,
  #                                         dataset_id=='countryside_survey_invertebrates_2017' ~ 0,
  #                                         dataset_id=='countryside_survey_macrophytes_2017' ~ 0,
  #                                         dataset_id=='countryside_survey_plants_2017' ~ 0,
  #                                         dataset_id=='dugan_2021a' ~ 0.05,
  #                                         dataset_id=='dugan_2021b' ~ 0.05,
  #                                         dataset_id=='green_2021' ~ 0,
  #                                         dataset_id=='muthukrishnan_2019' ~ 0,
  #                                         dataset_id=='santana_2017' ~ 0,
  #                                         dataset_id=='starko_2019' ~ 0,
  #                                         dataset_id=='swenson_2020' ~ 0,
  #                                         dataset_id=='van-cleve_2021' ~ 0.0636,
  #                                         dataset_id=='werner_2014' ~ 0,
  #                                         dataset_id=='willig_2010' ~ 0.00110,  
  #                                         TRUE ~ as.numeric(gamma_sum_grains_km2))) %>% 
  distinct(dataset_id, regional, regional_level, sample_type, gamma_bounding_box_km2, gamma_sum_grains_km2, realm, taxon) %>% 
  mutate(database = 'Homogenisation',
         realm = case_when(realm=='terrestrial' ~ 'Terrestrial',
                           realm=='marine' ~ 'Marine',
                           realm=='freshwater' ~ 'Freshwater',
                           TRUE ~ realm)) %>% 
  # put central coords in 
  left_join(homog_centroid) %>% 
  select(-geometry) 
  
# average multiple extents for now...needs fixing
homog_meta3 <- homog_meta3 %>% 
  group_by(dataset_id, regional_level, realm, taxon, sample_type) %>% 
  summarise(gamma_sum_grains_km2 = mean(gamma_sum_grains_km2),
            gamma_bounding_box_km2 = mean(gamma_bounding_box_km2)) %>% 
  ungroup() %>% 
  left_join(homog_centroid %>% 
              as_tibble() %>% 
              select(-geometry)) %>% 
  mutate(database = 'Homogenisation')

homog_regional_LR %>% 
  filter(!regional_level %in% homog_meta3$regional_level)

# Sonly metadata
Sonly_regional_level <- richness_only_regional_LR %>% 
  distinct(dataset_id, regional) %>% 
  mutate(regional_level = as.character(1:n()))

Sonly_meta <- read_csv('~/Dropbox/BioTimeX/Local-Regional Homogenization/_richness/metadata.csv') %>% 
  right_join(Sonly_regional_level) %>% 
  mutate(database = 'Sonly',
         regional_level = paste0('Sonly_', regional_level),
         sample_type = 'resurvey',
         realm = case_when(realm=='freshwater' ~ 'Freshwater',
                           realm=='marine' ~ 'Marine',
                           realm=='terrestrial' ~ 'Terrestrial',
                           TRUE ~ realm)) %>% 
  # drop repeat rows for each year
  distinct(dataset_id, regional_level, .keep_all = TRUE) 

# there is at least one extent estimate for every region
Sonly_meta %>% 
  filter(is.na(gamma_bounding_box_km2) & is.na(gamma_sum_grains_km2))

# Invertbrate metadata
load('~/Dropbox/1current/spatial_composition_change/data/invert-location-plots.Rdata')

invert_years_max_loc <- invert_years_max_loc %>% 
  unite(study_yr1, c(Datasource_ID, year1), remove = FALSE) %>% 
  unite(study_yr2, c(Datasource_ID, year2), remove = FALSE) 

# filter to reduce to the years that maximise number of sites
invert_filtered_2timeOnly <- invert_filtered %>% 
  unite(study_yr, c(Datasource_ID, Year), remove = FALSE) %>% 
  filter(study_yr %in% invert_years_max_loc$study_yr1 | study_yr %in% invert_years_max_loc$study_yr2) %>% 
  group_by(Datasource_ID, loc_plot) %>% 
  mutate(fYear = case_when(Year==min(Year) ~ 'start',
                           Year==max(Year) ~ 'end',
                           (Year!=min(Year) | Year!=max(Year)) ~ 'intermediate')) %>% 
  ungroup() 

invert_sf <- invert_filtered_2timeOnly %>% 
  distinct(Datasource_ID, loc_plot, Longitude, Latitude) %>% 
  st_as_sf(coords = c('Longitude', 'Latitude'))  %>% 
  # set geographic crs
  st_set_crs(4326) %>% 
  # combine geometry within Datasource_ID (==regions)
  group_by(Datasource_ID) %>% 
  summarise(geometry = st_combine(geometry)) %>% 
  ungroup()
  

invert_extent_hull <- invert_sf %>% 
  st_wrap_dateline(options = c("WRAPDATELINE=YES")) %>% 
  st_convex_hull() 


# and calculate the area of the hulls
invert_extent_temp <- invert_extent_hull %>% 
  group_by(Datasource_ID) %>% 
  st_area() 

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
         centroid_Y = as.numeric(invert_extent_centroid[,'Y'])))


invert_meta <- invert_filtered_2timeOnly %>% 
  distinct(Datasource_ID, Realm) %>% 
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
)

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

all_meta <- bind_rows(bt_meta %>% 
                     select(database, regional_level, sample_type, realm, taxon, 
                            gamma_bounding_box_km2, gamma_sum_grains_km2, longitude, latitude) %>% 
                       as_tibble() %>% 
                       select(-geometry),
                   rft_meta %>% 
                     select(database, regional_level, sample_type, realm, taxon, 
                            gamma_bounding_box_km2, gamma_sum_grains_km2, longitude, latitude),
                   homog_meta3 %>% 
                     select(database, regional_level, sample_type, realm, taxon, 
                            gamma_bounding_box_km2, gamma_sum_grains_km2, longitude, latitude) %>% 
                     mutate(regional_level = paste0('h_', regional_level)),
                   Sonly_meta %>% 
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
all_meta <- all_meta %>% 
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

all_meta %>% 
  filter(taxon_mod=='Benthos')

taxon %>% 
  filter(taxon=='all' | taxon=='multiple taxa')

# find sorte_2018
homog_meta3 %>% 
  filter(dataset_id=='sorte_2018b')

all_meta %>% 
  filter(taxon_mod=='Multiple taxa')

all_meta <- all_meta %>% 
  mutate(taxon_mod = case_when(
    # benthos
    regional_level=='bt_78' ~ 'Invertebrates',
    regional_level=='bt_110' ~ 'Invertebrates',
    regional_level=='bt_162' ~ 'Invertebrates',
    regional_level=='bt_163' ~ 'Fish',
    regional_level=='bt_196' ~ 'Invertebrates',
    regional_level=='bt_468' ~ 'Invertebrates',
    regional_level=='bt_469' ~ 'Invertebrates',
    # all or multiple taxa
    regional_level=='bt_166' ~ 'Birds',
    regional_level=='bt_213' ~ 'Fish',
    regional_level=='bt_274' ~ 'Invertebrates',
    regional_level=='bt_428' ~ 'Fish',
    regional_level=='bt_505' ~ 'Fish',
    regional_level=='bt_521' ~ 'Mammals',
    regional_level=='bt_527' ~ 'Birds',
    regional_level=='88' ~ 'Plants',
    TRUE ~ taxon_mod)
    )


all_meta <- all_meta %>% 
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

local_LRR %>% 
  distinct(database, regional_level)

all_meta %>% 
  distinct(database, regional_level)

as_tibble(all_meta) %>% 
  group_by(database) %>% 
  summarise(n())

local_LRR <- left_join(local_LRR,
                       all_meta)

regional_LRR <- left_join(regional_LRR,
                       all_meta)

regional_jknife_LRR <- left_join(regional_jknife_LRR,
                                 all_meta)

save(local_LRR,
     regional_LRR,
     regional_jknife_LRR,
     all_meta,
     file = '~/Dropbox/1current/spatial_composition_change/results/allLRR_meta.Rdata')

save(all_meta, file = '~/Dropbox/1current/spatial_composition_change/data/all_meta.Rdata')

