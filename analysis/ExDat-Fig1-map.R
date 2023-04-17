# code to plot map of grid-cell counts of all locations in analysis
library(tidyverse)
library(sf)
library(dggridR)
# these are the data we want to join the meta data to:
load('~/Dropbox/1current/spatial_composition_change/results/allLRR.Rdata')

# but, first we got some wrangling to do...and we need 
load('~/Dropbox/1current/spatial_composition_change/results/bt_LRR.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/rft_LRR.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/homog_LRR.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/Sonly_LRR.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/invert_LRR.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/mosquito_LRR.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/enc_LRR.Rdata')

resurvey_meta <- read_csv('~/Dropbox/BioTimeX/Local-Regional Homogenization/_data_extraction/metacommunity-survey-metadata.csv')
checklist_meta <- read_csv('~/Dropbox/BioTimeX/Local-Regional Homogenization/_data_extraction/checklist_change_metadata.csv')
rft_meta <- read_csv('~/Dropbox/1current/data/RivFishTime/1873_10_1873_2_RivFishTIME_TimeseriesTable.csv') %>% 
  select(-X13)
invert_meta <- read_csv('~/Dropbox/1current/spatial_composition_change/data/Insect Metacommunities Metadata.csv')

# need to tidy up the names in the homogenisation regional column (they blow up post-processing models),
# and there are some regions with the same name (that belong to different dataset_ids)
# we need these to join with the LRR dataframe
homog_regional_level <- homog_local_LR %>% 
  distinct(dataset_id, regional) %>% 
  mutate(regional_level = as.character(1:n()))

# there are some duplicates between Roel's data and the data that Alban has compiled
dupes <- c('countryside_survey_plants_2017_England', 'magnuson_2020_North Temperate Lakes', 
           'schuch_2011_Lower Saxony', 'valtonen_2018_Hungary')



load('~/Dropbox/1current/spatial_composition_change/results/bt_extent.Rdata')
# create the regional_level covariate used in the LRR data for joining
bt_meta <- bt_extent %>% 
  mutate(regional_level = paste0('bt_', STUDY_ID),
         database = 'BioTIME',
         sample_type = 'resurvey') %>% 
  # rename for consistency
  rename(taxon = TAXA,
         realm = REALM,
         grain_km2 = GRAIN_SQ_KM,
         gamma_extent_km2 = extent_km2,
         longitude = centroid_X,
         latitude = centroid_Y) %>% 
  select(-geometry)

load('~/Dropbox/1current/spatial_composition_change/results/rft_extent.Rdata')

rft_grain <- read_csv('~/Dropbox/1current/data/RivFishTime/1873_10_1873_2_RivFishTIME_SurveyTable.csv') %>% 
  distinct(TimeSeriesID, UnitAbundance)

rft_meta0 <- read_csv('~/Dropbox/1current/data/RivFishTime/1873_10_1873_2_RivFishTIME_TimeseriesTable.csv') 

rft_grain <- left_join(rft_grain, rft_meta0 %>% distinct(SourceID, TimeSeriesID)) %>% 
  distinct(SourceID, UnitAbundance)

# put taxa and realm into rft; create regional level and database covariates for joining with LRR
rft_meta <- rft_extent %>% 
  rename(gamma_extent_km2 = extent_km2,
         longitude = centroid_X,
         latitude = centroid_Y) %>% 
  mutate(taxon = 'Fish',
         realm = 'Freshwater',
         regional_level = paste0('rft_', SourceID),
         database = 'RivFishTime',
         sample_type = 'resurvey')

# want a filter to reduce locations to data used in the analysis
loc_filter <- homog_local_LR %>% 
  unite(location_filter, c(dataset_id, regional), remove = FALSE) %>% 
  distinct(location_filter)

# join with regional labels
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


# need to calculate centroid for plotting regions as a single point
homog_centroid <- homog_meta2 %>% 
  filter(!is.na(regional_level)) %>% 
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

# reduce to essentials before joining
homog_meta2 <- homog_meta2 %>% 
  filter(!is.na(regional_level)) %>% 
  distinct(dataset_id, regional, regional_level, sample_type, realm, taxon) %>% 
  unite(dsreg, c(dataset_id, regional), remove = F) %>% 
  filter(!dsreg %in% dupes) %>% 
  select(-dsreg) %>% 
  mutate(database = 'Homogenisation',
         realm = ifelse(realm=='freshwater', 'Freshwater',
                        ifelse(realm=='marine', 'Marine', 'Terrestrial'))) %>% 
  # put coords in 
  left_join(homog_centroid) %>% 
  select(-geometry)

# richness only 
Sonly_meta <- read_csv('~/Dropbox/BioTimeX/Local-Regional Homogenization/_richness/metadata.csv') %>% 
  distinct(dataset_id, regional, local, latitude, longitude, .keep_all = TRUE) %>% 
  mutate(sample_type = 'resurvey')
  

# want a map showing all locations in analysis
load('~/Dropbox/1current/spatial_composition_change/data/invert-location-plots.Rdata')

invert_years_max_loc <- invert_years_max_loc %>% 
  unite(study_yr1, c(Datasource_ID, year1), remove = FALSE) %>% 
  unite(study_yr2, c(Datasource_ID, year2), remove = FALSE) 

# filter to reduce to the years that max sites
invert_filtered_locations <- invert_filtered %>% 
  unite(study_yr, c(Datasource_ID, Year), remove = FALSE) %>% 
  filter(study_yr %in% invert_years_max_loc$study_yr1 | study_yr %in% invert_years_max_loc$study_yr2) %>% 
  group_by(Datasource_ID, loc_plot) %>% 
  mutate(fYear = case_when(Year==min(Year) ~ 'start',
                           Year==max(Year) ~ 'end',
                           (Year!=min(Year) | Year!=max(Year)) ~ 'intermediate')) %>% 
  ungroup() %>% 
  filter(fYear!='Intermediate') %>% 
  distinct(Datasource_ID, loc_plot, Longitude, Latitude) 
  

# enc meta
load('~/Dropbox/1current/spatial_composition_change/results/enc_extent.Rdata')

enc_sites <- enc_extent %>% 
  st_as_sf() %>% 
  st_cast(to = 'POINT') %>% 
  mutate(Longitude = st_coordinates(.)[,1],
         Latitude = st_coordinates(.)[,2]) %>% 
  as_tibble() %>% 
  mutate(regional_level = paste0('i_', region))

enc_meta <- tibble(database = 'Invertebrate',
                   regional_level = paste0('i_', enc_extent$region),
                   sample_type = 'resurvey',
                   taxon = 'Invertebrates',
                   realm = 'Terrestrial') %>% 
  right_join(enc_sites)

all_locations <- bind_rows(bt_local_LR %>% 
                             separate(loc_plot, c('longitude', 'latitude', 'plot'), sep = '_', convert = TRUE) %>% 
                             distinct(STUDY_ID, longitude, latitude) %>% 
                             rename(Longitude = longitude,
                                    Latitude = latitude) %>% 
                             left_join(bt_meta %>% 
                                         select(STUDY_ID, taxon, realm, sample_type) %>% 
                                         as_tibble() %>% 
                                         select(-geometry)) %>% 
                             select(-STUDY_ID) %>% 
                             mutate(database='BioTIME'),
                           rft_local_LR %>% 
                             distinct(SourceID, TimeSeriesID) %>% 
                             left_join(rft_meta0 %>% 
                                         distinct(TimeSeriesID, Latitude, Longitude)) %>% 
                             left_join(rft_meta %>% 
                                         select(SourceID, taxon, realm, sample_type) %>% 
                                         mutate(SourceID=as.numeric(SourceID))) %>% 
                             select(-SourceID, -TimeSeriesID) %>% 
                             mutate(database = 'RivFishTime'),
                           homog_meta2 %>% 
                             unite(location_filter, c(dataset_id, regional), remove = FALSE) %>% 
                             filter(location_filter %in% loc_filter$location_filter) %>% 
                             distinct(realm, taxon, sample_type, longitude, latitude) %>% 
                             rename(Longitude = longitude,
                                    Latitude = latitude) %>% 
                             mutate(database = 'Homogensiation') %>% 
                             select(Latitude, Longitude, taxon, realm, sample_type, database),
                           Sonly_meta %>% 
                             rename(Longitude = longitude,
                                    Latitude = latitude) %>% 
                             mutate(database = 'Sonly') %>% 
                             select(Latitude, Longitude, taxon, realm, sample_type, database),
                           invert_local_LR %>% 
                             left_join(invert_filtered_locations) %>% 
                             left_join(invert_meta ) %>% 
                             rename(realm = Realm) %>% 
                             mutate(taxon = 'invertebrates',
                                    sample_type = 'resurvey',
                                    database = 'Invertebrate') %>% 
                             select(Latitude, Longitude, realm, taxon, sample_type, database),
                           mosquito_local_LR %>% 
                             separate(plot, c('Longitude', 'Latitude'), sep = '_', convert = TRUE) %>% 
                             mutate(realm = 'Terrestrial',
                                    taxon = 'Invertebrates',
                                    sample_type = 'resurvey',
                                    database = 'Invertebrate') %>% 
                             select(Latitude, Longitude, realm, taxon, sample_type, database),
                           enc_meta %>% 
                             select(Latitude, Longitude, realm, taxon, sample_type, database))

all_locations <- all_locations %>% 
  mutate(realm = case_when(realm=='freshwater' ~ 'Freshwater',
                           realm=='marine' ~ 'Marine',
                           realm=='terrestrial' ~ 'Terrestrial',
                           TRUE ~ as.character(realm)),
         taxon_mod = case_when(taxon=='fish' ~ 'Fish',
                                      taxon=='birds' ~ 'Birds',
                                      (taxon=='amphibians'|taxon=='Amphibians'|taxon=='herpetofauna') ~ 'Herpetofauna',
                                      taxon=='mammals' ~ 'Mammals',
                                      taxon=='invertebrates' ~ 'Invertebrates',
                                      # collapse two groups with small sample sizes into multiple taxa
                                      taxon=='Benthos' ~ 'Multiple taxa',
                                      taxon=='coral' ~ 'Multiple taxa',
                                      taxon=='algae' ~ 'Plants',
                                      taxon=='molluscs' ~ 'Multiple taxa',
                                      taxon=='insects' ~ 'Invertebrates',
                                      taxon=='plants' ~ 'Plants',
                                      taxon=='bryophytes' ~ 'Plants',
                                      taxon=='All' ~ 'Multiple taxa',
                                      (taxon=='Marine invertebrates' | taxon=='Terrestrial invertebrates' | taxon=='Freshwater invertebrates') ~
                                        'Invertebrates',
                                      (taxon=='Marine plants' | taxon=='Terrestrial plants' | taxon=='Freshwater plants') ~ 'Plants',
                                      TRUE ~ as.character(taxon)),
         sample_type = case_when(sample_type=='checklist' ~ 'Checklist',
                                 sample_type=='resurvey' ~ 'Resurvey')) 
        

# make dggrid version
dgg <- dgconstruct(res=6)
# dginfo(dgg)
##	get the corresponding grid cells for all observations
all_locations <- all_locations %>% 
  mutate(cell = dgGEO_to_SEQNUM(dggs = dgg, in_lon_deg = Longitude, in_lat_deg = Latitude)$seqnum)

cell_count <- all_locations %>% 
  group_by(cell) %>% 
  summarise(count = n())

grid <- dgcellstogrid(dggs = dgg,
                      cells = all_locations$cell) %>% 
  left_join(cell_count %>% 
              mutate(seqnum = as.numeric(cell)), 
            by = 'seqnum') %>% 
  st_make_valid()

# prelim map
world <- map_data('world')

# locations_cell_map <- 
ggplot() + 
  geom_polygon(data=world,
               aes(long, lat, group = group), colour='black', fill='#ffffff', size=0.25) +
  geom_sf(data=grid,
               aes(geometry = geometry, fill=count), alpha = 0.9) +
  geom_sf(data=grid,
               aes(geometry = geometry), alpha=0.4, color="white") +
  # coord_map('mollweide', ylim = c(-80, 90), xlim = c(-180, 180)) +
  coord_sf(ylim = c(-80, 90)) +
  scale_x_continuous(name = 'Longitude', breaks = seq(-180, 180, by = 30)) +
  scale_y_continuous(name = 'Latitude', breaks = c( -23.5, 23.5, -35, 35, -60, 60)) +
  scale_fill_distiller(name = 'Number of locations',
                       trans = 'log10', type = 'seq',
                       palette = 'YlGnBu', direction = -1) +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = 'black', size = 0.1), 
        panel.border = element_blank(),
        axis.ticks = element_blank(), 
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = 'top',
        legend.direction = 'horizontal',
        # legend.justification = c(1,1),
        # plot.tag = element_text(size = 8, face = "bold"),
        plot.margin = unit(c(0,0,0,0), units = 'mm'),
        legend.margin = margin(),
        legend.box.spacing = unit(c(0,0,0,0), units = 'mm')) +
  guides(fill = guide_legend( alpha = 0.9))

ggsave('~/Dropbox/MoBD (Measurements of Beta diversity)/Homogenization Paper/Draft of Manuscript/figures/ExDat-Fig1-col.pdf',
       width = 290, height = 200, units = 'mm')
