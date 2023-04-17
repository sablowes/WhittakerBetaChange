# script to reduce time series to balanced site-year combinations

# partially cleaned data
load('~/Dropbox/1current/spatial_composition_change/data/bt-location-plot-time-series.Rdata')

# manual inspection results
man_inspection <- read_csv('~/Dropbox/1current/spatial_composition_change/WhittakerBetaChange/data-wrangle/01-BioTIME-wrangle/balanced-site-years/bt-time-series-visual-check.csv')

# studies to clean
to_clean <- man_inspection %>% 
  filter(max_rect=='see file') %>% 
  distinct(STUDY_ID)

# studies ready to go
ready <- man_inspection %>% 
  filter(max_rect=='ok') %>% 
  distinct(STUDY_ID)

# create filelist to loop through
setwd('~/Dropbox/1current/spatial_composition_change/WhittakerBetaChange/data-wrangle/01-BioTIME-wrangle/balanced-site-years/')
filelist <- dir(pattern="bt_")

study_site_year <- tibble()

for(file in 1:length(filelist)){
  id = as.numeric(str_split(filelist[file], pattern = '_')[[1]][2])
  temp <- read_csv(filelist[file], show_col_types = FALSE) %>% 
    mutate(STUDY_ID = id)
  
  study_site_year <- bind_rows(study_site_year,
                          temp)
}

study_site_year <- study_site_year %>% 
  unite(styrsite, c(STUDY_ID, YEAR, loc_plot), remove = FALSE)

# reduce studies to site-year combinations manually selected
bt <- read_csv('~/Dropbox/BioTIMELatest/BioTIMEQJune2021.csv')
btmeta <- read_csv('~/Dropbox/BioTIMELatest/bioTIMEMetadataJune2021.csv')

# only want count data for these analyses (need relative abundance)
count <- btmeta %>% 
  filter(ABUNDANCE_TYPE!='Presence/Absence')

bt <- bt %>% 
  filter(STUDY_ID %in% count$STUDY_ID)

study_site_yr_selected <- bt %>% 
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT), remove = FALSE) %>%
  unite(styrsite, c(STUDY_ID, YEAR, loc_plot), remove = FALSE) %>% 
  filter(styrsite %in% study_site_year$styrsite) %>% 
  select(-styrsite)

rm(bt, btmeta)

# retain studies that are ok
ok_ts <- bt_filtered_time_series %>% 
  filter(STUDY_ID %in% ready$STUDY_ID) %>% 
  filter(max_rect=='in')

# put reduced study-site-year combinations back in,
# and reduce to sites in max_rect
ok_ts <- bind_rows(ok_ts,
                   study_site_yr_selected %>% 
                     mutate(max_rect = 'in'))

# check we have the right number of studies
target <- bind_rows(to_clean, ready) %>% distinct(STUDY_ID) %>% arrange(-desc(STUDY_ID))
check <- bt_filtered_time_series %>% distinct(STUDY_ID) %>% arrange(-desc(STUDY_ID))
filter(target, !(STUDY_ID %in% check$STUDY_ID))
  

# visual inspection
r <- ok_ts %>% distinct(STUDY_ID) %>% pull()

pdf('~/Desktop/bt_clean.pdf', width = 12, height = 9)

for(i in 1:length(r)){
  print(paste('STUDY', i, 'of', length(r), 'regions'))
  
  p = ggplot() +
    # facet_wrap(~YEAR) +
    geom_point(data = ok_ts %>% 
                 filter(STUDY_ID == r[i]) %>% 
                 distinct(loc_plot, YEAR, .keep_all = TRUE),
               aes(x = YEAR, y = loc_plot, colour = max_rect)) +
    labs(subtitle = paste0('STUDY_ID = ', r[i])) +
    theme(legend.position = 'none')
  
  
  print(p)
}
dev.off()

# check duration
bt_timeSeries <- ok_ts %>% 
  group_by(STUDY_ID) %>% 
  mutate(duration = max(YEAR) - min(YEAR) + 1) %>% 
  ungroup() %>% 
  filter(duration > 9)

rm(ok_ts)

# check that we have the same number of sites each year (within studies)
bt_timeSeries %>% 
  group_by(STUDY_ID, YEAR) %>% 
  summarise(effort = n_distinct(loc_plot)) %>% 
  filter(length(unique(effort))!=1)

# get min_samps for each study-location
# standardise effort for each study
# find min number of samples per location
min_samp <- bt_timeSeries %>% 
  group_by(STUDY_ID, YEAR, loc_plot) %>% 
  summarise(n_samps = n_distinct(SAMPLE_DESC)) %>% 
  group_by(STUDY_ID) %>% 
  summarise(min_samp = min(n_samps)) %>% 
  ungroup() 

set.seed(101)

bt_filtered_standardised <- bt_timeSeries %>% 
  distinct(STUDY_ID, YEAR, loc_plot, SAMPLE_DESC) %>% 
  left_join(min_samp) %>% 
  group_by(STUDY_ID, YEAR, loc_plot) %>% 
  sample_n(size = unique(min_samp)) %>% 
  ungroup()

bt_timeSeries <- bt_timeSeries %>% 
  inner_join(bt_filtered_standardised %>% 
               dplyr::select(-min_samp))

# save data (ready for analysis)
save(bt_timeSeries,
     file = '~/Dropbox/1current/spatial_composition_change/data/bt-location-plot-time-series-clean.Rdata')
