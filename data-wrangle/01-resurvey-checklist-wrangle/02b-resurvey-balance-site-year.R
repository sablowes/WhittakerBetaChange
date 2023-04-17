# script to reduce time series to balanced site-year combinations
path2dir <- '~/Dropbox/1current/spatial_composition_change/WhittakerBetaChange/data-wrangle/01-resurvey-checklist-wrangle/balanced-site-years'
# manual processing of metacommunity resurvey sampling sites and years
load('~/Dropbox/1current/spatial_composition_change/data/resurvey-location-plots-maxRect.Rdata')

##	Get the raw data
dat <- read_csv('~/Dropbox/BioTimeX/Local-Regional Homogenization/_data_extraction/metacommunity-survey-communities.csv')

# create new regional_level for resurvey data
dat_rs <- dat %>% 
  unite(regional_level, c(dataset_id, regional), remove = FALSE) 

# manual inspection results
man_inspection <- read_csv(paste0(path2dir, '/../resurvey-time-series-datasource-inspection.csv'),
                           show_col_types = FALSE)

# studies to clean
to_clean <- man_inspection %>% 
  filter(max_rect=='see file') %>% 
  distinct(regional_level)

# studies ready to go
ready <- man_inspection %>% 
  filter(max_rect=='ok') %>% 
  distinct(regional_level)

# create filelist to loop through
setwd(paste0(path2dir))
filelist <- dir(pattern="*.csv")

study_site_year <- tibble()

for(file in 1:length(filelist)){
  id = as.character(str_split(filelist[file], pattern = '.csv')[[1]][1])
  temp <- read_csv(filelist[file], show_col_types = FALSE) %>% 
    mutate(regional_level = id,
           local = as.character(local))
  
  study_site_year <- bind_rows(study_site_year,
                          temp)
}

study_site_year <- study_site_year %>% 
  select(-c(nyrs, nsites)) %>% 
  unite(styrsite, c(regional_level, year, local), remove = FALSE)

# reduce studies to site-year combinations manually selected
study_site_yr_selected <- dat_rs %>% 
  unite(styrsite, c(regional_level, year, local), remove = FALSE) %>% 
  filter(styrsite %in% study_site_year$styrsite) %>% 
  select(-styrsite)

rm(dat, dat_rs)

# retain studies that are ok
ok_ts <- rs_filtered %>% 
  filter(regional_level %in% ready$regional_level) %>% 
  filter(max_rect=='in')

# put reduced study-site-year combinations back in,
# and reduce to sites in max_rect
ok_ts <- bind_rows(ok_ts,
                   study_site_yr_selected %>% 
                     mutate(max_rect = 'in'))

# check we have the right number of studies
target <- bind_rows(to_clean, ready) %>% distinct(regional_level) %>% arrange(-desc(regional_level))
check <- rs_filtered %>% distinct(regional_level) %>% arrange(-desc(regional_level))
filter(target, !(regional_level %in% check$regional_level))
  

# visual inspection
r <- ok_ts %>% distinct(regional_level) %>% pull()

pdf('~/Dropbox/1current/spatial_composition_change/figures/data-visualisation/resurvey-time-series-clean.pdf', width = 12, height = 9)

for(i in 1:length(r)){
  print(paste('STUDY', i, 'of', length(r), 'regions'))
  
  p = ggplot() +
    # facet_wrap(~YEAR) +
    geom_point(data = ok_ts %>% 
                 filter(regional_level == r[i]) %>% 
                 distinct(local, year, .keep_all = TRUE),
               aes(x = year, y = local, colour = max_rect)) +
    labs(subtitle = paste0('regional_level = ', r[i])) +
    theme(legend.position = 'none')
  
  
  print(p)
}
dev.off()

# check duration
resurvey_timeSeries <- ok_ts %>% 
  group_by(regional_level) %>% 
  mutate(duration = max(year) - min(year) + 1) %>% 
  ungroup() %>% 
  filter(duration > 9)

rm(ok_ts)

# check that we have the same number of sites each year (within studies)
resurvey_timeSeries %>% 
  group_by(regional_level, year) %>% 
  summarise(effort = n_distinct(local)) %>% 
  filter(length(unique(effort))!=1)

# save data (ready for analysis)
save(resurvey_timeSeries,
     file = '~/Dropbox/1current/spatial_composition_change/data/resurvey-time-series-clean.Rdata')
