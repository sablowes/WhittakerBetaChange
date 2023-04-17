# script to reduce time series to balanced site-year combinations

# partially cleaned data
load('~/Dropbox/1current/spatial_composition_change/data/invert-location-plots-maxRect.Rdata')

# manual inspection results
man_inspection <- read_csv('~/Dropbox/1current/spatial_composition_change/WhittakerBetaChange/data-wrangle/01-Invertebrate-wrangle/balanced-site-years/invert-time-series-datasource-inspection.csv')

# studies to clean
to_clean <- man_inspection %>% 
  filter(maxRect=='see file') %>% 
  distinct(Datasource_ID)

# studies ready to go
ready <- man_inspection %>% 
  filter(maxRect=='ok') %>% 
  distinct(Datasource_ID)

# create filelist to loop through
setwd('~/Dropbox/1current/spatial_composition_change/WhittakerBetaChange/data-wrangle/01-Invertebrate-wrangle/balanced-site-years/')
filelist <- dir(pattern="invert_")

study_site_year <- tibble()

for(file in 1:length(filelist)){
  id = as.numeric(str_split(filelist[file], pattern = '_')[[1]][2])
  temp <- read_csv(filelist[file], show_col_types = FALSE) %>% 
    mutate(Datasource_ID = id)
  
  study_site_year <- bind_rows(study_site_year,
                          temp)
}

study_site_year <- study_site_year %>% 
  unite(styrsite, c(Datasource_ID, Year, loc_plot), remove = FALSE)

# reduce studies to site-year combinations manually selected
inverts <- readRDS('~/Dropbox/1current/spatial_composition_change/data/Raw data insect metacommunities.Rdata')
inverts <- inverts %>% 
  as_tibble() %>% 
  # remove the zeroes 
  filter(Number > 0) 



study_site_yr_selected <- inverts %>% 
  unite(loc_plot,  c(Plot_ID), remove = FALSE) %>%
  unite(styrsite, c(Datasource_ID, Year, loc_plot), remove = FALSE) %>% 
  filter(styrsite %in% study_site_year$styrsite) %>% 
  select(-styrsite)

rm(inverts)

# retain studies that are ok
ok_ts <- invert_filtered %>% 
  filter(Datasource_ID %in% ready$Datasource_ID) %>% 
  filter(max_rect=='in')

# put reduced study-site-year combinations back in,
# and reduce to sites in max_rect
ok_ts <- bind_rows(ok_ts,
                   study_site_yr_selected %>% 
                     mutate(max_rect = 'in'))

# check we have the right number of studies
target <- bind_rows(to_clean, ready) %>% distinct(Datasource_ID) %>% arrange(-desc(Datasource_ID))
check <- invert_filtered %>% distinct(Datasource_ID) %>% arrange(-desc(Datasource_ID))
filter(target, !(Datasource_ID %in% check$Datasource_ID))
  

# visual inspection
r <- ok_ts %>% distinct(Datasource_ID) %>% pull()

pdf('~/Desktop/invert_clean.pdf', width = 12, height = 9)

for(i in 1:length(r)){
  print(paste('STUDY', i, 'of', length(r), 'regions'))
  
  p = ggplot() +
    # facet_wrap(~YEAR) +
    geom_point(data = ok_ts %>% 
                 filter(Datasource_ID == r[i]) %>% 
                 distinct(loc_plot, Year, .keep_all = TRUE),
               aes(x = Year, y = loc_plot, colour = max_rect)) +
    labs(subtitle = paste0('Datasource_ID = ', r[i])) +
    theme(legend.position = 'none')
  
  
  print(p)
}
dev.off()

# check duration
invert_timeSeries <- ok_ts %>% 
  group_by(Datasource_ID) %>% 
  mutate(duration = max(Year) - min(Year) + 1) %>% 
  ungroup() %>% 
  filter(duration > 9)

rm(ok_ts)

# check that we have the same number of sites each year (within studies)
invert_timeSeries %>% 
  group_by(Datasource_ID, Year) %>% 
  summarise(effort = n_distinct(loc_plot)) %>% 
  filter(length(unique(effort))!=1)

# save data (ready for analysis)
save(invert_timeSeries,
     file = '~/Dropbox/1current/spatial_composition_change/data/invert-location-plot-time-series-clean.Rdata')
