##======================================================================
## identify studies with the same location sampled twice, at least 10 years apart
## want studies (i.e., regions) with at least four locations
##======================================================================

rm(list=ls())
##	
library(tidyverse)
library(vegan)
library(reshape2)

source('~/Dropbox/1current/R_random/find_max_rect.R')
##	Get the raw data 
bt <- read_csv('~/Dropbox/BioTIMELatest/BioTIMEQJune2021.csv')
btmeta <- read_csv('~/Dropbox/BioTIMELatest/bioTIMEMetadataJune2021.csv')

# only want count data for these analyses (need relative abundance)
count <- btmeta %>% 
  filter(ABUNDANCE_TYPE!='Presence/Absence')

bt <- bt %>% 
  filter(STUDY_ID %in% count$STUDY_ID)

bt_loc <- bt %>% 
  dplyr::select(STUDY_ID, YEAR, LATITUDE, LONGITUDE, PLOT) %>% 
  distinct()

# remove years with number of locations or plots < 4.
bt_loc <-  bt_loc %>% 
  group_by(STUDY_ID, YEAR) %>%
  mutate(n_loc = n_distinct(LATITUDE, LONGITUDE, PLOT)) %>%
  ungroup() %>%
  filter(n_loc >= 4) %>%
  dplyr::select(-n_loc)

# calculate number of locations, number of years and duration，
# and remove studies with number of years < 2 and duration <10 years
# NB: this is meta data only (no species records)
meta_year <- bt_loc %>%
  group_by(STUDY_ID, YEAR) %>%
  summarise(n_loc = n_distinct(LATITUDE, LONGITUDE, PLOT)) %>%
  group_by(STUDY_ID) %>%
  mutate(total_loc = sum(n_loc), 
         mean_loc = round(mean(n_loc),1), 
         min_loc = min(n_loc), 
         max_loc = max(n_loc),
         n_years = n_distinct(YEAR, na.rm = TRUE),
         duration = max(YEAR) - min(YEAR) +1) %>%
  ungroup() %>% 
  filter(n_years >=2 & duration >=10)

# meta data of search criteria
meta <- meta_year %>% 
  dplyr::select(-c(YEAR, n_loc)) %>% 
  distinct()

# get the data corresponding to studies with number of years > 2 and duration > 10 years
bt_4loc_10yr <- bt_loc %>% 
  unite(col = study_year, STUDY_ID, YEAR, remove = FALSE) %>%
  filter(study_year %in% (meta_year %>% unite(col = study_year, STUDY_ID, YEAR) %>% pull(study_year))) %>%
  dplyr::select(-study_year)

# loop de loop to optimise each study
# find pair of years ≥ 10 years apart, with maximum number of locations (plots)
bt_4loc_10yr_filtered <- NULL
# bt_years_max_loc <- NULL

for(i in 1:nrow(meta)){
  print(paste('study', i, 'of', nrow(meta)))
  # perform loop for each study
  study <- bt_4loc_10yr %>% 
    filter(STUDY_ID == meta$STUDY_ID[i])  %>% 
    unite(loc_plot, c(LONGITUDE, LATITUDE, PLOT), remove = F)
  
  # long to wide for passing to find_max_rect
  year_loc <- as.matrix(xtabs( ~ YEAR + loc_plot, data = study[,c("YEAR","loc_plot")], sparse=TRUE)) 
  
  # find max rectangle of ones
  max_rect <- find_max_rect(year_loc)
  
  rare_study <- study %>% 
    filter(loc_plot %in% max_rect[[5]]) %>% 
    mutate(end_year = as.numeric(max_rect[[4]]),
           year_count = max_rect[[3]])

  vals = sort(unique(rare_study$YEAR))
  start_year = vals[which(vals==rare_study$end_year[1]) -rare_study$year_count[1] + 1] 
  
  rare_study <- rare_study %>% 
    mutate(start_year = start_year) %>% 
    mutate(max_rect = case_when((YEAR >= start_year & YEAR <= end_year) ~ 'in',
                                TRUE ~ 'out')) #%>% 
    # ggplot() +
    # geom_point(aes(x = YEAR, y = loc_plot, colour = max_rect))
  # bt_years_max_loc <- bind_rows(bt_years_max_loc , bind_cols(STUDY_ID = meta$STUDY_ID[i], max_co_loc))
  bt_4loc_10yr_filtered  <- bind_rows(bt_4loc_10yr_filtered , rare_study)
}

# double check::
# remove years with < 4 locations and studies with number of years < 2 and duration < 10 years
# bt_4loc_10yr_filtered <- bt_4loc_10yr_filtered %>% 
#   group_by(STUDY_ID, YEAR) %>%
#   mutate(n_loc = n_distinct(LATITUDE, LONGITUDE, PLOT)) %>% 
#   filter(n_loc >= 4) %>%
#   group_by(STUDY_ID) %>%
#   mutate(n_years = n_distinct(YEAR, na.rm = TRUE),
#          duration = max(YEAR) - min(YEAR) + 1) %>%
#   filter(n_years >= 2 & duration >= 10) %>%
#   ungroup() %>%
#   dplyr::select(-c(n_loc, n_years, duration))

# get the raw data for these years and locations
bt_filtered_time_series <- bt_4loc_10yr_filtered %>% 
  inner_join(bt %>% dplyr::select(-1), 
             by = c("STUDY_ID",  "YEAR", "LATITUDE", "LONGITUDE", "PLOT"))


# visual inspection
r <- bt_filtered_time_series %>% distinct(STUDY_ID) %>% pull()

pdf('~/Desktop/bt_filtered_maxRect', width = 12, height = 9)

for(i in 1:length(r)){
  print(paste('STUDY', i, 'of', length(r), 'regions'))
  
    p = ggplot() +
      # facet_wrap(~YEAR) +
      geom_point(data = bt_filtered_time_series %>% 
                   filter(STUDY_ID == r[i]) %>% 
                   distinct(loc_plot, YEAR, .keep_all = TRUE),
                 aes(x = YEAR, y = loc_plot, colour = max_rect)) +
      labs(subtitle = paste0('STUDY_ID = ', r[i])) +
      theme(legend.position = 'none')
    
    
    print(p)
  }
dev.off()

# create list of studies for manual checking  
# write_csv(bt_filtered %>% 
#             distinct(STUDY_ID), file = '~/Desktop/bt-time-series-visual-check.csv')

# save data (not ready for analysis, manual inspection next step)
save(bt_filtered_time_series,
     file = '~/Dropbox/1current/spatial_composition_change/data/bt-location-plot-time-series.Rdata')
