# code to select sites and years for analysis from the new 'homogenisation' database
# compiled by Alban xx and Jon Chase

# resurvey and checklist data are wrangled separately


library(tidyverse)

dat <- read_csv('~/Dropbox/BioTimeX/Local-Regional Homogenization/_data_extraction/metacommunity-survey_communities-standardised.csv')
meta <- read_csv('~/Dropbox/BioTimeX/Local-Regional Homogenization/_data_extraction/metacommunity-survey_metadata-standardised.csv')

# function to find maximum rectangle of ones in a binary matrix
source('~/Dropbox/1current/R_random/find_max_rect.R')

# create new regional_level for resurvey data
dat_rs <- dat %>% 
  unite(regional_level, c(dataset_id, regional), remove = FALSE) 

# identify unique years and locations within studies for the resurvey data 
rs_loc <- dat_rs %>% 
  distinct(dataset_id, regional, local, year) %>% 
  # count the number of sites per region per year
  group_by(dataset_id, regional, year) %>% 
  mutate(n_loc = n_distinct(local)) %>% 
  ungroup() %>% 
  # filter to years with at least 4 locations
  filter(n_loc >= 4) %>% 
  unite(regional_level, c(dataset_id, regional), remove = F)
  
# calculate number of locations, number of years and duration for each region
# remove regions with number of years < 2 and duration <10 years
# NB: this is meta data only (no species records)
meta_year <- rs_loc %>%
  group_by(regional_level, year) %>%
  summarise(n_loc = n_distinct(local)) %>%
  group_by(regional_level) %>% 
  mutate(total_loc = sum(n_loc), 
         mean_loc = round(mean(n_loc),1), 
         min_loc = min(n_loc), 
         max_loc = max(n_loc),
         n_years = n_distinct(year, na.rm = TRUE),
         duration = max(year) - min(year) +1) %>%
  ungroup() %>% 
  filter(n_years >=2 & duration >=10)

# clean meta data for all regional_levels
meta_new <- meta_year %>% 
  dplyr::select(-c(year, n_loc)) %>% 
  distinct()

# meta data for time series (> 2 annual observations)
meta_new <- meta_year %>% 
  filter(n_years > 2) %>% 
  dplyr::select(-c(year, n_loc)) %>% 
  distinct()

# get the data corresponding to studies with number of years > 2 (want only time
# series for this analysis) and duration > 10 years
rs_4loc_10yr <- rs_loc %>% 
  unite(col = rl_year, c(regional_level, year), remove = FALSE) %>%
  filter(rl_year %in% 
           (meta_year %>% 
              filter(n_years > 2) %>% 
              unite(col = rl_year, c(regional_level, year)) %>% 
              pull(rl_year))) %>%
  dplyr::select(-rl_year)

# loop de loop to optimise each region: want to maximise number of plots and duration 
rs_4loc_10yr_filtered <- NULL


for(i in 1:nrow(meta_new)){
  print(paste('region', i, 'of', nrow(meta_new)))
  # perform loop for each study
  region <- rs_4loc_10yr %>% 
    filter(regional_level == meta_new$regional_level[i])
  
  # calculate number of locations / plots in each year: 
  # rows are years, columns are sites
  year_loc <- as.matrix(xtabs( ~ year + local, data = region[,c("year","local")], sparse=TRUE)) 
  
  # find max rectangle of ones
  max_rect <- find_max_rect(year_loc)
  
  rare_study <- region %>% 
    filter(local %in% max_rect[[5]]) %>% 
    mutate(end_year = as.numeric(max_rect[[4]]),
           year_count = max_rect[[3]])
  
  vals = sort(unique(rare_study$year))
  start_year = vals[which(vals==rare_study$end_year[1]) -rare_study$year_count[1] + 1] 
  
  rare_study <- rare_study %>% 
    mutate(start_year = start_year) %>% 
    mutate(max_rect = case_when((year >= start_year & year <= end_year) ~ 'in',
                                TRUE ~ 'out')) #%>% 
  
  rs_4loc_10yr_filtered  <- bind_rows(rs_4loc_10yr_filtered , rare_study)
}

# get the raw data for these years and locations
rs_filtered <- rs_4loc_10yr_filtered %>% 
  inner_join(dat_rs, 
             by = c("dataset_id", "regional_level",  "regional", "year", "local"))

# reapply four site, 10-year check
rs_filtered <- rs_filtered %>% 
  group_by(regional_level) %>% 
  mutate(n_loc = n_distinct(local),
         duration = max(year) - min(year) +1) %>% 
  ungroup() %>% 
  # filter to years with at least 4 locations
  filter(duration >= 10 & n_loc >= 4)
         

# visual inspection
r <- rs_filtered %>% distinct(regional_level) %>% pull()

pdf('~/Desktop/resurvey-maxRect.pdf', width = 12, height = 9)

for(i in 1:length(r)){
  print(paste('STUDY', i, 'of', length(r), 'regions'))
  
  p = ggplot() +
    geom_point(data = rs_filtered %>% 
                 filter(regional_level==r[i]) %>%
                 distinct(local, year, max_rect),
               aes(x = year, y = local, colour = max_rect)) +#, colour = fyear
    labs(subtitle = r[i])
  
  
  print(p)
}
dev.off()

# create list of studies for manual checking  
write_csv(rs_filtered %>%
            distinct(regional_level),
file = '~/Dropbox/1current/spatial_composition_change/WhittakerBetaChange/data-wrangle/01-resurvey-checklist-wrangle/balanced-site-years/resurvey-time-series-datasource-inspection-new1.csv')

save(rs_filtered,
     file = '~/Dropbox/1current/spatial_composition_change/data/resurvey-location-plots-maxRect.Rdata')
