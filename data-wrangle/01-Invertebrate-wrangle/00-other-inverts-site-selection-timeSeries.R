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

##	Get the raw data (thesee are the data published in Van Klink et al 2020 Ecology)
inverts <- readRDS('~/Dropbox/1current/spatial_composition_change/data/Raw data insect metacommunities.Rdata')
inverts <- inverts %>% 
  as_tibble() %>% 
  # remove the zeroes 
  filter(Number > 0) 

invert_meta <- read_csv('~/Dropbox/1current/spatial_composition_change/data/Insect Metacommunities Metadata.csv')

invert_loc <- inverts %>% 
  dplyr::select(Datasource_ID, Year, Plot_ID) %>% 
  distinct()

# remove years with number of locations / plots < 4.
invert_loc <-  invert_loc %>% 
  group_by(Datasource_ID, Year) %>%
  mutate(n_loc = n_distinct(Plot_ID)) %>%
  ungroup() %>%
  filter(n_loc >= 4) %>%
  dplyr::select(-n_loc)

# calculate number of locations, number of years and duration，
# and remove studies with number of years < 2 and duration <10 years
meta_year <- invert_loc %>%
  group_by(Datasource_ID, Year) %>%
  summarise(n_loc = n_distinct(Plot_ID)) %>%
  group_by(Datasource_ID) %>%
  mutate(total_loc = sum(n_loc), 
         mean_loc = round(mean(n_loc),1), 
         min_loc = min(n_loc), 
         max_loc = max(n_loc),
         n_years = n_distinct(Year, na.rm = TRUE),
         duration = max(Year) - min(Year) +1) %>%
  ungroup() %>% 
  filter(n_years >=2 & duration >=10)

# meta data of search criteria
meta <- meta_year %>% 
  dplyr::select(-c(Year, n_loc)) %>% 
  distinct()

# remove studies with number of years < 2 and duration <10 years
invert_4loc_10yr <- invert_loc %>% 
  unite(col = study_year, Datasource_ID, Year, remove = FALSE) %>%
  filter(study_year %in% (meta_year %>% unite(col = study_year, Datasource_ID, Year) %>% pull(study_year))) %>%
  dplyr::select(-study_year)

# loop de loop to optimise each study
# find pair of years ≥ 10 years apart, with maximum number of locations (plots)
invert_4loc_10yr_filtered <- NULL

for(i in 1:nrow(meta)){
  print(paste('study', i, 'of', nrow(meta)))
  # perform loop for each study
  study <- invert_4loc_10yr %>% 
    filter(Datasource_ID == meta$Datasource_ID[i])  %>% 
    unite(loc_plot, c(Plot_ID), remove = F)
  
  # enter da Dr Wubing Ma...lesson learned: sometimes wide data are useful!
  # calculate number of locations / plots in each year: rows are years, columns are cells, elements are number of locations
  year_loc <- as.matrix(xtabs( ~ Year + loc_plot, data = study[,c("Year","loc_plot")], sparse=TRUE)) 
  
  # find max rectangle of ones
  max_rect <- find_max_rect(year_loc)
  
  rare_study <- study %>% 
    filter(loc_plot %in% max_rect[[5]]) %>% 
    mutate(end_year = as.numeric(max_rect[[4]]),
           year_count = max_rect[[3]])
  
  vals = sort(unique(rare_study$Year))
  start_year = vals[which(vals==rare_study$end_year[1]) -rare_study$year_count[1] + 1] 
  
  rare_study <- rare_study %>% 
    mutate(start_year = start_year) %>% 
    mutate(max_rect = case_when((Year >= start_year & Year <= end_year) ~ 'in',
                                TRUE ~ 'out')) #%>% 
  
  invert_4loc_10yr_filtered  <- bind_rows(invert_4loc_10yr_filtered , rare_study)
  
}


# get the raw data for these years and locations
invert_filtered <- invert_4loc_10yr_filtered %>% 
  inner_join(inverts, 
             by = c("Datasource_ID",  "Year", "Plot_ID"))

# visual inspection
r <- invert_filtered %>% distinct(Datasource_ID) %>% pull()


pdf('~/Desktop/invert_filtered_maxRect.pdf', width = 12, height = 9)

for(i in 1:length(r)){
  print(paste('STUDY', i, 'of', length(r), 'regions'))
  
  p = ggplot() +
    # facet_wrap(~YEAR) +
    geom_point(data = invert_filtered %>% 
                 filter(Datasource_ID == r[i]) %>% 
                 distinct(loc_plot, Year, .keep_all = TRUE),
               aes(x = Year, y = loc_plot, colour = max_rect)) +
    labs(subtitle = paste0('Datasource_ID = ', r[i])) +
    theme(legend.position = 'none')
  
  
  print(p)
}
dev.off()

# create list of studies for manual checking  
# write_csv(invert_filtered %>%
#             distinct(Datasource_ID), 
          # file = '~/Dropbox/1current/spatial_composition_change/WhittakerBetaChange/data-wrangle/01-Invertebrate-wrangle/balanced-site-years/invert-time-series-datasource-inspection.csv')

save(invert_filtered,
     file = '~/Dropbox/1current/spatial_composition_change/data/invert-location-plots-maxRect.Rdata')

