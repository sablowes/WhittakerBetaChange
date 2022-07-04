##======================================================================
## identify studies with the same location sampled twice, at least 10 years apart
## want studies (i.e., regions) with at least four locations
##======================================================================

rm(list=ls())
##	
library(tidyverse)
library(vegan)
library(reshape2)


##	Get the raw data 
bt <- read_csv('~/Dropbox/BioTIMELatest/BioTIMEQJune2021.csv')
btmeta <- read_csv('~/Dropbox/BioTIMELatest/bioTIMEMetadataJune2021.csv')
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
bt_years_max_loc <- NULL

for(i in 1:nrow(meta)){
  print(paste('study', i, 'of', nrow(meta)))
  # perform loop for each study
  study <- bt_4loc_10yr %>% 
    filter(STUDY_ID == meta$STUDY_ID[i])  %>% 
    unite(loc_plot, c(LONGITUDE, LATITUDE, PLOT), remove = F)
  
  # enter Dr Wubing Ma...lesson learned: sometimes 'wide' beats 'long' data for wrangling!
  # calculate number of locations / plots in each year: rows are years, columns are cells, elements are number of locations
  year_loc <- as.matrix(xtabs( ~ YEAR + loc_plot, data = study[,c("YEAR","loc_plot")], sparse=TRUE)) 

  # calculations in preparation to keep locations /plots with samples in all years 
  loc_dat <- data.frame("loc_plot" = colnames(year_loc),
                         "p_years" = colMeans(year_loc>0),
                         "mean_loc" = colMeans(year_loc)) %>% 
    mutate(p_loc = round(mean_loc/sum(mean_loc),3)) #relative density of locations
  
  # logical test: which locations have samples in all years or > 1/2 of te
  id_location <- with(loc_dat, p_years==1 |  p_loc > 0.5*1/nrow(loc_dat))
  year_location <-  year_loc[, id_location, drop=FALSE]
  
  # number of samples that co-occur in the locations between years
  co_loc <- as.matrix(designdist(year_loc, method = "J", terms= "minimum"))
  
  # find which two years (year-pair) have the maximum number of co-occurred locations and duration >=10 years
  # these two years have priority to be kept, and other years will be compared to the two years
  max_co_loc <- reshape2::melt(co_loc) %>% 
    as_tibble() %>%
    set_names("year1","year2","n_loc") %>%
    mutate(year1 = as.numeric(year1),
           year2 = as.numeric(year2),
           duration = year2 - year1 + 1) %>%
    filter(duration > 9 & n_loc > 3)
  
  if(nrow(max_co_loc) == 0) {next}
  
  # NOT USED for BioTIME
  # set  proportion of sites to get: studies with many sites get lower threshold
  # prop = meta %>% 
  #   filter(STUDY_ID == meta$STUDY_ID[i]) %>% 
  #   mutate(prop = case_when(mean_loc > 20 ~ 0.5, 
  #                           mean_loc < 20 ~ 0.9)
  #          ) %>% 
  #   pull(prop)
  
  # first, find pair of years with at 75% of the maximum # locations 
  max_co_loc <-  filter(max_co_loc, n_loc >= 0.75*max(n_loc))
  # break ties with the maximum duration
  max_co_loc <- filter(max_co_loc, duration == max(duration))
  # break remaining ties with the max number of locations
  max_co_loc <-  filter(max_co_loc, n_loc == max(n_loc))
  
  # if necessary, break tie (multiple start and end points with the same # sites / plots)
  if(nrow(max_co_loc) > 1){
    # get the latest time period
    n = nrow(max_co_loc)
    max_co_loc <- max_co_loc %>% 
      slice(n)
  }
  
  # keep the locations in both the selected years
  loc_year1 <- year_loc[rownames(year_loc) %in% unlist(max_co_loc[1,1]),]
  loc_year2 <- year_loc[rownames(year_loc) %in% unlist(max_co_loc[1,2]),]
  cell_shared <- loc_year1 > 0 & loc_year2 > 0
  year_loc <- year_loc[,cell_shared, drop=FALSE]
  
  # keep the desired locations & years
  rare_study <- study %>% 
    filter(loc_plot %in% colnames(year_loc) & YEAR %in% rownames(year_loc))

  bt_years_max_loc <- bind_rows(bt_years_max_loc , bind_cols(STUDY_ID = meta$STUDY_ID[i], max_co_loc))
  bt_4loc_10yr_filtered  <- bind_rows(bt_4loc_10yr_filtered , rare_study)
}

# double check::
# remove years with < 4 locations and studies with number of years < 2 and duration < 10 years
bt_4loc_10yr_filtered <- bt_4loc_10yr_filtered %>% 
  group_by(STUDY_ID, YEAR) %>%
  mutate(n_loc = n_distinct(LATITUDE, LONGITUDE, PLOT)) %>% 
  filter(n_loc >= 4) %>%
  group_by(STUDY_ID) %>%
  mutate(n_years = n_distinct(YEAR, na.rm = TRUE),
         duration = max(YEAR) - min(YEAR) + 1) %>%
  filter(n_years >= 2 & duration >= 10) %>%
  ungroup() %>%
  dplyr::select(-c(n_loc, n_years, duration))

# get the raw data for these years and locations
bt_filtered <- bt_4loc_10yr_filtered %>% 
  inner_join(bt %>% dplyr::select(-X1), 
             by = c("STUDY_ID",  "YEAR", "LATITUDE", "LONGITUDE", "PLOT"))

bt_years_max_loc <- bt_years_max_loc %>% 
  unite(study_yr1, c(STUDY_ID, year1), remove = FALSE) %>% 
  unite(study_yr2, c(STUDY_ID, year2), remove = FALSE) 

# filter to reduce to the years that max sites
bt_filtered_2timeOnly <- bt_filtered %>% 
  unite(study_yr, c(STUDY_ID, YEAR), remove = FALSE) %>% 
  filter(study_yr %in% bt_years_max_loc$study_yr1 | study_yr %in% bt_years_max_loc$study_yr2) %>% 
  group_by(STUDY_ID, loc_plot) %>% 
  mutate(fYear = case_when(YEAR==min(YEAR) ~ 'start',
                           YEAR==max(YEAR) ~ 'end',
                           (YEAR!=min(YEAR) | YEAR!=max(YEAR)) ~ 'intermediate')) %>% 
  ungroup() 

# check we've got equal numbers of loc_plots at the start and end
left_join(bt_filtered_2timeOnly %>% 
            group_by(STUDY_ID, loc_plot) %>% 
            # still have > 2 years
            mutate(fYear = case_when(YEAR==min(YEAR) ~ 'start',
                                     YEAR==max(YEAR) ~ 'end',
                                     (YEAR!=min(YEAR) | YEAR!=max(YEAR)) ~ 'intermediate')) %>% 
            ungroup() %>% 
            filter(fYear=='start') %>% 
            group_by(STUDY_ID) %>% 
            summarise(n_loc_plots_start = n_distinct(loc_plot)),
          bt_filtered_2timeOnly %>% 
            group_by(STUDY_ID, loc_plot) %>% 
            mutate(fYear = case_when(YEAR==min(YEAR) ~ 'start',
                                     YEAR==max(YEAR) ~ 'end',
                                     (YEAR!=min(YEAR) | YEAR!=max(YEAR)) ~ 'intermediate')) %>% 
            ungroup() %>% 
            filter(fYear=='end') %>% 
            group_by(STUDY_ID) %>% 
            summarise(n_loc_plots_end = n_distinct(loc_plot))) %>% 
  filter(n_loc_plots_start!=n_loc_plots_end)


# visual inspection
r <- bt_filtered_2timeOnly %>% distinct(STUDY_ID) %>% pull()

pdf('~/Dropbox/1current/spatial_composition_change/figures/data-visualisation/bt_sampling.pdf', width = 12, height = 9)

for(i in 1:length(unique(bt_filtered_2timeOnly$STUDY_ID))){
  print(paste('STUDY', i, 'of', length(unique(bt_filtered_2timeOnly$STUDY_ID)), 'regions'))
  
  p = ggplot() +
    geom_point(data = bt_filtered_2timeOnly %>% 
                 # filter(fyear!='intermediate') %>% 
                 distinct(STUDY_ID, loc_plot, YEAR, fYear) %>% 
                 # right_join(bt_filtered) %>%  
                 filter(STUDY_ID == r[i]) %>% 
                 distinct(loc_plot, YEAR, fYear),
               aes(x = YEAR, y = loc_plot, colour = fYear)) +
    labs(subtitle = r[i])
  
  
  print(p)
}
dev.off()


save(bt_filtered,
     bt_years_max_loc,
     file = '~/Dropbox/1current/spatial_composition_change/data/bt-location-plot-approach.Rdata')

