# code to reduce homogenisation database to studies with equal effort,
# and calculate LR (log-ratio) of richness change at two scales - local and regional
# using multiple time points where possible
library(tidyverse)

# load the data that have had the years and sites identified
load('~/Dropbox/1current/spatial_composition_change/data/homog-site-year-selected.Rdata')
meta <- read_csv('~/Dropbox/BioTimeX/Local-Regional Homogenization/_data_extraction/metadata.csv')

# check how many years for each regional_level in resample (i.e., not checklists), 
# identify median year for those with > 3 years sampled
region_yr_median <- tibble()
for(r in 1:nrow(rs_years_max_loc)){
  print(paste('region ', r, 'in ', nrow(rs_years_max_loc)))
  # sth study
  region_r = rs_years_max_loc$regional_level[r]
  
  study_r_dat <- homog_dat %>% 
    filter(regional_level==region_r)
  
  # if there are only 3 distinct years, we can't average for initial and last observations
  n_years = length(unique(study_r_dat$year))
  
  temp = tibble(regional_level = region_r,
                n_yrs = n_years,
                med_yr = ifelse(n_years > 3, (min(study_r_dat$year) + max(study_r_dat$year))/ 2,
                                NA)) %>% 
    mutate(n_before = sum(unique(study_r_dat$year) < med_yr),
           n_after = sum(unique(study_r_dat$year) > med_yr))
  
  region_yr_median <- bind_rows(region_yr_median,
                               temp)
}

# separate studies where only first and last year can be used (i.e., those with ≤3 years sampled)
first_last_only <- region_yr_median %>% 
  filter(is.na(med_yr))

# do any checklists have more than three years? 
homog_dat %>% 
  filter(checklist==TRUE) %>% 
  group_by(regional_level) %>% 
  summarise(n_yrs = n_distinct(year)) %>% 
  filter(n_yrs > 3) # No. 

# put the checklists in with other regions where only first-last data are available
first_last_only <- bind_rows(first_last_only %>% 
                               select(regional_level, n_yrs),
                             homog_dat %>% 
                               filter(checklist==TRUE) %>% 
                               group_by(regional_level) %>% 
                               summarise(n_yrs = n_distinct(year)) %>% 
                               ungroup())

# regions (and locations) with <=3 yrs sampled cannot be averaged for the two periods 
# retain for analyses though
fl_dat <- homog_dat %>% 
  filter(regional_level %in% first_last_only$regional_level) %>% 
  # designate new period covariate
  group_by(regional_level) %>% 
  mutate(period = case_when(year == min(year) ~ 'first',
                            year == max(year) ~ 'second'),
         n_site = n_distinct(local)) %>% 
  ungroup() %>% 
  select(-fyear, -timepoints) %>% 
  filter(period %in% c('first', 'second'))

# check: are all locations sampled in the first and last periods
fl_site_period_check <-fl_dat %>% 
  group_by(dataset_id, regional, local) %>% 
  summarise(n_period = n_distinct(period)) %>% 
  ungroup()

# these sites will need to be removed (they were not sampled in each time period)
remove_rows <- fl_site_period_check %>% 
  filter(n_period!=2) %>% 
  unite(ss, c(dataset_id, regional, local), remove = FALSE)

fl_dat <- fl_dat %>% 
  unite(ss, c(dataset_id, regional, local), remove = FALSE) %>% 
  filter(!ss %in% remove_rows$ss) %>% 
  select(-ss)

# check number of locations are equal in each period for the regional level
fl_dat_nlocal_check <- fl_dat %>% 
  group_by(regional_level, period) %>% 
  summarise(n_plots = n_distinct(local)) %>% 
  ungroup()

fl_dat_nlocal_check %>% 
  group_by(regional_level) %>% 
  filter(n_plots[period=='first'] != n_plots[period=='second'])

# prepare multiyr data
multi_yr <- region_yr_median %>% 
  filter(n_yrs > 3) 

multi_yr_dat <- homog_dat %>% 
  filter(regional_level %in% multi_yr$regional_level) %>% 
  select(-period, -timepoints, -fyear, -order_month, -n_site)

# find middle year for each region, and define period
multi_yr_dat <- multi_yr_dat %>%
  group_by(dataset_id, regional) %>% 
  mutate(med_yr = (min(year) + max(year))/ 2,
         period = ifelse(year < med_yr, 'first', 'second')) %>% 
  ungroup()

# find target number of years for each period in each region
target_num_years <- multi_yr_dat %>% 
  # first number of samples in each period for each location
  group_by(dataset_id, regional, local, period) %>% 
  summarise(nyrs = n_distinct(year)) %>% 
  # now find minimum for each region
  group_by(dataset_id, regional) %>% 
  mutate(target_nyrs = min(nyrs)) %>% 
  ungroup()

# now get the target number of years from the start of each time series
multi_yr_dat1 <- multi_yr_dat %>% 
  filter(period=='first') %>% 
  # put target number in
  left_join(target_num_years %>% 
              filter(period == 'first') %>% 
              select(dataset_id, period, regional, local, target_nyrs)) %>% 
  select(-med_yr, -checklist) %>% 
  group_by(dataset_id, regional, local, year) %>% 
  nest(data = c(species, metric, value, unit)) %>% 
  group_by(dataset_id, regional, local) %>% 
  arrange(-desc(year), .by_group = TRUE) %>% 
  filter(row_number() <= target_nyrs) %>% 
  ungroup()

# and get the same target number of years from the end of the time series
multi_yr_dat2 <- multi_yr_dat %>% 
  filter(period=='second') %>% 
  # put target number in
  left_join(target_num_years %>% 
              filter(period == 'second') %>% 
              select(dataset_id, period, regional, local, target_nyrs)) %>% 
  select(-med_yr, -checklist) %>% 
  group_by(dataset_id, regional, local, year) %>% 
  nest(data = c(species, metric, value, unit)) %>% 
  group_by(dataset_id, regional, local) %>% 
  arrange(desc(year), .by_group = TRUE) %>% 
  filter(row_number() <= target_nyrs) %>% 
  ungroup()

# tidy up 
rm(multi_yr_dat)

# check: are all locations sampled in the first and last periods
site_period_check <-bind_rows(multi_yr_dat1,
                              multi_yr_dat2) %>% 
  group_by(dataset_id, regional, local) %>% 
  summarise(n_period = n_distinct(period)) %>% 
  ungroup()

# these sites will need to be removed (they were not sampled in each time period)
remove_rows <- site_period_check %>% 
  filter(n_period!=2) %>% 
  unite(ss, c(dataset_id, regional, local), remove = FALSE)

# remove sites, and recode year to counter for year
# year needs to be recoded for calculating richness within years 
# (we've got the same number of years for each location, but 
# actual years themselves can differ, we need to make these consistent to 
# calculate averages over 'years')
multi_yr_dat1 <- multi_yr_dat1 %>% 
  unite(ss, c(dataset_id, regional, local), remove = FALSE) %>% 
  filter(!ss %in% remove_rows$ss) %>% 
  select(-ss) %>% 
  group_by(dataset_id, regional, local, period) %>% 
  mutate(yr_i = 1:n()) %>% 
  ungroup()

multi_yr_dat2 <- multi_yr_dat2 %>% 
  unite(ss, c(dataset_id, regional, local), remove = FALSE) %>% 
  filter(!ss %in% remove_rows$ss) %>% 
  select(-ss) %>% 
  group_by(dataset_id, regional, local, period) %>% 
  mutate(yr_i = 1:n()) %>% 
  ungroup()


# check number of years for study-loc_plot are equal in each period
check_yrs_local <- bind_rows(multi_yr_dat1 %>% 
                               select(dataset_id, regional, local, year, yr_i, period),
                             multi_yr_dat2 %>% 
                               select(dataset_id, regional, local, year, yr_i, period)) %>% 
  group_by(dataset_id, regional, local, period) %>% 
  summarise(nyrs = n_distinct(yr_i)) %>% 
  ungroup()

check_yrs_local %>% 
  group_by(dataset_id, regional, local) %>% 
  filter(nyrs[period=='first'] != nyrs[period=='second'])

# check number of locations are equal in each period for the regional level
# does make sense to count unique years, as different locations within regions may
# be using different year, but the number of years is equal at all locations (see previous check)
check_loc_regional <- bind_rows(multi_yr_dat1 %>% 
                                  select(dataset_id, regional, local, period),
                                multi_yr_dat2 %>% 
                                  select(dataset_id, regional, local, period)) %>% 
  group_by(dataset_id, regional, period) %>% 
  summarise(nplots = n_distinct(local)) %>% 
  ungroup()

check_loc_regional %>% 
  group_by(dataset_id, regional) %>% 
  filter(nplots[period=='first'] != nplots[period=='second'])


# richness calculations for data with only two time points (same as primary analysis)
# local richness 
fl_local_S <- fl_dat %>% 
  group_by(dataset_id, regional, local, period) %>% 
  summarise(S = n_distinct(species)) %>% 
  ungroup()
# regional richness
fl_regional_S <- fl_dat %>% 
  group_by(dataset_id, regional, period) %>% 
  summarise(S = n_distinct(species)) %>% 
  ungroup() 

# regional richness jackknife
fl_regional_jacknife_prep <- fl_dat %>% 
  select(-checklist, -order_month) %>% 
  group_by(regional_level, dataset_id, regional, local, period, year, n_site) %>% 
  nest(data = c(species, metric, value, unit)) %>% 
  ungroup()

# initialise storage for results
fl_regional_jacknife <- NULL
# suppress summarise statement (so counter is visible)
options(dplyr.summarise.inform = FALSE)
# calculate jacknife resampled regional S for each study in a loop 
for(i in 1:length(unique(fl_dat$regional_level))){
  print(paste('study ', i, ' in ', length(unique(fl_regional_jacknife_prep$regional_level))))
  
  # get a study
  study_start = fl_regional_jacknife_prep %>% 
    filter(regional_level==unique(fl_dat$regional_level)[i] & period=='first')
  study_end = fl_regional_jacknife_prep %>% 
    filter(regional_level==unique(fl_dat$regional_level)[i] & period=='second')
  
  # initial temporary storage for each study
  study_jknife = NULL
  for(j in 1:unique(study_start$n_site)){
    # drop on row and calculate regional richness
    start_temp = study_start %>% 
      slice(-j) %>% 
      unnest(data) %>% 
      group_by(regional_level, period) %>% 
      summarise(S_jk = n_distinct(species)) %>% 
      ungroup() %>% 
      mutate(jacknife = j)
    
    end_temp = study_end %>% 
      slice(-j) %>% 
      unnest(data) %>% 
      group_by(regional_level, period) %>% 
      summarise(S_jk = n_distinct(species)) %>% 
      ungroup() %>% 
      mutate(jacknife = j)
    
    # join
    study_jknife = bind_rows(study_jknife, 
                             start_temp,
                             end_temp) %>% 
      mutate(n_loc_plots = unique(study_start$n_site)[1])
  }
  # join studies
  fl_regional_jacknife <- bind_rows(fl_regional_jacknife,
                                    study_jknife)
}

# richness calculations for regions with multiple years

# for the study/locations with multiple years, calculate average richness in each period
multi_yr_local_S <- bind_rows(multi_yr_dat1 %>% 
                                select(-target_nyrs), 
                              multi_yr_dat2 %>% 
                                select(-target_nyrs)) %>% 
  # first, richness in each plot in each year
  mutate(S_yr = map(data, ~n_distinct(.x$species))) %>% 
  unnest(S_yr) %>% 
  # now average over the years in each period for each plot
  group_by(dataset_id, regional, local, period) %>% 
  summarise(S = (mean(S_yr)), # round to integer
            nyr = n_distinct(year)) %>% 
  ungroup()

# (hopefully) redundant check
multi_yr_local_S %>% 
  group_by(dataset_id, regional, local) %>% 
  filter(nyr[period=='first'] != nyr[period=='second'])

multi_yr_regional_S <- bind_rows(multi_yr_dat1 %>% 
                                   select(-target_nyrs), 
                                 multi_yr_dat2 %>% 
                                   select(-target_nyrs)) %>% 
  unnest(data) %>% 
  # first get regional diversity in each year (i.e., combine plots within regions and years)
  group_by(dataset_id, regional, yr_i, period) %>%
  summarise(S_yr = n_distinct(species),
            nsite = n_distinct(local)) %>%
  group_by(dataset_id, regional, period) %>% 
  summarise(S = mean(S_yr)) %>% 
  ungroup()

#  sanity checks: is regional_S > local_S?
left_join(fl_local_S %>% 
            unnest(S) %>% 
            rename(local_S = S),
          fl_regional_S %>% 
            rename(regional_S = S)) %>% #filter(local_S > regional_S) 
  ggplot() +
  geom_point(aes(x = local_S, y = regional_S)) +
  geom_abline(intercept = 0, slope = 1, lty = 2)

left_join(multi_yr_local_S %>% 
            rename(local_S = S),
          multi_yr_regional_S %>% 
            rename(regional_S = S)) %>% #filter(local_S > regional_S) 
  ggplot() +
  geom_point(aes(x = local_S, y = regional_S)) +
  geom_abline(intercept = 0, slope = 1, lty = 2)

# jacknife for multiyr data
# multiyr regional jacknife
multiyr_jacknife_prep <- bind_rows(multi_yr_dat1 %>% 
                                     select(-target_nyrs), 
                                   multi_yr_dat2 %>% 
                                     select(-target_nyrs)) %>% 
  group_by(regional_level) %>% 
  mutate(n_loc_plots = n_distinct(local)) %>% 
  ungroup()

# initialise storage for results
multiyr_regional_jacknife_allyrs <- NULL
# suppress summarise statement (so counter is visible)
options(dplyr.summarise.inform = FALSE)
# calculate jacknife resampled regional S for each study in a loop 
for(i in 1:length(unique(multiyr_jacknife_prep$regional_level))){
  print(paste('region ', i, ' in ', length(unique(multiyr_jacknife_prep$regional_level))))
  
  # get a study
  study_start = multiyr_jacknife_prep %>% 
    filter(regional_level==unique(multiyr_jacknife_prep$regional_level)[i] & period=='first')
  study_end = multiyr_jacknife_prep %>% 
    filter(regional_level==unique(multiyr_jacknife_prep$regional_level)[i] & period=='second')
  
  # initial temporary storage for each study
  study_jknife = NULL
  for(j in 1:unique(study_start$n_loc_plots)){
    
    # need to get a jackknife resample (leave one site out) for each year
    nyr = unique(study_start$yr_i)
    
    # initialise some temporary storage 
    start_temp = NULL
    end_temp = NULL
    for(yr in 1:length(nyr)){
      
      # drop one row and calculate regional richness
      temp1 = study_start %>% 
        filter(yr_i==nyr[yr]) %>% 
        slice(-j) %>% 
        unnest(data) %>% 
        group_by(regional_level, yr_i, period) %>% 
        summarise(S_jk_yr = n_distinct(species)) %>% 
        ungroup() %>% 
        mutate(jacknife = j)
      
      start_temp = bind_rows(start_temp, 
                             temp1)
      
      temp2 = study_end %>% 
        filter(yr_i==nyr[yr]) %>% 
        slice(-j) %>% 
        unnest(data) %>% 
        group_by(regional_level, yr_i, period) %>% 
        summarise(S_jk_yr = n_distinct(species)) %>% 
        ungroup() %>% 
        mutate(jacknife = j)
      
      end_temp = bind_rows(end_temp,
                           temp2)
    }
    
    # join
    study_jknife = bind_rows(study_jknife, 
                             start_temp,
                             end_temp) %>% 
      mutate(n_loc_plots = unique(study_start$n_loc_plots)[1])
  }
  # join studies
  multiyr_regional_jacknife_allyrs <- bind_rows(multiyr_regional_jacknife_allyrs,
                                                study_jknife)
}

# need to average over years for each period
multiyr_regional_jacknife <- multiyr_regional_jacknife_allyrs %>% 
  group_by(regional_level, period, jacknife, n_loc_plots) %>% 
  summarise(S_jk = mean(S_jk_yr)) %>% 
  ungroup()



# calculate local LR
homog_local_S <- bind_rows(fl_local_S,
                     multi_yr_local_S %>% 
                       select(-nyr)) %>% 
  # count number of locations
  group_by(dataset_id, regional) %>%
  mutate(nLocations = n_distinct(local)) %>% 
  ungroup() %>% 
  # remove regions with nLocations < 4
  filter(nLocations > 3) %>% 
  select(-nLocations)

# remove regions with durations < 10 years
homog_local_S <- homog_local_S %>% 
  filter(!dataset_id %in% c('dustan-halas_1987', 'taylor_2010a', 'taylor_2010b'))


homog_local_LR <- left_join(homog_local_S %>%
                           filter(period=='first') %>% 
                           rename(S_historical = S) %>% 
                           select(-period),
                           homog_local_S %>%
                           filter(period=='second') %>% 
                           rename(S_modern = S) %>% 
                           select(-period)) %>% 
  mutate(alpha_LR = log(S_modern/S_historical))


homog_local_mean_LR <- homog_local_LR %>% 
  group_by(dataset_id, regional) %>% 
  summarise(alpha_LR_mu = mean(alpha_LR),
            alpha_LR_sd = sd(alpha_LR)) %>% 
  ungroup()

# create filter to remove the regions with fewer than 4 locations or duration < 10
regional_filter <- homog_local_mean_LR %>% 
  distinct(dataset_id, regional) %>% 
  unite(rfilter, c(dataset_id, regional))

# regional log-ratio
homog_regional_S <- bind_rows(fl_regional_S,
                        multi_yr_regional_S) %>% 
  unite(rfilter, c(dataset_id, regional), remove = F) %>% 
  filter(rfilter %in% regional_filter$rfilter) %>% 
  select(-rfilter)

homog_regional_LR <- left_join(homog_regional_S %>% 
                              filter(period=='first') %>% 
                              rename(S_historical = S) %>% 
                              select(-period),
                              homog_regional_S %>% 
                              filter(period=='second') %>% 
                              rename(S_modern = S) %>% 
                              select(-period)) %>% 
  mutate(gamma_LR = log(S_modern/S_historical))


# regional jackknife log-ratio
homog_regional_jacknife_S <- bind_rows(fl_regional_jacknife,
                              multiyr_regional_jacknife) %>% 
  filter(regional_level %in% regional_filter$rfilter) 

homog_regional_jacknife_LR <- left_join(homog_regional_jacknife_S %>% 
                                 filter(period=='first') %>% 
                                 rename(S_historical = S_jk) %>% 
                                 select(-period),
                                 homog_regional_jacknife_S %>% 
                                 filter(period=='second') %>% 
                                 rename(S_modern = S_jk) %>% 
                                 select(-period)) %>% 
  mutate(gamma_LR = log(S_modern/S_historical))


# reduce to regions in the primary analysis 
# (the broader definition of two periods resulted in extra regions being included)

homog_local_LR_multiyr = homog_local_LR
homog_regional_LR_multiyr = homog_regional_LR
homog_regional_jacknife_LR_multiyr = homog_regional_jacknife_LR

rm(homog_local_LR, 
   homog_regional_LR,
   homog_regional_jacknife_LR)

# load the data used in primary analysis
load('~/Dropbox/1current/spatial_composition_change/results/homog_LRR.Rdata')

# regional filter

local_filter <- homog_local_LR %>% 
  distinct(dataset_id, regional, local, regional_level) %>% 
  unite(lfilter, c(dataset_id, regional, local), remove = FALSE)


homog_local_LR_multiyr <- homog_local_LR_multiyr %>% 
  unite(lf, c(dataset_id, regional, local), remove = FALSE) %>% 
  filter(lf %in% local_filter$lfilter) %>% 
  left_join(local_filter) %>% 
  select(-lf, -lfilter) 
  

regionaL_filter <- homog_regional_LR %>% 
  distinct(dataset_id, regional, regional_level) %>% 
  unite(rf, c(dataset_id, regional), remove = FALSE)

homog_regional_LR_multiyr <- homog_regional_LR_multiyr %>% 
  unite(rf, c(dataset_id, regional), remove = FALSE) %>% 
  filter(rf %in% regionaL_filter$rf) %>% 
  left_join(regionaL_filter) %>% 
  select(-rf)

homog_regional_jacknife_LR_multiyr <- homog_regional_jacknife_LR_multiyr %>% 
  filter(regional_level %in% regionaL_filter$rf) %>% 
  rename(rf = regional_level) %>% 
  left_join(regionaL_filter) %>% 
  select(-rf)

# get dt (duration) and save
dt = homog_local_LR %>% 
  distinct(dataset_id, regional, regional_level, dt) %>% 
  unite(rf, c(dataset_id, regional), remove = FALSE)

homog_local_LR_multiyr <- left_join(homog_local_LR_multiyr,
                                    dt) %>% 
  select(-rf)

homog_regional_LR_multiyr <- left_join(homog_regional_LR_multiyr,
                                    dt) %>% 
  select(-rf)

homog_regional_jacknife_LR_multiyr <- left_join(homog_regional_jacknife_LR_multiyr,
                                    dt)

save(homog_local_LR_multiyr, 
     homog_regional_LR_multiyr,
     homog_regional_jacknife_LR_multiyr,
     file = '~/Dropbox/1current/spatial_composition_change/results/homog_multiyr_LRR.Rdata')


