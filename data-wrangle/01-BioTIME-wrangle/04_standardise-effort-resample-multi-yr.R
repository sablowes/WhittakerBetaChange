# script to examine sensitivity to using only two years for calcuations of richness change
# for combinations study and location where data allow, combine equal number of years to calculate
# average richness for two periods; use study (i.e., regional level) median year for middle point.
# use the same duration as the initial analysis to standardise per unit time


library(tidyverse)

load('~/Dropbox/1current/spatial_composition_change/data/bt-location-plot-approach.Rdata')

# check how many years for each study, identify median year for those with > 3 years sampled
study_yr_median <- tibble()
for(s in 1:nrow(bt_years_max_loc)){
  print(paste('study ', s, 'in ', nrow(bt_years_max_loc)))
  # sth study
  study_s = bt_years_max_loc$STUDY_ID[s]
  
  study_s_dat <- bt_filtered %>% 
    filter(STUDY_ID==study_s)
  
  # if there are only 3 distinct years, we can't average for initial and last observations
  n_years = length(unique(study_s_dat$YEAR))
  
  temp = tibble(STUDY_ID = study_s,
                n_yrs = n_years,
                med_yr = ifelse(n_years > 3, (min(study_s_dat$YEAR) + max(study_s_dat$YEAR))/ 2,
                                NA)) %>% 
    mutate(n_before = sum(unique(study_s_dat$YEAR) < med_yr),
           n_after = sum(unique(study_s_dat$YEAR) > med_yr))
  
  study_yr_median <- bind_rows(study_yr_median,
                               temp)
}

# separate studies where only first and last year can be used (i.e., those with ≤3 years sampled)
first_last_only <- study_yr_median %>% 
  filter(is.na(med_yr))

fl_filter <- bt_years_max_loc %>% 
  unite(study_yr1, c(STUDY_ID, year1), remove = FALSE) %>% 
  unite(study_yr2, c(STUDY_ID, year2), remove = FALSE) %>% 
  filter(STUDY_ID %in% first_last_only$STUDY_ID)

fl_dat <- bt_filtered %>% 
  unite(study_yr, c(STUDY_ID, YEAR), remove = FALSE) %>% 
  filter(study_yr %in% fl_filter$study_yr1 | study_yr %in% fl_filter$study_yr2) %>% 
  select(-study_yr) %>% 
  group_by(STUDY_ID, loc_plot) %>% 
  mutate(period = case_when(YEAR==min(YEAR) ~ 'first',
                           YEAR==max(YEAR) ~ 'second',
                           (YEAR!=min(YEAR) | YEAR!=max(YEAR)) ~ 'intermediate')) %>% 
  ungroup() 


# check we've got equal numbers of loc_plots at the start and end
left_join(fl_dat %>% 
            group_by(STUDY_ID, loc_plot) %>% 
            # still have > 2 years
            mutate(period = case_when(YEAR==min(YEAR) ~ 'first',
                                     YEAR==max(YEAR) ~ 'second',
                                     (YEAR!=min(YEAR) | YEAR!=max(YEAR)) ~ 'intermediate')) %>% 
            ungroup() %>% 
            filter(period=='first') %>% 
            group_by(STUDY_ID) %>% 
            summarise(n_loc_plots_start = n_distinct(loc_plot)),
          fl_dat %>% 
            group_by(STUDY_ID, loc_plot) %>% 
            mutate(period = case_when(YEAR==min(YEAR) ~ 'first',
                                     YEAR==max(YEAR) ~ 'second',
                                     (YEAR!=min(YEAR) | YEAR!=max(YEAR)) ~ 'intermediate')) %>% 
            ungroup() %>% 
            filter(period=='second') %>% 
            group_by(STUDY_ID) %>% 
            summarise(n_loc_plots_end = n_distinct(loc_plot))) %>% 
  filter(n_loc_plots_start!=n_loc_plots_end)

# get the data for locations within studies where we can average multiple years before-after median year
# chose the earliest and latest years possible when the number of samples before-after median is unequal
multi_yr_dat <- bt_filtered %>% 
  filter(!STUDY_ID %in% first_last_only$STUDY_ID) %>% 
  group_by(STUDY_ID, loc_plot, YEAR) %>% 
  nest(data = c(LATITUDE, LONGITUDE, PLOT, DAY, MONTH, SAMPLE_DESC, ID_SPECIES, 
                sum.allrawdata.ABUNDANCE, sum.allrawdata.BIOMASS, GENUS, SPECIES, GENUS_SPECIES)) %>% 
  ungroup() %>% 
  # put the median year in
  left_join(study_yr_median) %>% 
  mutate(period = ifelse(YEAR < med_yr, 'first', 'second')) %>% 
  ungroup() %>% 
  # tidy up 
  select(-n_yrs, -med_yr, -n_before, -n_after)

# need to find target_num_years for each region 
target_n_yrs <- multi_yr_dat %>% 
  # first, how many years in each period for each location (within regions)
  group_by(STUDY_ID, loc_plot, period) %>% 
  summarise(n_yr = n_distinct(YEAR)) %>% 
  # now get minimum number of years for each region
  group_by(STUDY_ID) %>% 
  mutate(target_num_yrs = min(n_yr)) %>% 
  ungroup()
  
# now get the target number of years from the start of each time series
multi_yr_dat1 <- multi_yr_dat %>% 
  filter(period=='first') %>% 
  # put target number in
  left_join(target_n_yrs %>% 
              filter(period == 'first') %>% 
              select(STUDY_ID, period, loc_plot, target_num_yrs)) %>% 
  group_by(STUDY_ID, loc_plot) %>% 
  arrange(-desc(YEAR), .by_group = TRUE) %>% 
  filter(row_number() <= target_num_yrs) %>% 
  ungroup()

multi_yr_dat2 <- multi_yr_dat %>% 
  filter(period=='second') %>% 
  # put target number in
  left_join(target_n_yrs %>% 
              filter(period == 'second') %>% 
              select(STUDY_ID,  period, loc_plot, target_num_yrs)) %>% 
  group_by(STUDY_ID, loc_plot) %>% 
  arrange(desc(YEAR), .by_group = TRUE) %>% 
  filter(row_number() <= target_num_yrs) %>% 
  ungroup()

# tidy up 
rm(multi_yr_dat)

# check: do we have the target number of years for each location and period?
bind_rows(multi_yr_dat1 %>% 
            select(STUDY_ID, YEAR, loc_plot, period, target_num_yrs),
          multi_yr_dat2 %>% 
            select(STUDY_ID, YEAR, loc_plot, period, target_num_yrs)) %>% 
  group_by(STUDY_ID, period, loc_plot) %>% 
  summarise(nyrs = n()) %>% 
  ungroup() %>% 
  left_join(target_n_yrs) %>% 
  filter(nyrs != target_num_yrs)


# check: are all locations sampled in the first and last periods
site_period_check <-bind_rows(multi_yr_dat1 %>% 
                       select(-data),
                     multi_yr_dat2 %>% 
                       select(-data)) %>% 
  group_by(STUDY_ID, loc_plot) %>% 
  summarise(n_period = n_distinct(period)) %>% 
  ungroup()

# these sites will need to be removed (they were not sampled in each time period)
remove_rows <- site_period_check %>% 
  filter(n_period!=2) %>% 
  unite(ss, c(STUDY_ID, loc_plot), remove = FALSE)

# remove sites, and recode year to counter for year
# year needs to be recoded for calculating richness within years 
# (we've got the same number of years for each location, but 
# actual years themselves can differ, we need to make these consistent to 
# calculate averages over 'years')
multi_yr_dat1 <- multi_yr_dat1 %>% 
  unite(ss, c(STUDY_ID, loc_plot), remove = FALSE) %>% 
  filter(!ss %in% remove_rows$ss) %>% 
  select(-ss) %>% 
  group_by(STUDY_ID, loc_plot, period) %>% 
  mutate(yr_i = 1:n()) %>% 
  ungroup()

multi_yr_dat2 <- multi_yr_dat2 %>% 
  unite(ss, c(STUDY_ID, loc_plot), remove = FALSE) %>% 
  filter(!ss %in% remove_rows$ss) %>% 
  select(-ss) %>% 
  group_by(STUDY_ID, loc_plot, period) %>% 
  mutate(yr_i = 1:n()) %>% 
  ungroup()

# POTENTIAL TODO: some studies only have a single year as target number
# move them to first-last only pipepline? 
singleyr_studies <- bind_rows(multi_yr_dat1 %>% 
                                select(STUDY_ID, YEAR, loc_plot, period, target_num_yrs),
                              multi_yr_dat2 %>% 
                                select(STUDY_ID, YEAR, loc_plot, period, target_num_yrs)) %>% 
  filter(target_num_yrs==1) %>% 
  distinct(STUDY_ID)

# check number of years for study-loc_plot are equal in each period
check_yrs_local <- bind_rows(multi_yr_dat1 %>% 
            select(STUDY_ID, YEAR, loc_plot, period),
          multi_yr_dat2 %>% 
            select(STUDY_ID, YEAR, loc_plot, period)) %>% 
  group_by(STUDY_ID, loc_plot, period) %>% 
  summarise(nyrs = n_distinct(YEAR)) %>% 
  ungroup()

check_yrs_local %>% 
  group_by(STUDY_ID, loc_plot) %>% 
  filter(nyrs[period=='first'] != nyrs[period=='second'])

# check number of locations are equal in each period for the regional level
# does make sense to count unique years, as different locations within regions may
# be using different year, but the number of years is equal at all locations (see previous check)
check_loc_regional <- bind_rows(multi_yr_dat1 %>% 
            select(STUDY_ID, YEAR, loc_plot, period),
          multi_yr_dat2 %>% 
            select(STUDY_ID, YEAR, loc_plot, period)) %>% 
  group_by(STUDY_ID, period) %>% 
  summarise(nplots = n_distinct(loc_plot)) %>% 
  ungroup()

check_loc_regional %>% 
  group_by(STUDY_ID) %>% 
  filter(nplots[period=='first'] != nplots[period=='second'])

# ok ready do calculations
# tidy up data with first and last years only
fl_dat <- fl_dat %>% 
  group_by(STUDY_ID, loc_plot, YEAR, period) %>% 
  nest(data = c(LATITUDE, LONGITUDE, PLOT, DAY, MONTH, SAMPLE_DESC, ID_SPECIES, 
                sum.allrawdata.ABUNDANCE, sum.allrawdata.BIOMASS, GENUS, SPECIES, GENUS_SPECIES)) %>% 
  ungroup()

# calculate local richness (same as primary analysis)
fl_local_S <- fl_dat %>% 
  mutate(S = map(data, ~n_distinct(.x$GENUS_SPECIES)))

# calculate regional richness 
fl_regional_S <- fl_dat %>% 
  unnest(data) %>% 
  group_by(STUDY_ID, period) %>% 
  summarise(S = n_distinct(GENUS_SPECIES)) %>% 
  ungroup() 

# and regional richness jackknife
# first some metadata needed in the loop code: number of plots and fYear 
fl_dat <- fl_dat %>% 
  group_by(STUDY_ID) %>% 
  mutate(n_loc_plots = n_distinct(loc_plot),
         fYear = case_when(YEAR==min(YEAR) ~ 'start',
                           YEAR==max(YEAR) ~ 'end')) %>% 
  ungroup() 

# initialise storage for results
fl_regional_jacknife <- NULL
# suppress summarise statement (so counter is visible)
options(dplyr.summarise.inform = FALSE)
# calculate jacknife resampled regional S for each study in a loop 
for(i in 1:length(unique(fl_dat$STUDY_ID))){
  print(paste('study ', i, ' in ', length(unique(fl_dat$STUDY_ID))))
  
  # get a study
  study_start = fl_dat %>% 
    filter(STUDY_ID==unique(fl_dat$STUDY_ID)[i] & fYear=='start')
  study_end = fl_dat %>% 
    filter(STUDY_ID==unique(fl_dat$STUDY_ID)[i] & fYear=='end')
  
  # initial temporary storage for each study
  study_jknife = NULL
  for(j in 1:unique(study_start$n_loc_plots)){
    # drop on row and calculate regional richness
    start_temp = study_start %>% 
      slice(-j) %>% 
      unnest(data) %>% 
      group_by(STUDY_ID, fYear) %>% 
      summarise(S_jk = n_distinct(GENUS_SPECIES)) %>% 
      ungroup() %>% 
      mutate(jacknife = j)
    
    end_temp = study_end %>% 
      slice(-j) %>% 
      unnest(data) %>% 
      group_by(STUDY_ID, fYear) %>% 
      summarise(S_jk = n_distinct(GENUS_SPECIES)) %>% 
      ungroup() %>% 
      mutate(jacknife = j)
    
    # join
    study_jknife = bind_rows(study_jknife, 
                             start_temp,
                             end_temp) %>% 
      mutate(n_loc_plots = unique(study_start$n_loc_plots)[1])
  }
  # join studies
  fl_regional_jacknife <- bind_rows(fl_regional_jacknife,
                               study_jknife)
}


# for the study/locations with multiple years, calculate average richness in each period
multi_yr_local_S <- bind_rows(multi_yr_dat1 %>% 
                                select(-target_num_yrs), 
                              multi_yr_dat2 %>% 
                                select(-target_num_yrs)) %>% 
  # first, richness in each plot in each year
  mutate(S_yr = map(data, ~n_distinct(.x$GENUS_SPECIES))) %>% 
  unnest(S_yr) %>% 
  # now average over the years in each period for each plot
  group_by(STUDY_ID, loc_plot, period) %>% 
  summarise(S = (mean(S_yr)), # round to integer
            nyr = n_distinct(YEAR)) %>% 
  ungroup()

# redundant check
multi_yr_local_S %>% 
  group_by(STUDY_ID, loc_plot) %>% 
  filter(nyr[period=='first'] != nyr[period=='second'])

multi_yr_regional_S <- bind_rows(multi_yr_dat1 %>% 
                                   select(-target_num_yrs), 
                                 multi_yr_dat2 %>% 
                                   select(-target_num_yrs)) %>% 
  unnest(data) %>% 
  # first get regional diversity in each year (i.e., combine plots within regions and years)
  group_by(STUDY_ID, yr_i, period) %>%
  summarise(S_yr = n_distinct(GENUS_SPECIES),
            nsite = n_distinct(loc_plot)) %>%
  group_by(STUDY_ID, period) %>% 
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

# multiyr regional jacknife
multiyr_jacknife_prep <- bind_rows(multi_yr_dat1 %>% 
                                     select(-target_num_yrs), 
                                   multi_yr_dat2 %>% 
                                     select(-target_num_yrs)) %>% 
  group_by(STUDY_ID) %>% 
  mutate(n_loc_plots = n_distinct(loc_plot)) %>% 
  ungroup()

# initialise storage for results
multiyr_regional_jacknife_allyrs <- NULL
# suppress summarise statement (so counter is visible)
options(dplyr.summarise.inform = FALSE)
# calculate jacknife resampled regional S for each study in a loop 
for(i in 1:length(unique(multiyr_jacknife_prep$STUDY_ID))){
  print(paste('study ', i, ' in ', length(unique(multiyr_jacknife_prep$STUDY_ID))))
  
  # get a study
  study_start = multiyr_jacknife_prep %>% 
    filter(STUDY_ID==unique(multiyr_jacknife_prep$STUDY_ID)[i] & period=='first')
  study_end = multiyr_jacknife_prep %>% 
    filter(STUDY_ID==unique(multiyr_jacknife_prep$STUDY_ID)[i] & period=='second')
  
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
        group_by(STUDY_ID, yr_i, period) %>% 
        summarise(S_jk_yr = n_distinct(GENUS_SPECIES)) %>% 
        ungroup() %>% 
        mutate(jacknife = j)
      
      start_temp = bind_rows(start_temp, 
                             temp1)
      
      temp2 = study_end %>% 
        filter(yr_i==nyr[yr]) %>% 
        slice(-j) %>% 
        unnest(data) %>% 
        group_by(STUDY_ID, yr_i, period) %>% 
        summarise(S_jk_yr = n_distinct(GENUS_SPECIES)) %>% 
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
  group_by(STUDY_ID, period, jacknife, n_loc_plots) %>% 
  summarise(S_jk = mean(S_jk_yr)) %>% 
  ungroup()

# calculate local LR
local_S <- bind_rows(fl_local_S %>% 
                       unnest(S) %>% 
                       select(-data),
                     multi_yr_local_S %>% 
                       select(-nyr))

bt_local_LR <- left_join(local_S %>%
                           filter(period=='first') %>% 
                           rename(S_historical = S) %>% 
                           select(-YEAR, -period),
                         local_S %>%
                           filter(period=='second') %>% 
                           rename(S_modern = S) %>% 
                           select(-YEAR, -period)) %>% 
  mutate(alpha_LR = log(S_modern/S_historical))


bt_local_mean_LR <- bt_local_LR %>% 
  group_by(STUDY_ID) %>% 
  summarise(alpha_LR_mu = mean(alpha_LR),
            alpha_LR_sd = sd(alpha_LR)) %>% 
  ungroup()

# regional log-ratio
regional_S <- bind_rows(fl_regional_S,
                        multi_yr_regional_S)

bt_regional_LR <- left_join(regional_S %>% 
                              filter(period=='first') %>% 
                              rename(S_historical = S) %>% 
                              select(-period),
                            regional_S %>% 
                              filter(period=='second') %>% 
                              rename(S_modern = S) %>% 
                              select(-period)) %>% 
  mutate(gamma_LR = log(S_modern/S_historical))

# regional log-ratio
regional_S_jacknife <- bind_rows(fl_regional_jacknife %>% 
                                   rename(period = fYear) %>% 
                                   mutate(period = case_when(period=='start' ~ 'first',
                                                             period=='end' ~ 'second')),
                        multiyr_regional_jacknife)

bt_regional_jacknife_LR <- left_join(regional_S_jacknife %>% 
                              filter(period=='first') %>% 
                              rename(S_historical = S_jk) %>% 
                              select(-period),
                              regional_S_jacknife %>% 
                              filter(period=='second') %>% 
                              rename(S_modern = S_jk) %>% 
                              select(-period)) %>% 
  mutate(gamma_LR = log(S_modern/S_historical))

# visual inspection
ggplot() +
  geom_point(data = left_join(bt_local_mean_LR, 
                              bt_regional_LR),
             aes(x = alpha_LR_mu, y = gamma_LR)) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2)

ggplot() +
  geom_point(data = left_join(bt_local_mean_LR, 
                              bt_regional_jacknife_LR %>% 
                                group_by(STUDY_ID) %>% 
                                summarise(gamma_LR = mean(gamma_LR))),
             aes(x = alpha_LR_mu, y = gamma_LR)) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2)

# use the same dt (duration) as initial analyses
load('~/Dropbox/1current/spatial_composition_change/results/allLRR.Rdata')
dt <- local_LRR %>% 
  filter(database=='BioTIME') %>% 
  separate(regional_level, c('bt', 'STUDY_ID')) %>% 
  mutate(STUDY_ID = as.numeric(STUDY_ID)) %>% 
  distinct(STUDY_ID, dt)

bt_local_LR <- left_join(bt_local_LR,
                         dt)

bt_regional_LR <- left_join(bt_regional_LR,
                         dt)

bt_regional_jacknife_LR <- left_join(bt_regional_jacknife_LR,
                         dt)

save(bt_local_LR, 
     bt_regional_LR,
     bt_regional_jacknife_LR,
     file = '~/Dropbox/1current/spatial_composition_change/results/bt_multiyr_LRR.Rdata')

# TODO (maybe): for time series analysis
bt_local_S_yr <- bind_rows(multi_yr_dat1 %>% 
                             select(-target_num_yrs), 
                           multi_yr_dat2 %>% 
                             select(-target_num_yrs)) %>% 
  # first, richness in each plot in each year
  mutate(S_yr = map(data, ~n_distinct(.x$GENUS_SPECIES))) %>% 
  unnest(S_yr) %>% 
  bind_rows(fl_dat %>% 
              select(-n_loc_plots, -fYear) %>% 
              mutate(S_yr = map(data, ~n_distinct(.x$GENUS_SPECIES))) %>% 
              unnest(S_yr))

left_join(multiyr_regional_jacknife_allyrs, 
          multiyr_jacknife_prep %>% 
            distinct(STUDY_ID, YEAR, period, yr_i)
          )
