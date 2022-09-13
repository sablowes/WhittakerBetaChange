# wrangle rivfishtime for estimates of change that include multiple 
# years where possible

library(tidyverse)

load('~/Dropbox/1current/spatial_composition_change/data/rft_filtered_4s_10y_sameQ.Rdata')
ft_filtered

# calculate local richness 
local_richness <- ft_filtered %>% 
  group_by(SourceID, TimeSeriesID, Year) %>% 
  summarise(S = n_distinct(Species),
            S_PIE = vegan::diversity(Abundance, index = 'invsimpson')) %>% 
  ungroup() %>% 
  # find median year, add indicator for period
  group_by(SourceID) %>% 
  mutate(n_yrs = n_distinct(Year),
         med_yr = ifelse(n_yrs <= 3, NA, (max(Year) + min(Year)) / 2)) %>% 
  ungroup()

# regions where only two time points are available
fl_only_alpha_S <- local_richness %>% 
  filter(is.na(med_yr)) %>% 
  select(-med_yr) %>% 
  group_by(SourceID, TimeSeriesID) %>% 
  mutate(period = case_when(Year==min(Year) ~ 'first',
                            Year==max(Year) ~ 'second')) %>% 
  ungroup()

# check: are all locations sampled in the first and last periods
fl_only_alpha_S %>% 
  group_by(SourceID, TimeSeriesID) %>% 
  summarise(n_period = n_distinct(period)) %>% 
  filter(n_period != 2)
  
# check number of locations are equal in each period for the regional level
nloc_check <- fl_only_alpha_S %>% 
  group_by(SourceID, period) %>% 
  summarise(n_loc = n_distinct(TimeSeriesID))

nloc_check %>% 
  group_by(SourceID) %>% 
  filter(n_loc[period=='first'] != n_loc[period=='second'])

# now, regional scale for these data
# we already have the regional jacknife for these data
fl_only_id <- fl_only_alpha_S %>% 
  distinct(SourceID, TimeSeriesID, Year) 
  
load('~/Dropbox/1current/spatial_composition_change/results/rft_LRR.Rdata')
fl_regional_jacknife_LR <- rft_regional_jknife_LR %>% 
  filter(SourceID %in% fl_only_id$SourceID)


# prepare multiyr data: how many years for each period in each location
target_years <- local_richness %>% 
  filter(n_yrs > 3) %>% 
  group_by(SourceID) %>% 
  mutate(period = case_when(Year < med_yr ~ 'first', 
                            Year > med_yr ~ 'second')) %>% 
  # remove the NAs (year==med_yr)
  filter(!is.na(period)) %>% 
  # now count how many years in each period
  group_by(SourceID, TimeSeriesID, period) %>% 
  mutate(n_yrs = n_distinct(Year)) %>% 
  # and put the target number of years in
  group_by(SourceID) %>% 
  mutate(target_n_yrs = min(n_yrs)) %>% 
  ungroup()

multiyr_dat1 <- local_richness %>% 
  filter(n_yrs > 3) %>% 
  group_by(SourceID) %>% 
  mutate(period = case_when(Year < med_yr ~ 'first', 
                            Year > med_yr ~ 'second')) %>% 
  # get first period
  filter(period=='first') %>%
  ungroup() %>% 
  # put target number in
  left_join(target_years %>% 
              filter(period == 'first') %>% 
              select(SourceID, TimeSeriesID, Year, period, target_n_yrs)) %>% 
  select(-n_yrs, -med_yr) %>% 
  group_by(SourceID, TimeSeriesID, Year, period) %>% 
  nest(data = c(S, S_PIE)) %>% 
  group_by(SourceID, TimeSeriesID) %>% 
  arrange(-desc(Year), .by_group = TRUE) %>% 
  filter(row_number() <= target_n_yrs) %>% 
  ungroup()

multiyr_dat2 <- local_richness %>% 
  filter(n_yrs > 3) %>% 
  group_by(SourceID) %>% 
  mutate(period = case_when(Year < med_yr ~ 'first', 
                            Year > med_yr ~ 'second')) %>% 
  # get first period
  filter(period=='second') %>%
  ungroup() %>% 
  # put target number in
  left_join(target_years %>% 
              filter(period == 'second') %>% 
              select(SourceID, TimeSeriesID, Year, period, target_n_yrs)) %>% 
  select(-n_yrs, -med_yr) %>% 
  group_by(SourceID, TimeSeriesID, Year, period) %>% 
  nest(data = c(S, S_PIE)) %>% 
  group_by(SourceID, TimeSeriesID) %>% 
  arrange(desc(Year), .by_group = TRUE) %>% 
  filter(row_number() <= target_n_yrs) %>% 
  ungroup()

# check: are all locations sampled in the first and last periods
site_period_check <-bind_rows(multiyr_dat1,
                              multiyr_dat2) %>% 
  group_by(SourceID, TimeSeriesID) %>% 
  summarise(n_period = n_distinct(period)) %>% 
  ungroup()

# these sites will need to be removed (they were not sampled in each time period)
remove_rows <- site_period_check %>% 
  filter(n_period!=2) %>% 
  unite(ss, c(SourceID, TimeSeriesID), remove = FALSE)

# remove sites, and recode year to counter for year
# year needs to be recoded for calculating richness within years 
# (we've got the same number of years for each location, but 
# actual years themselves can differ, we need to make these consistent to 
# calculate averages over 'years')
multiyr_dat1 <- multiyr_dat1 %>% 
  unite(ss, c(SourceID, TimeSeriesID), remove = FALSE) %>% 
  filter(!ss %in% remove_rows$ss) %>% 
  select(-ss) %>% 
  group_by(SourceID, TimeSeriesID, period) %>% 
  mutate(yr_i = 1:n()) %>% 
  ungroup()

multiyr_dat2 <- multiyr_dat2 %>% 
  unite(ss, c(SourceID, TimeSeriesID), remove = FALSE) %>% 
  filter(!ss %in% remove_rows$ss) %>% 
  select(-ss) %>% 
  group_by(SourceID, TimeSeriesID, period) %>% 
  mutate(yr_i = 1:n()) %>% 
  ungroup()


# check number of years for study-loc_plot are equal in each period
check_yrs_local <- bind_rows(multiyr_dat1 %>% 
                               select(SourceID, TimeSeriesID, Year, yr_i, period),
                             multiyr_dat2 %>% 
                               select(SourceID, TimeSeriesID, Year, yr_i, period)) %>% 
  group_by(SourceID, TimeSeriesID, period) %>% 
  summarise(nyrs = n_distinct(yr_i)) %>% 
  ungroup()

check_yrs_local %>% 
  group_by(SourceID, TimeSeriesID) %>% 
  filter(nyrs[period=='first'] != nyrs[period=='second'])

# check number of locations are equal in each period for the regional level
# does make sense to count unique years, as different locations within regions may
# be using different year, but the number of years is equal at all locations (see previous check)
check_loc_regional <- bind_rows(multiyr_dat1 %>% 
                                  select(SourceID, TimeSeriesID, period),
                                multiyr_dat2 %>% 
                                  select(SourceID, TimeSeriesID, period)) %>% 
  group_by(SourceID, period) %>% 
  summarise(nplots = n_distinct(TimeSeriesID)) %>% 
  ungroup()

check_loc_regional %>% 
  group_by(SourceID) %>% 
  filter(nplots[period=='first'] != nplots[period=='second'])

rft_local_multiyr_S <- bind_rows(multiyr_dat1,
                         multiyr_dat2) %>% 
  unnest(data) %>% 
  group_by(SourceID, TimeSeriesID, period) %>% 
  summarise(S = mean(S),
            S_PIE = mean(S_PIE)) %>% 
  ungroup()

# want the standardised data for these regions and locations to estimate regional richness
# using jacknife resampling
reg_loc <- bind_rows(multiyr_dat1,
                     multiyr_dat2) %>% 
  unnest(data) %>% 
  distinct(SourceID, TimeSeriesID, Year, period, yr_i) %>% 
  unite(reg_loc_yr, c(SourceID, TimeSeriesID, Year), remove = F)


# regional scale jacknife
prep_regional <- ft_filtered %>% 
  unite(reg_loc_yr, c(SourceID, TimeSeriesID, Year)) %>% 
  filter(reg_loc_yr %in% reg_loc$reg_loc_yr) %>% 
  left_join(reg_loc) %>% 
  select(SourceID, TimeSeriesID, Year, yr_i, period, Species, Abundance) %>% 
  nest(data = c(Species, Abundance)) %>% 
  group_by(SourceID, Year, yr_i, period) %>% 
  mutate(n_locations = n_distinct(TimeSeriesID)) %>% 
  ungroup() %>% 
  select(SourceID, TimeSeriesID, Year, yr_i, period, data, n_locations) %>% 
  ungroup()

# also want regional jack knife resample
regional_jknife_allyrs <- NULL
# suppress summarise statement (so counter is visible)
options(dplyr.summarise.inform = FALSE)

for(r in 1:length(unique(prep_regional$SourceID))){
  print(paste('region ', r, 'in ', length(unique(prep_regional$SourceID)), 'regions'))
  # get the rth region
  region = prep_regional %>% 
    filter(SourceID == unique(prep_regional$SourceID)[r])
  
  # split into periods
  period1 = region %>% 
    filter(period=='first')
  
  period2 = region %>% 
    filter(period=='second')
  
  # check same number of years 
  # length(period1$yr_i) == length(period2$yr_i)
  # if(sum(period1$plot != period2$plot) != 0)  print('plots do not match') break
  
  # now want a jacknife resample for each year
  yrs = unique(period1$yr_i)
  n_yrs = length(yrs)
  all_yrs_jacknife <- NULL
  for(yr in 1:n_yrs){
    # get all plots for year == yr
    p1_yr_i_allplots = period1 %>% 
      filter(yr_i == yrs[yr])
    
    p2_yr_i_allplots = period2 %>% 
      filter(yr_i == yrs[yr])
    
    # calculate how many plots
    n_plots = nrow(p1_yr_i_allplots)
    
    # do jacknife (leave one out) resampling
    yr_jknife <- NULL
    for(n in 1:n_plots){
      
      # drop on row and calculate regional richness
      start_temp = p1_yr_i_allplots %>% 
        slice(-n) %>% 
        unnest(data) %>% 
        group_by(SourceID, Year, yr_i, period, Species) %>% 
        summarise(N = sum(Abundance)) %>% 
        group_by(SourceID, Year, yr_i, period) %>% 
        summarise(S_jk = n_distinct(Species),
                  S_PIE_jk = vegan::diversity(N, index = 'invsimpson')) %>% 
        ungroup() %>% 
        mutate(jacknife = n)
      
      end_temp = p2_yr_i_allplots %>%
        slice(-n) %>% 
        unnest(data) %>% 
        group_by(SourceID, Year, yr_i, period, Species) %>% 
        summarise(N = sum(Abundance)) %>% 
        group_by(SourceID, Year, yr_i, period) %>% 
        summarise(S_jk = n_distinct(Species),
                  S_PIE_jk = vegan::diversity(N, index = 'invsimpson')) %>% 
        ungroup() %>% 
        mutate(jacknife = n)
      
      # join
      yr_jknife = bind_rows(yr_jknife, 
                            start_temp,
                            end_temp) %>% 
        mutate(n_loc_plots = n_plots)
      
    }
    all_yrs_jacknife <- bind_rows(all_yrs_jacknife,
                                  yr_jknife)
  }
  regional_jknife_allyrs <- bind_rows(regional_jknife_allyrs,
                                      all_yrs_jacknife)
}

# now average richness over the years in each period (for each jackknife resample)
rft_regional_jknife <- regional_jknife_allyrs %>% 
  group_by(SourceID, period, jacknife) %>% 
  summarise(S = median(S_jk),
            S_PIE = median(S_PIE_jk)) %>% 
  ungroup()

# join data with first-last samples only to multiyr dat, 
# calculate log-ratios and save
rft_local_S_multiyr <- bind_rows(fl_only_alpha_S %>% 
                                   select(-n_yrs, -Year),
                                 rft_local_multiyr_S)


rft_local_LR_multiyr <- left_join(rft_local_S_multiyr %>% 
                                    filter(period=='first') %>% 
                                    rename(S_historical = S,
                                           S_PIE_historical = S_PIE) %>% 
                                    select(-period),
                                  rft_local_S_multiyr %>% 
                                    filter(period=='second') %>% 
                                    rename(S_modern = S,
                                           S_PIE_modern = S_PIE) %>% 
                                    select(-period)) %>% 
  mutate(alpha_LR = log(S_modern / S_historical),
         alpha_LR_S_PIE = log(S_PIE_modern / S_PIE_historical)) 

rft_regional_jknife_LR_multiyr <- left_join(rft_regional_jknife %>% 
                                              filter(period == 'first') %>% 
                                              rename(S_historical = S,
                                                     S_PIE_historical = S_PIE) %>% 
                                              select(-period),
                                            rft_regional_jknife %>% 
                                              filter(period == 'second') %>% 
                                              rename(S_modern = S,
                                                     S_PIE_modern = S_PIE) %>% 
                                              select(-period)) %>% 
  mutate(gamma_LR = log(S_modern / S_historical),
         gamma_LR_S_PIE = log(S_PIE_modern / S_PIE_historical)) %>% 
  left_join(rft_regional_LR %>% 
              distinct(SourceID, deltaT) %>% 
              rename(dt = deltaT)) %>% 
  mutate(ES_gamma = gamma_LR / dt,
         ES_gamma_S_PIE = gamma_LR_S_PIE / dt) %>% 
  bind_rows(fl_regional_jacknife_LR %>% 
              select(-t1, -t2, -n_locations) %>% 
              rename(ES_gamma = ES,
                     ES_gamma_S_PIE = ES_S_PIE))

save(rft_local_LR_multiyr,
     rft_regional_jknife_LR_multiyr,
     file = '~/Dropbox/1current/spatial_composition_change/results/rft_LRR-multiyr.Rdata')
