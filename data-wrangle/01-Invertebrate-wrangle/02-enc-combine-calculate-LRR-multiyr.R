## combine ENC data and calculate LRR for analyses (multi-yr average for sensitivity analyses)
library(tidyverse)
enc_alpha <- NULL
enc_allresamps <- NULL

load('~/Dropbox/1current/spatial_composition_change/data/enc_butterflies-multiyr.Rdata')
enc_alpha <- bind_rows(enc_alpha, localS) %>% 
  mutate(region = 'enc_butterflies')
enc_allresamps <- enc_butterflies_resamps
# tidy up  
rm(localS, regionalS, regionalS_allresamps, enc_butterflies_resamps)

load('~/Dropbox/1current/spatial_composition_change/data/enc_moths-multiyr.Rdata')

enc_alpha <- bind_rows(enc_alpha, 
                       localS %>% 
                         mutate(region = 'enc_moths')) 
enc_allresamps <- bind_rows(enc_allresamps,
                            enc_moths_resamps)

rm(localS, regionalS, regionalS_allresamps, enc_moths_resamps)

load('~/Dropbox/1current/spatial_composition_change/data/enc_beetles-multiyr.Rdata')
enc_alpha <- bind_rows(enc_alpha, 
                       localS %>% 
                         mutate(region = 'enc_beetles'))
enc_allresamps <- bind_rows(enc_allresamps,
                            enc_beetle_resamps)

rm(localS, regionalS, regionalS_allresamps, enc_beetle_resamps)

# need to find the median year for each region
med_yr <- enc_alpha %>% 
  group_by(region) %>% 
  summarise(n_yrs = n_distinct(year),
            med_yr = ifelse(n_yrs > 3, (max(year) + min(year)) / 2,
                            NA)) %>% 
  ungroup()

# put median year into local scale data, and define each year as being from one of two periods
enc_alpha <- enc_alpha %>% 
  left_join(med_yr %>% 
              select(-n_yrs)) %>% 
  mutate(period = case_when(year < med_yr ~ 'first',
                            year > med_yr ~ 'second'))

# need to find target_num_years for each region-period (equal effort needed for each period)
target_yrs <- enc_alpha %>% 
  group_by(region, site_loc, period) %>% 
  summarise(n_yrs = n_distinct(year)) %>% 
  # want min for each region
  group_by(region) %>% 
  mutate(target_n_yrs = min(n_yrs)) %>% 
  ungroup()

# now get the target number of years from the start of each time series
multi_yr_dat1 <- enc_alpha %>% 
  filter(period=='first') %>% 
  # put target number in
  left_join(target_yrs %>% 
              filter(period == 'first') %>% 
              select(region, period, site_loc, target_n_yrs)) %>% 
  group_by(region, site_loc) %>% 
  arrange(-desc(year), .by_group = TRUE) %>% 
  filter(row_number() <= target_n_yrs) %>% 
  ungroup()

multi_yr_dat2 <- enc_alpha %>% 
  filter(period=='second') %>% 
  # put target number in
  left_join(target_yrs %>% 
              filter(period == 'second') %>% 
              select(region, period, site_loc, target_n_yrs)) %>% 
  group_by(region, site_loc) %>% 
  arrange(desc(year), .by_group = TRUE) %>% 
  filter(row_number() <= target_n_yrs) %>% 
  ungroup()

# check: do we have the target number of years for each location and period?
bind_rows(multi_yr_dat1 %>% 
            select(region, site_loc, year, period, target_n_yrs),
          multi_yr_dat2 %>% 
            select(region, site_loc, year, period, target_n_yrs)) %>% 
  group_by(region, period, site_loc) %>% 
  summarise(nyrs = n()) %>% 
  ungroup() %>% 
  left_join(target_yrs) %>% 
  filter(nyrs != target_n_yrs)


# check: are all locations sampled in the first and last periods
bind_rows(multi_yr_dat1,
          multi_yr_dat2) %>% 
  group_by(region, site_loc) %>% 
  summarise(n_period = n_distinct(period)) %>% 
  ungroup() %>% 
  filter(n_period!=2)

# check are they the same years for each region in each period?
yr_check <- bind_rows(multi_yr_dat1,
          multi_yr_dat2) %>% 
  group_by(region, site_loc, period) %>% 
  summarise(n_uniq_year = n_distinct(year)) %>% 
  ungroup() 

yr_check %>% 
  group_by(region, site_loc) %>% 
  filter(n_uniq_year[period=='first'] != n_uniq_year[period=='second'])

enc_alpha_multiyr <- bind_rows(multi_yr_dat1,
                               multi_yr_dat2) %>% 
  group_by(region, site_loc, period) %>% 
  summarise(S = mean(Sbar)) %>% 
  ungroup()

# calculate local LR
enc_local_LR_multiyr <- left_join(enc_alpha_multiyr %>%
                            filter(period=='first') %>% 
                            rename(S_historical = S) %>% 
                            select(-period),
                          enc_alpha_multiyr %>%
                            filter(period=='second') %>% 
                            rename(S_modern = S) %>% 
                            select(-period)) %>% 
  mutate(alpha_LR = log(S_modern/S_historical))

# duration is the same as initial analyses
load('~/Dropbox/1current/spatial_composition_change/results/allLRR_meta.Rdata')
local_LRR %>% 
  filter(regional_level %in% c('i_enc_butterflies', 'i_enc_moths', 'i_enc_beetles')) %>% 
  distinct(dt)

enc_local_LR_multiyr <- enc_local_LR_multiyr %>% 
  mutate(dt = 14,
         ES = alpha_LR / dt)

# need to combine resamples from these sites and years for regional scale
rsy <- bind_rows(multi_yr_dat1,
          multi_yr_dat2) %>% 
  distinct(region, site_loc, year) %>% 
  unite(rsy, c(region, site_loc, year)) %>% 
  pull()


enc_allresamps <- enc_allresamps %>% 
  unite(rsy, c(region, site_loc, year), remove = FALSE) %>% 
  filter(rsy %in% rsy) %>% 
  select(-rsy) 

# calculate regional richness for each resample in each year
enc_regional_jknife <- enc_allresamps %>% 
  # create regional SAD for each year and each resample
  group_by(region, year, resamp, FIELDNAME) %>% 
  summarise(N = sum(N)) %>% 
  # calculate regional richness for each resample; retain the resamples to use in conjunction with jack-knife resamps
  group_by(region, year, resamp) %>% 
  summarise(S = n_distinct(FIELDNAME)) %>% 
  ungroup()

# put med-yr in, define periods 
enc_regional_jknife <- enc_regional_jknife %>% 
  # need to define periods and target_num year
  left_join(med_yr %>% 
              select(region, med_yr),
            by = 'region') %>% 
  mutate(period = case_when(year < med_yr ~ 'first',
                            year > med_yr ~ 'second'))
  
# separate periods and get target-number-years for each period (to match local samples)
enc_regional_jknife_p1 <- enc_regional_jknife %>% 
  filter(period=='first') %>% 
  # put target number in
  left_join(target_yrs %>% 
              filter(period == 'first') %>% 
              distinct(region, period, target_n_yrs)) %>% 
  group_by(region, resamp) %>% 
  arrange(-desc(year), .by_group = TRUE) %>% 
  filter(row_number() <= target_n_yrs) %>% 
  ungroup()

enc_regional_jknife_p2 <- enc_regional_jknife %>% 
  filter(period=='second') %>% 
  # put target number in
  left_join(target_yrs %>% 
              filter(period == 'second') %>% 
              distinct(region, period, target_n_yrs)) %>% 
  group_by(region, resamp) %>% 
  arrange(desc(year), .by_group = TRUE) %>% 
  filter(row_number() <= target_n_yrs) %>% 
  ungroup()
rm(enc_regional_jknife)

enc_regional_jknife_multiyr <- bind_rows(enc_regional_jknife_p1,
                                         enc_regional_jknife_p2) %>% 
  group_by(region, resamp, period) %>% 
  summarise(S_jk = median(S)) %>% 
  ungroup()


enc_regional_jknife_LR_multiyr <- left_join(enc_regional_jknife_multiyr %>% 
                                      filter(period=='first') %>% 
                                      rename(S_historical = S_jk) %>% 
                                      select(-period),
                                      enc_regional_jknife_multiyr %>% 
                                      filter(period=='second') %>% 
                                      rename(S_modern = S_jk) %>% 
                                      select(-period)) %>% 
  mutate(gamma_LR = log(S_modern/S_historical),
         deltaT = 14,
         ES = gamma_LR / deltaT)


# reduce the number of resamples to a jacknife size (i.e., nLocations - 1) 
# so as they don't dominate model fitting
n_loc <- enc_alpha %>% 
  group_by(region) %>% 
  summarise(nLocations = n_distinct(site_loc))

enc_regional_jknife_LR_multiyr <- left_join(enc_regional_jknife_LR_multiyr,
                                    n_loc) %>% 
  group_by(region, nLocations) %>% 
  nest(data = c(S_historical, S_modern, gamma_LR, deltaT, ES)) %>% 
  sample_n(size = nLocations) %>% 
  ungroup() %>% 
  unnest(data) %>% 
  rename(jacknife = resamp)


save(enc_local_LR_multiyr,
     enc_regional_jknife_LR_multiyr,
     file = '~/Dropbox/1current/spatial_composition_change/results/enc_LRR_multiyr.Rdata')

