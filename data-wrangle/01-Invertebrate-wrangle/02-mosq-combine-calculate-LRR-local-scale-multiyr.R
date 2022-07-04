## combine mosquito data and calculate LRR for analyses
library(tidyverse)
mosquito_alpha <- NULL
mosquito_gamma <- NULL
mosquito_jknife <- NULL

load('~/Dropbox/1current/spatial_composition_change/data/Chicago_clean.Rdata')
mosquito_alpha <- bind_rows(mosquito_alpha, alpha_S)
mosquito_gamma <- bind_rows(mosquito_gamma, gamma_S)
mosquito_jknife <- bind_rows(mosquito_jknife, study_jknife)
rm(alpha_S, gamma_S, study_jknife)

load('~/Dropbox/1current/spatial_composition_change/data/florida_clean.Rdata')
mosquito_alpha <- bind_rows(mosquito_alpha, alpha_S)
mosquito_gamma <- bind_rows(mosquito_gamma, gamma_S)
mosquito_jknife <- bind_rows(mosquito_jknife, study_jknife)
rm(alpha_S, gamma_S, study_jknife)

load('~/Dropbox/1current/spatial_composition_change/data/idaho-n_dakota-montana_clean.Rdata')
mosquito_alpha <- bind_rows(mosquito_alpha, alpha_S)
mosquito_gamma <- bind_rows(mosquito_gamma, gamma_S)
mosquito_jknife <- bind_rows(mosquito_jknife, study_jknife)
rm(alpha_S, gamma_S, study_jknife)

load('~/Dropbox/1current/spatial_composition_change/data/indiana_clean.Rdata')
mosquito_alpha <- bind_rows(mosquito_alpha, alpha_S)
mosquito_gamma <- bind_rows(mosquito_gamma, gamma_S)
mosquito_jknife <- bind_rows(mosquito_jknife, study_jknife)
rm(alpha_S, gamma_S, study_jknife)

load('~/Dropbox/1current/spatial_composition_change/data/iowa_clean.Rdata')
mosquito_alpha <- bind_rows(mosquito_alpha, alpha_S)
mosquito_gamma <- bind_rows(mosquito_gamma, gamma_S)
# fix the region name for jknife
study_jknife <- study_jknife %>% 
  mutate(region = 'iowa')
mosquito_jknife <- bind_rows(mosquito_jknife, study_jknife)
rm(alpha_S, gamma_S, study_jknife)

load('~/Dropbox/1current/spatial_composition_change/data/virginia_clean.Rdata')
mosquito_alpha <- bind_rows(mosquito_alpha, alpha_S)
mosquito_gamma <- bind_rows(mosquito_gamma, gamma_S)
mosquito_jknife <- bind_rows(mosquito_jknife, study_jknife)
rm(alpha_S, gamma_S, study_jknife)

# find the median year for each region and designate periods
mosquito_alpha <- mosquito_alpha %>% 
  group_by(region) %>% 
  mutate(med_yr = (max(year) + min(year))/2,
         period = case_when(year < med_yr ~ 'first',
                            year > med_yr ~ 'second')) %>% 
  ungroup()

# calculate target number of years either side of middle year
target_yrs <-
mosquito_alpha %>% 
  filter(!is.na(period)) %>% 
  group_by(region, plot, period) %>% 
  summarise(n_yrs = n_distinct(year)) %>% 
  # only three plots have a single year (if we remove these we get multiple years
  # for two regions where we otherwise wouldn't)
  # filter(n_yrs == 1)
  filter(!plot %in% c('-82.1227_27.6136', '-82.4171_27.6395', '-82.5045_27.4065', '-76.787_36.682')) %>% 
  group_by(region) %>% 
  summarise(target_nyrs = min(n_yrs)) %>% 
  ungroup()
  

# now get the target number of years from the start of each time series
multi_yr_dat1 <- mosquito_alpha %>% 
  filter(period=='first') %>% 
  # put target number in
  left_join(target_yrs) %>% 
  select(-med_yr) %>% 
  group_by(region, plot) %>% 
  arrange(-desc(year), .by_group = TRUE) %>% 
  filter(row_number() <= target_nyrs) %>% 
  ungroup()

# and get the same target number of years from the end of the time series
multi_yr_dat2 <- mosquito_alpha %>% 
  filter(period=='second') %>% 
  # put target number in
  left_join(target_yrs) %>% 
  select(-med_yr) %>% 
  group_by(region, plot) %>% 
  arrange(desc(year), .by_group = TRUE) %>% 
  filter(row_number() <= target_nyrs) %>% 
  ungroup()


# check: are all locations sampled in the first and last periods
site_period_check <-bind_rows(multi_yr_dat1,
                              multi_yr_dat2) %>% 
  group_by(region, plot) %>% 
  summarise(n_period = n_distinct(period)) %>% 
  ungroup()

# check each plot has two periods: they do
site_period_check %>% 
  filter(n_period!=2) %>% 
  unite(ss, c(region, plot), remove = FALSE)

# recode year to counter
# year needs to be recoded for calculating richness within years 
# (we've got the same number of years for each location, but 
# actual years themselves can differ, we need to make these consistent to 
# calculate averages over 'years')
multi_yr_dat1 <- multi_yr_dat1 %>% 
  group_by(region, plot, period) %>% 
  mutate(yr_i = 1:n()) %>% 
  ungroup()

multi_yr_dat2 <- multi_yr_dat2 %>% 
  group_by(region, plot, period) %>% 
  mutate(yr_i = 1:n()) %>% 
  ungroup()


# check number of years for study-loc_plot are equal in each period
check_yrs_local <- bind_rows(multi_yr_dat1 %>% 
                               select(region, plot, year, yr_i, period),
                             multi_yr_dat2 %>% 
                               select(region, plot, year, yr_i, period)) %>% 
  group_by(region, plot, period) %>% 
  summarise(nyrs = n_distinct(yr_i)) %>% 
  ungroup()

# some plots do not have the same number of years in each period
plots2remove <- check_yrs_local %>% 
  group_by(region, plot) %>% 
  filter(nyrs[period=='first'] != nyrs[period=='second']) %>% 
  unite(regplot, c(region, plot))

multi_yr_dat1 <- multi_yr_dat1 %>% 
  unite(regplot, c(region, plot), remove = FALSE) %>% 
  filter(!regplot %in% plots2remove$regplot) %>% 
  select(-regplot)

multi_yr_dat2 <- multi_yr_dat2 %>% 
  unite(regplot, c(region, plot), remove = FALSE) %>% 
  filter(!regplot %in% plots2remove$regplot) %>% 
  select(-regplot)

# check number of locations are equal in each period for the regional level
# does make sense to count unique years, as different locations within regions may
# be using different year, but the number of years is equal at all locations (see previous check)
check_loc_regional <- bind_rows(multi_yr_dat1 %>% 
                                  select(region, plot, period),
                                multi_yr_dat2 %>% 
                                  select(region, plot, period)) %>% 
  group_by(region, period) %>% 
  summarise(nplots = n_distinct(plot)) %>% 
  ungroup()

# good
check_loc_regional %>% 
  group_by(region) %>% 
  filter(nplots[period=='first'] != nplots[period=='second'])

# average richness over the years in each period
mosq_alphaS_multiyr <- bind_rows(multi_yr_dat1,
          multi_yr_dat2) %>% 
  group_by(region, plot, period) %>% 
  summarise(S = mean(S)) %>% 
  ungroup()

# calculate log-ratio
mosq_local_LR_multi <- left_join(mosq_alphaS_multiyr %>% 
                                   filter(period == 'first') %>% 
                                   rename(S_historical = S) %>% 
                                   select(-period),
                                 mosq_alphaS_multiyr %>% 
                                   filter(period == 'second') %>% 
                                   rename(S_modern = S) %>% 
                                   select(-period)) %>% 
  mutate(alpha_LR = log(S_modern / S_historical))

# need to get these regions and plots from the raw data to calculate regional richness
mosq_sites <- bind_rows(multi_yr_dat1 %>% 
                          select(region, plot, year, yr_i, period),
                        multi_yr_dat2 %>% 
                          select(region, plot, year, yr_i, period))

save(mosq_local_LR_multi,
     mosq_sites,
     file = '~/Dropbox/1current/spatial_composition_change/results/mosquito_alpha_LRR_multiyr.Rdata')

