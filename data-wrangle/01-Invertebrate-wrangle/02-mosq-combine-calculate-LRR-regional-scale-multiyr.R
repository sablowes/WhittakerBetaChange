# code to wrangle multi-year log-ratios for the mosquito data at the regional scale
library(tidyverse)

# load local scale estimates; these contain the region / site - year combinations we need to 
# get data for
load('~/Dropbox/1current/spatial_composition_change/results/mosquito_alpha_LRR_multiyr.Rdata')

mosq_sites %>% 
  distinct(region)

# chicago data
load('~/Dropbox/1current/spatial_composition_change/data/Chicago_clean.Rdata')

combine_local_resamps <- local_resamps %>% 
  mutate(region = 'Chicago')

rm(local_resamps)

# idaho data
load('~/Dropbox/1current/spatial_composition_change/data/idaho-n_dakota-montana_clean.Rdata')

combine_local_resamps <- bind_rows(combine_local_resamps,
                                   local_resamps %>% 
                                     mutate(region = 'idaho-n_dakota-montana')
                                   )

rm(local_resamps)

# indiana data
load('~/Dropbox/1current/spatial_composition_change/data/indiana_clean.Rdata')

combine_local_resamps <- bind_rows(combine_local_resamps,
                                   local_resamps %>% 
                                     mutate(region = 'indiana')
                                   )

rm(local_resamps)

# iowa data
load('~/Dropbox/1current/spatial_composition_change/data/iowa_clean.Rdata')

combine_local_resamps <- bind_rows(combine_local_resamps,
                                   local_resamps %>% 
                                     mutate(region = 'iowa')
                                   )

rm(local_resamps)

# Lee (Florida (United States)) and Manatee data
load('~/Dropbox/1current/spatial_composition_change/data/florida_clean.Rdata')

local_resamps %>% distinct(Locations)

combine_local_resamps <- bind_rows(combine_local_resamps,
                                   local_resamps %>% 
                                     rename(region = Locations)
)

rm(local_resamps)

# virginia data
load('~/Dropbox/1current/spatial_composition_change/data/virginia_clean.Rdata')

combine_local_resamps <- bind_rows(combine_local_resamps,
                                   local_resamps %>% 
                                     mutate(region = 'virginia')
)

rm(local_resamps)

combine_local_resamps <- combine_local_resamps %>% 
  group_by(region, plot, year) %>% 
  nest(data = c(Species, N, resample)) %>% 
  ungroup()

# filter to region-site-year combinations in the alpha-scale calculations
rsy <- mosq_sites %>% 
  unite(rsy, c(region, plot, year)) %>% 
  pull(rsy)

combine_local_resamps <- combine_local_resamps %>% 
  unite(rpy, c(region, plot, year), remove = FALSE) %>% 
  filter(rpy %in% rsy) %>% 
  select(-rpy) %>% 
  left_join(mosq_sites)

# check number of plots per year
combine_local_resamps %>% 
  group_by(region, yr_i, period) %>% 
  summarise(n_plots = n_distinct(plot)) %>% 
  group_by(region) %>% 
  summarise(n_plots = n_distinct(n_plots))

gamma_S <- combine_local_resamps %>% 
  unnest(data) %>% 
  # first get the richness for each region in each year for each resample
  group_by(region, yr_i, period, resample) %>% 
  summarise(S_resamp = n_distinct(Species)) %>% 
  ungroup() %>% 
  # now want to average richness over all the resamples
  group_by(region, yr_i, period) %>% 
  summarise(Syr = median(S_resamp)) %>% 
  # now want richness per period
  group_by(region, period) %>% 
  summarise(S = median(Syr)) %>% 
  ungroup() 

# also want regional jack knife resample
regional_jknife_allyrs <- NULL
options(dplyr.summarise.inform = FALSE)
for(r in 1:length(unique(combine_local_resamps$region))){
  # get the rth region
  region = combine_local_resamps %>% 
    filter(region == unique(combine_local_resamps$region)[r])
  
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
        group_by(region, yr_i, period, resample) %>% 
        summarise(S_resamp = n_distinct(Species)) %>% 
        ungroup() %>% 
        group_by(region, period, yr_i) %>% 
        summarise(S_jk = round(median(S_resamp))) %>% 
        ungroup() %>% 
        mutate(jacknife = n)
      
      end_temp = p2_yr_i_allplots %>%
        slice(-n) %>% 
        unnest(data) %>% 
        group_by(region, yr_i, period, resample) %>% 
        summarise(S_resamp = n_distinct(Species)) %>% 
        ungroup() %>% 
        group_by(region, period, yr_i) %>% 
        summarise(S_jk = round(median(S_resamp))) %>% 
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


# now want to average for each period
mosq_regional_jknife <- regional_jknife_allyrs %>% 
  group_by(region, period, jacknife, n_loc_plots) %>% 
  summarise(S = median(S_jk)) %>% 
  ungroup()

mosq_regional_jknife_LR_multi <- left_join(mosq_regional_jknife %>% 
                                       filter(period=='first') %>% 
                                       rename(S_historical = S) %>% 
                                       select(-period),
                                     mosq_regional_jknife %>% 
                                       filter(period=='second') %>% 
                                       rename(S_modern = S) %>% 
                                       select(-period)) %>% 
  mutate(gamma_LR = log(S_modern / S_historical))

# get duration information
load('~/Dropbox/1current/spatial_composition_change/results/mosquito_LRR.Rdata')
dt = mosquito_local_LR %>% 
  distinct(region, plot, deltaT) %>% 
  rename(dt = deltaT)

mosq_regional_jknife_LR_multi <- left_join(mosq_regional_jknife_LR_multi,
                                           dt %>% distinct(region, dt))

mosq_local_LR_multi <- left_join(mosq_local_LR_multi,
                                 dt)

save(mosq_regional_jknife_LR_multi,
     mosq_local_LR_multi,
     file = '~/Dropbox/1current/spatial_composition_change/results/mosquito_LRR_multiyr.Rdata')
