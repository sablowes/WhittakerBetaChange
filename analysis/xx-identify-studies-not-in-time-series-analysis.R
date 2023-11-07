# want to identify studies in the time series analysis versus the two time point 
# analyses
library(tidyverse)

load('~/Dropbox/1current/spatial_composition_change/results/model_fits/regional-ES-jk-norm-sigma.Rdata')

all_regions <- regional_ES_jk_norm_sigma2$data %>% 
  as_tibble() %>% 
  distinct(regional_level)

load('~/Dropbox/1current/spatial_composition_change/results/model_fits/gammaS-ts-model.Rdata')

ts_regions <- m.gammaS$data %>% 
  as_tibble() %>% 
  distinct(regional_level)

# names of regions changed between initial analysis and time series
# analyses due to the use of numbered regions (some dropped out and
# order got changed)

# these are the labels used in the time series analyses
load('~/Dropbox/1current/spatial_composition_change/data/resurvey-levels.Rdata')
# these are the regions we need to identify in the homogenisation levels
r2get <- resurvey_levels2 %>% 
  filter(rl2 %in% ts_regions$regional_level) %>% 
  distinct(regional_level, rl2)

# these are the labels used in the initial (two time point) analyses
load('~/Dropbox/1current/spatial_composition_change/data/homogenisation-levels.Rdata')

level_match <- homogenisation_levels %>% 
  distinct(dataset_id, regional, regional_level) %>% 
  unite(new_level, c(dataset_id, regional), remove = F) %>% 
  filter(new_level %in% r2get$regional_level) %>% 
  select(new_level, regional_level) %>% 
  left_join(r2get %>% 
              rename(new_level = regional_level))

ts_regions <- ts_regions %>% 
  filter(!str_detect(regional_level, 'resurvey')) %>% 
  bind_rows(level_match %>% select(regional_level))


all_regions %>% 
  filter(!regional_level %in% ts_regions$regional_level)

# these have names consistent with those used in the two time point analyses
# to allow filtering to subsets of regions in time series analysis
save(ts_regions, file = '~/Dropbox/1current/spatial_composition_change/results/ts-regions.Rdata')

