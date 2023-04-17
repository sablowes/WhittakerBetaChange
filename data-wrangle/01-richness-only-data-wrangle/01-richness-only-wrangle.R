# wrangle data for homogenisation analysis with only richness values

library(tidyverse)

richness_only <- read_csv('~/Dropbox/BioTimeX/Local-Regional Homogenization/_richness/communities.csv')
richness_meta <- read_csv('~/Dropbox/BioTimeX/Local-Regional Homogenization/_richness/metadata.csv')


# create fyear indicator for calculations
richness_only <- richness_only %>% 
  group_by(dataset_id, regional) %>% 
  mutate(fyear = case_when(year == min(year) ~ 'first',
                           year == max(year) ~ 'last', 
                           (year!=min(year) & year!=max(year)) ~ 'intermediate')) %>%
  ungroup()

# check 
richness_only %>% 
  group_by(dataset_id, regional, fyear) %>% 
  summarise(nyrs = n_distinct(year)) %>% 
  filter(nyrs > 1)

# calculate log-ratios
richness_only_local_LR <- left_join(
  richness_only %>% 
    filter(fyear=='first') %>% 
    rename(S_historical = local_richness,
           t1 = year) %>% 
    select(-fyear, -regional_richness),
  richness_only %>% 
    filter(fyear=='last') %>% 
    rename(S_modern = local_richness,
           t2 = year) %>% 
    select(-fyear, -regional_richness),
  by = c('dataset_id', 'regional', 'local')
) %>% 
  # remove locations where we don't have two samples
  filter(!is.na(S_modern) & !is.na(S_historical)) %>% 
  # remove duplicate rows
  distinct(dataset_id, regional, local, .keep_all = TRUE) %>% 
  mutate(alpha_LR = log(S_modern / S_historical),
         alpha_natural = S_modern - S_historical,
         dt = t2 - t1 + 1,
         ES = alpha_LR / dt) %>% 
  # filter to regions with  dt ≥ 10
  filter(dt >= 10)

richness_only_regional_LR <- left_join(
  richness_only %>% 
    filter(fyear=='first') %>% 
    rename(S_historical = regional_richness,
           t1 = year) %>% 
    select(-fyear, -local_richness, -local),
  richness_only %>% 
    filter(fyear=='last') %>% 
    rename(S_modern = regional_richness,
           t2 = year) %>% 
    select(-fyear, -local_richness, -local),
  by = c('dataset_id', 'regional')
) %>% 
  # remove locations where we don't have two samples
  filter(!is.na(S_modern) & !is.na(S_historical)) %>% 
  # remove duplicate rows
  distinct(dataset_id, regional, .keep_all = TRUE) %>% 
  mutate(gamma_LR = log(S_modern / S_historical),
         gamma_natural = S_modern - S_historical,
         dt = t2 - t1 + 1,
         ES_gamma = gamma_LR / dt) %>% 
  # filter to regions with  dt ≥ 10
  filter(dt >= 10)


# do some wrangling of regional levels now
regional_level <- richness_only_regional_LR %>% 
  distinct(dataset_id, regional) %>% 
  mutate(regional_level = as.character(1:n()))

richness_only_regional_LR <- left_join(richness_only_regional_LR,
                                       regional_level)

richness_only_local_LR <- left_join(richness_only_local_LR,
                                       regional_level)

save(richness_only_local_LR,
     richness_only_regional_LR,
     file = '~/Dropbox/1current/spatial_composition_change/results/Sonly_LRR.Rdata')
