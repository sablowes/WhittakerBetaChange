# code to reduce homogenisation database to studies with equal effort,
# and calculate LR (log-ratio) of richness change at two scales - local and regional
# using only two time points
library(tidyverse)

# load the data that have had the years and sites identified
load('~/Dropbox/1current/spatial_composition_change/data/homog-site-year-selected-two-time-points-clean.Rdata')
meta <- read_csv('~/Dropbox/BioTimeX/Local-Regional Homogenization/_data_extraction/metacommunity-survey_metadata-standardised.csv')

# first get label the first and last years for the select sites in the resurvey data
rs_years_max_loc <- rs_years_max_loc %>% 
  unite(region_yr1, c(regional_level, year1), remove = FALSE) %>% 
  unite(region_yr2, c(regional_level, year2), remove = FALSE) 

# filter to reduce to the years that max sites
filtered_2timeOnly <- homog_dat %>% 
  # filter(checklist==FALSE) %>% 
  unite(region_yr, c(regional_level, year), remove = FALSE) %>% 
  filter(region_yr %in% rs_years_max_loc$region_yr1 | region_yr %in% rs_years_max_loc$region_yr2) %>% 
  # put the checklist data back in
  bind_rows(homog_dat %>%
              filter(checklist==TRUE)) %>%
  group_by(dataset_id, regional_level, local) %>% 
  mutate(fyear = case_when(year==min(year) ~ 'start',
                           year==max(year) ~ 'end',
                           (year!=min(year) | year!=max(year)) ~ 'intermediate')) %>% 
  ungroup() %>% 
  select(-region_yr)

# check we've got equal numbers of loc_plots at the start and end
left_join(filtered_2timeOnly %>% 
            filter(fyear=='start') %>% 
            group_by(regional_level) %>% 
            summarise(n_loc_plots_start = n_distinct(local)),
          filtered_2timeOnly %>% 
            filter(fyear=='end') %>% 
            group_by(regional_level) %>% 
            summarise(n_loc_plots_end = n_distinct(local))) %>% 
  filter(n_loc_plots_start!=n_loc_plots_end)

# before calculating richness, need to check (and possibly standardise) sampling effort
effort_check <- left_join(filtered_2timeOnly,
                          meta %>% 
                            select(dataset_id, year, regional, local, effort))

# no start and end samples have unequal effort
left_join(effort_check %>% 
            filter(fyear=='start') %>% 
            mutate(effort1 = effort) %>% 
            distinct(dataset_id, regional, local, effort1),
          effort_check %>% 
            filter(fyear=='end') %>% 
            mutate(effort2 = effort) %>% 
            distinct(dataset_id, regional, local, effort2)) %>% 
  filter(effort1!=effort2)

# want to know number of species at each location for first and last observation in each region
alpha <- filtered_2timeOnly %>%
  filter(fyear %in% c('start', 'end')) %>% 
  group_by(dataset_id, fyear, regional, local, metric) %>% 
  dplyr::summarise(S = n_distinct(species),
            eH = ifelse(metric!='pa', exp(vegan::diversity(value, index = 'shannon')),
                        NA),
            S_PIE = ifelse(metric!='pa', vegan::diversity(value, index = 'invsimpson'),
                           NA),
            year = unique(year)
            ) %>% 
  ungroup() %>% 
  # something weird (14/09/22): grouping not reducing to one row per group
  distinct(dataset_id, regional, local, fyear, year, metric, .keep_all = TRUE)

# split into first and last (we need these in separate columns)
# omit intermediate observations for now
homog_local_LR <- left_join(
  alpha %>% 
    filter(fyear=='start') %>% 
    rename(S_historical = S,
           eH_historical = eH,
           S_PIE_historical = S_PIE,
           t1 = year) %>% 
    select(-fyear),
  alpha %>% 
    filter(fyear=='end') %>% 
    rename(S_modern = S,
           eH_modern = eH,
           S_PIE_modern = S_PIE,
           t2 = year) %>% 
    select(-fyear),
  by = c('dataset_id', 'regional', 'local', 'metric')) %>% 
  # remove locations where we don't have two samples
  filter(!is.na(S_modern) & !is.na(S_historical)) %>% 
  mutate(alpha_LR = log(S_modern / S_historical),
         alpha_LR_eH = log(eH_modern / eH_historical),
         alpha_LR_S_PIE = log(S_PIE_modern / S_PIE_historical),
         alpha_natural = S_modern - S_historical,
         dt = t2 - t1 + 1,
         ES = alpha_LR / dt,
         ES_eH = alpha_LR_eH / dt,
         ES_S_PIE = alpha_LR_S_PIE / dt) %>% 
  group_by(dataset_id, regional) %>% 
  mutate(nLocations = n_distinct(local)) %>% 
  ungroup() %>% 
  # filter to regions with at least 4 locations and dt ≥ 10
  filter(nLocations > 3 & dt >= 10)


# create filter to get the same data for regional estimates
data_filter <- homog_local_LR %>% 
  distinct(dataset_id, regional, local) %>% 
  unite(filter, c(dataset_id, regional, local)) %>% 
  distinct(filter)

# calculate regional S for the two time periods 
regional <- filtered_2timeOnly %>% 
  unite(filter, c(dataset_id, regional, local), remove = FALSE) %>% 
  filter(filter %in% data_filter$filter) %>% 
  select(-filter) %>% 
  filter(fyear %in% c('start', 'end')) %>% 
  group_by(dataset_id, fyear, regional, metric, species) %>% 
  summarise(N = sum(value),
            year = unique(year)) %>% 
  group_by(dataset_id, fyear, regional) %>% 
  summarise(S = n_distinct(species),
            eH = ifelse(metric!='pa',
                        exp(vegan::diversity(N, index = 'shannon')),
                        NA),
            S_PIE = ifelse(metric!='pa', 
                           vegan::diversity(N, index = 'invsimpson'),
                           NA),
            year = unique(year)) %>% 
  ungroup() %>% 
  distinct(dataset_id, regional, fyear, .keep_all = TRUE)

# calculate log-ratio
homog_regional_LR <- left_join(
  regional %>% 
    filter(fyear=='start') %>% 
    rename(S_historical = S,
           eH_historical = eH,
           S_PIE_historical = S_PIE,
           t1 = year) %>% 
    select(-fyear),
  regional %>% 
    filter(fyear=='end') %>% 
    rename(S_modern = S,
           eH_modern = eH,
           S_PIE_modern = S_PIE,
           t2 = year) %>% 
    select(-fyear),
  by = c('dataset_id', 'regional')
) %>% 
  mutate(regional_LR = log(S_modern / S_historical),
         regional_LR_eH = log(eH_modern / eH_historical),
         regional_LR_S_PIE = log(S_PIE_modern / S_PIE_historical),
         dt = t2 - t1 + 1,
         ES = regional_LR / dt,
         ES_eH = regional_LR_eH / dt,
         ES_S_PIE = regional_LR_S_PIE / dt)

# jacknife regional estimates
prep_regional <- filtered_2timeOnly %>% 
  unite(filter, c(dataset_id, regional, local), remove = FALSE) %>% 
  filter(filter %in% data_filter$filter) %>% 
  select(-filter) %>% 
  select(dataset_id, fyear, regional, local, metric, species, value) %>% 
  group_by(dataset_id, fyear, regional, local, metric) %>% 
  nest(data = c(species, value)) %>% 
  ungroup() %>% 
  # add number of locations
  group_by(dataset_id, fyear, regional) %>% 
  mutate(n_locations = n_distinct(local)) %>% 
  ungroup() %>% 
  # create 'regional' covariate that is combination of dataset_id and region 
  # (e.g., 2 id's (wiles_2005a, wiles_2005b) can have the same regions: )
  unite(new_region, c(dataset_id, regional), remove = F)

# suppress summarise statement (so counter is visible)
options(dplyr.summarise.inform = FALSE)

homog_regional_jknife <- NULL
# calculate jacknife resampled regional S for each study in a loop 
for(i in 1:length(unique(prep_regional$new_region))){
  print(paste('region ', i, ' in ', length(unique(prep_regional$new_region))))
  
  # get a study
  study_start = prep_regional %>% 
    filter(new_region==unique(prep_regional$new_region)[i] & fyear=='start')
  study_end = prep_regional %>% 
    filter(new_region==unique(prep_regional$new_region)[i] & fyear=='end')
  
  # initial temporary storage for each study
  region_jknife = NULL
  for(j in 1:unique(study_start$n_locations)){
    # drop on row and calculate regional richness
    start_temp = study_start %>% 
      slice(-j) %>% 
      unnest(data) %>% 
      group_by(dataset_id, regional, fyear, metric, species) %>% 
      summarise(N = sum(value)) %>% 
      ungroup() %>% 
      group_by(dataset_id, regional, fyear) %>% 
      summarise(S_jk = n_distinct(species),
                eH_jk = ifelse(metric!='pa', exp(vegan::diversity(N, index = 'shannon')), NA),
                S_PIE = ifelse(metric!='pa', vegan::diversity(N, index = 'invsimpson'), NA)) %>% 
      ungroup() %>% 
      mutate(jacknife = j) %>% 
      # summarise not working as expected (multirow output, when one expected)
      distinct(dataset_id, regional, fyear, jacknife, .keep_all = TRUE)
    
    end_temp = study_end %>% 
      slice(-j) %>% 
      unnest(data) %>% 
      group_by(dataset_id, regional, fyear, metric, species) %>% 
      summarise(N = sum(value)) %>% 
      group_by(dataset_id, regional, fyear) %>% 
      summarise(S_jk = n_distinct(species),
                eH_jk = ifelse(metric!='pa', exp(vegan::diversity(N, index = 'shannon')), NA),
                S_PIE = ifelse(metric!='pa', vegan::diversity(N, index = 'invsimpson'), NA)) %>% 
      ungroup() %>% 
      mutate(jacknife = j) %>% 
      # summarise not working as expected
      distinct(dataset_id, regional, fyear, jacknife, .keep_all = TRUE)
    
    # join
    region_jknife = bind_rows(region_jknife, 
                             start_temp,
                             end_temp) %>% 
      mutate(n_locations = unique(study_start$n_locations)[1])
  }
  # join studies
  homog_regional_jknife <- bind_rows(homog_regional_jknife,
                               region_jknife)
}

# visual check: jacknife estimate should be <= regional estimate
left_join(regional %>% 
            filter(fyear!='intermediate'),
          homog_regional_jknife %>% 
            group_by(dataset_id, regional, fyear) %>% 
            summarise(S_jk_mu = mean(S_jk))) %>% 
  ggplot() +
  geom_point(aes(x = S_jk_mu, y = S)) +
  scale_x_continuous(trans='log2') +
  scale_y_continuous(trans='log2') +
  geom_abline(intercept = 0, slope = 1, lty = 2)

# put the year back in
homog_regional_jknife <- left_join(homog_regional_jknife,
                                   regional %>% 
                                     select(dataset_id, regional, fyear, year))

# calculate log-ratio for the jacknife resamples
homog_regional_jknife_LR <- left_join(homog_regional_jknife %>% 
                                        filter(fyear=='start') %>% 
                                        rename(S_historical = S_jk,
                                               eH_historical = eH_jk,
                                               S_PIE_historical = S_PIE,
                                               t1 = year) %>% 
                                        select(-fyear),
                                      homog_regional_jknife %>% 
                                        filter(fyear=='end') %>% 
                                        rename(S_modern = S_jk,
                                               eH_modern = eH_jk,
                                               S_PIE_modern = S_PIE,
                                               t2 = year) %>% 
                                        select(-fyear)
) %>% 
  mutate(regional_LR = log(S_modern / S_historical),
         regional_LR_eH = log(eH_modern / eH_historical),
         regional_LR_S_PIE = log(S_PIE_modern / S_PIE_historical),
         dt = t2 - t1 + 1,
         ES = regional_LR / dt,
         ES_eH = regional_LR_eH / dt,
         ES_S_PIE = regional_LR_S_PIE / dt)

# need to tidy up the names in the homogenisation regional column (they blow up post-processing models),
homog_regional_level <- homog_local_LR %>% 
  distinct(dataset_id, regional) %>% 
  mutate(regional_level = as.character(1:n()))

homog_local_LR <- left_join(homog_local_LR,
                            homog_regional_level)

homog_regional_LR <- left_join(homog_regional_LR,
                               homog_regional_level)

homog_regional_jknife_LR <- left_join(homog_regional_jknife_LR,
                                      homog_regional_level)

homog_regional_jknife <- left_join(homog_regional_jknife,
                                       homog_regional_level)

save(homog_local_LR,
     homog_regional_LR, 
     homog_regional_jknife,
     homog_regional_jknife_LR,
     file = '~/Dropbox/1current/spatial_composition_change/results/homog_LRR-new.Rdata')
