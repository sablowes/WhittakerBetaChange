# standarise sampling effort in data previously filtered for duration and loc_plot
# NB: two time points only (first and last year)

library(tidyverse)

load('~/Dropbox/1current/spatial_composition_change/data/bt-location-plot-approach-two-time-points.Rdata')

# need to get abundance type columns so as to know what we are doing when use
# relative abundance in new metrics
count_type <- read_csv('~/Dropbox/BioTIMELatest/bioTIMEMetadataJune2021.csv') %>% 
  distinct(STUDY_ID, ABUNDANCE_TYPE, BIOMASS_TYPE) %>% 
  filter(STUDY_ID %in% unique(bt_filtered$STUDY_ID))


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

# want to know how many samples per year per loc_plot (within studies)
raw_new <- bt_filtered_2timeOnly %>% 
  filter(fYear!='intermediate') %>% 
  ungroup() %>% 
  unite(ObsEventID, c(STUDY_ID, loc_plot, YEAR, SAMPLE_DESC), remove = F) %>%
  group_by(STUDY_ID, loc_plot, YEAR) %>% 
  mutate(sample_effort = n_distinct(ObsEventID)) %>% 
  ungroup() %>% 
  # get relative abundances columns
  rename(N = sum.allrawdata.ABUNDANCE,
         B = sum.allrawdata.BIOMASS) %>% 
  # use biomass as relative abundance when data on individuals does not exist
  mutate(N = ifelse((N==0 & B > 0), yes = B, no =  as.numeric(N))) %>% 
  select(STUDY_ID, loc_plot, YEAR, sample_effort, GENUS_SPECIES, N) %>%
  mutate(N = as.numeric(N)) %>% 
  group_by(STUDY_ID, loc_plot, YEAR, sample_effort) %>% 
  nest(data = c(GENUS_SPECIES, N)) %>% 
  ungroup() %>% 
  group_by(STUDY_ID, loc_plot) %>% 
  mutate(fYear = ifelse(YEAR==min(YEAR), 'start', 'end')) %>% 
  ungroup() 

raw_new %>% 
  group_by(STUDY_ID) %>% 
  filter(length(unique(sample_effort)) > 1)
# no study has effort > 1, so in the next step we just take a subsample of 1 from each loc_plot
raw_new %>%
  group_by(STUDY_ID, loc_plot) %>% 
  filter(sample_effort[fYear=='start'] != sample_effort[fYear=='end']) 

# put the type of sample into the data
raw_new <- left_join(raw_new,
                     count_type)

local_S <- raw_new %>% 
  mutate(S = map(data, ~n_distinct(.x$GENUS_SPECIES)),
         eH = ifelse(ABUNDANCE_TYPE!='Presence/Absence',
                     map(data, ~exp(vegan::diversity(.x$N, index = 'shannon'))),
                     NA),
         S_PIE = ifelse(ABUNDANCE_TYPE!='Presence/Absence',
                        map(data, ~vegan::diversity(.x$N, index = 'invsimpson')),
                        NA))

regional_S <- raw_new %>% 
  unnest(data) %>% 
  group_by(STUDY_ID, fYear, ABUNDANCE_TYPE, GENUS_SPECIES) %>% 
  summarise(N = sum(N)) %>% 
  ungroup() %>% 
  group_by(STUDY_ID, fYear, ABUNDANCE_TYPE) %>% 
  summarise(S = n_distinct(GENUS_SPECIES),
            eH = ifelse(ABUNDANCE_TYPE!='Presence/Absence',
                        exp(vegan::diversity(N, index = 'shannon')),
                        NA),
            S_PIE = ifelse(ABUNDANCE_TYPE!='Presence/Absence',
                           vegan::diversity(N, index = 'invsimpson'),
                           NA)) %>% 
  ungroup() %>% 
  # weird multirow output again?
  distinct(STUDY_ID, fYear, .keep_all=TRUE) %>% 
  # put the YEAR back in for start and finish time points
  left_join(local_S %>% 
              distinct(STUDY_ID, fYear, YEAR))

# visual sanity check
left_join(local_S %>% 
            unnest(S) %>% 
            select(-eH, -S_PIE) %>% 
            rename(local_S = S),
          regional_S %>% 
            select(-eH, -S_PIE) %>% 
            rename(regional_S = S)) %>% #filter(local_S > regional_S)
  ggplot() +
  geom_point(aes(x = local_S, y = regional_S)) +
  geom_abline(intercept = 0, slope = 1, lty = 2)

left_join(local_S %>% 
            unnest(S_PIE) %>% 
            select(-S, -eH) %>% 
            rename(local_S_PIE = S_PIE),
          regional_S %>% 
            select(-S, -eH) %>% 
            rename(regional_S_PIE = S_PIE)) %>% #filter(local_S > regional_S)
  ggplot() +
  geom_point(aes(x = local_S_PIE, y = regional_S_PIE)) +
  geom_abline(intercept = 0, slope = 1, lty = 2)

# also want to jackknife resample for regional-scale analysis
# add the number of loc_plots to each combination of study/fYear combination
prep <- raw_new %>% 
  group_by(STUDY_ID, fYear) %>% 
  mutate(n_loc_plots = n_distinct(loc_plot)) %>% 
  ungroup()

# suppress summarise statement (so counter is visible)
options(dplyr.summarise.inform = FALSE)

regional_jknife <- NULL
# calculate jacknife resampled regional S for each study in a loop 
for(i in 1:length(unique(prep$STUDY_ID))){
  print(paste('study ', i, ' in ', length(unique(prep$STUDY_ID))))
  
  # get a study
  study_start = prep %>% 
    filter(STUDY_ID==unique(prep$STUDY_ID)[i] & fYear=='start')
  study_end = prep %>% 
    filter(STUDY_ID==unique(prep$STUDY_ID)[i] & fYear=='end')
  
  # initial temporary storage for each study
  study_jknife = NULL
  for(j in 1:unique(study_start$n_loc_plots)){
    # drop on row and calculate regional richness and S_PIE
    start_temp = study_start %>% 
      slice(-j) %>% 
      unnest(data) %>% 
      group_by(STUDY_ID, fYear, ABUNDANCE_TYPE, GENUS_SPECIES) %>% 
      summarise(N = sum(N)) %>% 
      ungroup() %>% 
      group_by(STUDY_ID, fYear, ABUNDANCE_TYPE) %>% 
      summarise(S = n_distinct(GENUS_SPECIES),
                eH = ifelse(ABUNDANCE_TYPE!='Presence/Absence',
                            exp(vegan::diversity(N, index = 'shannon')),
                            NA),
                S_PIE = ifelse(ABUNDANCE_TYPE!='Presence/Absence',
                               vegan::diversity(N, index = 'invsimpson'),
                               NA)) %>% 
      ungroup() %>% 
      # weird multirow output again?
      distinct(STUDY_ID, fYear, .keep_all=TRUE) %>% 
      ungroup() %>% 
      mutate(jacknife = j)
    
    end_temp = study_end %>% 
      slice(-j) %>% 
      unnest(data) %>% 
      group_by(STUDY_ID, fYear, GENUS_SPECIES, ABUNDANCE_TYPE) %>% 
      summarise(N = sum(N)) %>% 
      ungroup() %>% 
      group_by(STUDY_ID, fYear, ABUNDANCE_TYPE) %>% 
      summarise(S = n_distinct(GENUS_SPECIES),
                eH = ifelse(ABUNDANCE_TYPE!='Presence/Absence',
                            exp(vegan::diversity(N, index = 'shannon')),
                            NA),
                S_PIE = ifelse(ABUNDANCE_TYPE!='Presence/Absence',
                               vegan::diversity(N, index = 'invsimpson'),
                               NA)) %>% 
      ungroup() %>% 
      # weird multirow output again?
      distinct(STUDY_ID, fYear, .keep_all=TRUE) %>% 
      ungroup() %>% 
      mutate(jacknife = j)
    
    # join
    study_jknife = bind_rows(study_jknife, 
                             start_temp,
                             end_temp) %>% 
      mutate(n_loc_plots = unique(study_start$n_loc_plots)[1])
  }
  # join studies
  regional_jknife <- bind_rows(regional_jknife,
                               study_jknife)
}

# visual check: jacknife estimate should be <= regional estimate
left_join(regional_S,
          regional_jknife %>% 
            group_by(STUDY_ID, fYear) %>% 
            summarise(S_jk_mu = mean(S),
                      S_PIE_jk_mu = mean(S_PIE))) %>% #filter(S_jk_mu > S)
  ggplot() +
  geom_point(aes(x = S_jk_mu, y = S)) +
  geom_point(aes(x = S_PIE_jk_mu, y = S_PIE), colour='red') +
  geom_abline(intercept = 0, slope = 1, lty = 2)

# calculate local LR
bt_local_LR <- left_join(local_S %>%
                           unnest(c(S, eH, S_PIE)) %>% 
                           filter(fYear=='start') %>% 
                           rename(S_historical = S,
                                  eH_historical = eH,
                                  S_PIE_historical = S_PIE,
                                  t1 = YEAR) %>% 
                           select(-fYear, -sample_effort, -data),
                         local_S %>%
                           unnest(c(S, eH, S_PIE)) %>% 
                           filter(fYear=='end') %>% 
                           rename(S_modern = S,
                                  eH_modern = eH,
                                  S_PIE_modern = S_PIE,
                                  t2 = YEAR) %>% 
                           select(-fYear, -sample_effort, -data)) %>% 
  mutate(alpha_LR = log(S_modern/S_historical),
         alpha_LR_eH = log(eH_modern/eH_historical),
         alpha_LR_S_PIE = log(S_PIE_modern/S_PIE_historical),
         deltaT = t2 - t1 + 1,
         ES = alpha_LR / deltaT,
         ES_eH = alpha_LR_eH / deltaT,
         ES_S_PIE = alpha_LR_S_PIE / deltaT)


# bt_local_mean_LR <- bt_local_LR %>% 
#   group_by(STUDY_ID) %>% 
#   summarise(alpha_LR_mu = mean(alpha_LR),
#             check = mean(log(S_modern/S_historical)),
#             alpha_LR_sd = sd(alpha_LR),
#             ES_mu = mean(ES),
#             ES_S_PIE_mu = mean(ES_S_PIE),
#             ES_se = sd(ES)/(sqrt(n_distinct(loc_plot)))) %>% 
#   ungroup()

bt_regional_LR <- left_join(regional_S %>% 
                              filter(fYear=='start') %>% 
                              rename(S_historical = S,
                                     eH_historical = eH,
                                     S_PIE_historical = S_PIE,
                                     t1 = YEAR) %>% 
                              select(-fYear),
                            regional_S %>% 
                              filter(fYear=='end') %>% 
                              rename(S_modern = S,
                                     eH_modern = eH,
                                     S_PIE_modern = S_PIE,
                                     t2 = YEAR) %>% 
                              select(-fYear)) %>% 
  mutate(gamma_LR = log(S_modern/S_historical),
         gamma_LR_eH = log(eH_modern/eH_historical),
         gamma_LR_S_PIE = log(S_PIE_modern / S_PIE_historical),
         deltaT = t2 - t1 + 1,
         ES = gamma_LR / deltaT,
         ES_eH = gamma_LR_eH / deltaT,
         ES_S_PIE = gamma_LR_S_PIE / deltaT)

# calculate jacknife regional LR
# first put Year back in for effect size calculation
bt_regional_jknife <- left_join(regional_jknife, 
                             local_S %>% 
            distinct(STUDY_ID, fYear, YEAR))

bt_regional_jknife_LR <- left_join(bt_regional_jknife %>% 
                                  filter(fYear=='start') %>% 
                                    rename(S_historical = S,
                                           eH_historical = eH,
                                           S_PIE_historical = S_PIE,
                                           t1 = YEAR) %>% 
                                  select(-fYear),
                                  bt_regional_jknife %>% 
                                  filter(fYear=='end') %>% 
                                  rename(S_modern = S,
                                         eH_modern = eH,
                                         S_PIE_modern = S_PIE,
                                         t2 = YEAR) %>% 
                                  select(-fYear)) %>% 
  mutate(gamma_LR = log(S_modern/S_historical),
         gamma_LR_eH = log(eH_modern/eH_historical),
         gamma_LR_S_PIE = log(S_PIE_modern / S_PIE_historical),
         deltaT = t2 - t1 + 1,
         ES = gamma_LR / deltaT,
         ES_eH = gamma_LR_eH / deltaT,
         ES_S_PIE = gamma_LR_S_PIE / deltaT)

ggplot() +
  geom_point(data = left_join(bt_regional_jknife_LR %>% 
                                group_by(STUDY_ID) %>% 
                                summarise(mean_jk_LR = mean(ES)), 
                              bt_regional_LR),
             aes(x = mean_jk_LR, y = ES)) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2)



save(bt_local_LR, 
     # bt_local_mean_LR,
     bt_regional_LR,
     bt_regional_jknife_LR,
     bt_regional_jknife,
     file = '~/Dropbox/1current/spatial_composition_change/results/bt_LRR-new.Rdata')

