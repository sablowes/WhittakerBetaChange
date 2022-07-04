# standarise sampling effort in data previously filtered for duration and loc_plot
# NB: two time points only (first and last year)

library(tidyverse)

load('~/Dropbox/1current/spatial_composition_change/data/bt-location-plot-approach.Rdata')

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
  unite(ObsEventID, c(STUDY_ID, loc_plot, YEAR), remove = F) %>%
  group_by(STUDY_ID, loc_plot, YEAR) %>% 
  mutate(sample_effort = n_distinct(ObsEventID)) %>% 
  select(STUDY_ID, loc_plot, YEAR, sample_effort, GENUS_SPECIES) %>% 
  group_by(STUDY_ID, loc_plot, YEAR, sample_effort) %>% 
  nest(data = c(GENUS_SPECIES)) %>% 
  ungroup() %>% 
  group_by(STUDY_ID, loc_plot) %>% 
  mutate(fYear = ifelse(YEAR==min(YEAR), 'start', 'end')) %>% 
  ungroup() 

# no study has effort > 1, so in the next step we just take a subsample of 1 from each loc_plot
raw_new %>%
  group_by(STUDY_ID, loc_plot) %>% 
  filter(sample_effort[fYear=='start'] != sample_effort[fYear=='end']) 

local_S <- raw_new %>% 
  mutate(S = map(data, ~n_distinct(.x$GENUS_SPECIES)))

regional_S <- raw_new %>% 
  unnest(data) %>% 
  group_by(STUDY_ID, fYear) %>% 
  summarise(S = n_distinct(GENUS_SPECIES)) %>% 
  ungroup() %>% 
  # put the YEAR back in for start and finish time points
  left_join(local_S %>% 
              distinct(STUDY_ID, fYear, YEAR))

# visual sanity check
left_join(local_S %>% 
            unnest(S) %>% 
            rename(local_S = S),
          regional_S %>% 
            rename(regional_S = S)) %>% #filter(local_S > regional_S)
  ggplot() +
  geom_point(aes(x = local_S, y = regional_S)) +
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
  regional_jknife <- bind_rows(regional_jknife,
                               study_jknife)
}

# visual check: jacknife estimate should be <= regional estimate
left_join(regional_S,
          regional_jknife %>% 
            group_by(STUDY_ID, fYear) %>% 
            summarise(S_jk_mu = mean(S_jk))) %>% #filter(S_jk_mu > S)
  ggplot() +
  geom_point(aes(x = S_jk_mu, y = S)) +
  geom_abline(intercept = 0, slope = 1, lty = 2)

# calculate local LR
bt_local_LR <- left_join(local_S %>%
                           unnest(S) %>% 
                           filter(fYear=='start') %>% 
                           rename(S_historical = S,
                                  t1 = YEAR) %>% 
                           select(-fYear, -sample_effort, -data),
                         local_S %>%
                           unnest(S) %>% 
                           filter(fYear=='end') %>% 
                           rename(S_modern = S,
                                  t2 = YEAR) %>% 
                           select(-fYear, -sample_effort, -data)) %>% 
  mutate(alpha_LR = log(S_modern/S_historical),
         deltaT = t2 - t1 + 1,
         ES = alpha_LR / deltaT)


bt_local_mean_LR <- bt_local_LR %>% 
  group_by(STUDY_ID) %>% 
  summarise(alpha_LR_mu = mean(alpha_LR),
            check = mean(log(S_modern/S_historical)),
            alpha_LR_sd = sd(alpha_LR),
            ES_mu = mean(ES),
            ES_se = sd(ES)/(sqrt(n_distinct(loc_plot)))) %>% 
  ungroup()

bt_regional_LR <- left_join(regional_S %>% 
                              filter(fYear=='start') %>% 
                              rename(S_historical = S, 
                                     t1 = YEAR) %>% 
                              select(-fYear),
                            regional_S %>% 
                              filter(fYear=='end') %>% 
                              rename(S_modern = S,
                                     t2 = YEAR) %>% 
                              select(-fYear)) %>% 
  mutate(gamma_LR = log(S_modern/S_historical),
         deltaT = t2 - t1 + 1,
         ES = gamma_LR / deltaT)

# calculate jacknife regional LR
# first put Year back in for effect size calculation
bt_regional_jknife <- left_join(regional_jknife, 
                             local_S %>% 
            distinct(STUDY_ID, fYear, YEAR))

bt_regional_jknife_LR <- left_join(bt_regional_jknife %>% 
                                  filter(fYear=='start') %>% 
                                  rename(S_historical = S_jk, 
                                         t1 = YEAR) %>% 
                                  select(-fYear),
                                  bt_regional_jknife %>% 
                                  filter(fYear=='end') %>% 
                                  rename(S_modern = S_jk,
                                         t2 = YEAR) %>% 
                                  select(-fYear)) %>% 
  mutate(gamma_LR = log(S_modern/S_historical),
         deltaT = t2 - t1 + 1,
         ES = gamma_LR / deltaT)

ggplot() +
  geom_point(data = left_join(bt_local_mean_LR, 
                              bt_regional_LR),
             aes(x = alpha_LR_mu, y = gamma_LR)) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2)

# 
ggplot() +
  geom_point(data = left_join(bt_local_mean_LR, 
                              bt_regional_LR),
             aes(x = ES_mu, y = ES)) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2)


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
     bt_local_mean_LR,
     bt_regional_LR,
     bt_regional_jknife_LR,
     bt_regional_jknife,
     file = '~/Dropbox/1current/spatial_composition_change/results/bt_LRR.Rdata')

