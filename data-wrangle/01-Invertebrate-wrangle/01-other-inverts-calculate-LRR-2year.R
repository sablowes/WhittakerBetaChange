# calculate metrics for invert data

load('~/Dropbox/1current/spatial_composition_change/data/invert-location-plots.Rdata')

invert_years_max_loc <- invert_years_max_loc %>% 
  unite(study_yr1, c(Datasource_ID, year1), remove = FALSE) %>% 
  unite(study_yr2, c(Datasource_ID, year2), remove = FALSE) 

# filter to reduce to the years that max sites
invert_filtered_2timeOnly <- invert_filtered %>% 
  unite(study_yr, c(Datasource_ID, Year), remove = FALSE) %>% 
  filter(study_yr %in% invert_years_max_loc$study_yr1 | study_yr %in% invert_years_max_loc$study_yr2) %>% 
  group_by(Datasource_ID, loc_plot) %>% 
  mutate(fYear = case_when(Year==min(Year) ~ 'start',
                           Year==max(Year) ~ 'end',
                           (Year!=min(Year) | Year!=max(Year)) ~ 'intermediate')) %>% 
  ungroup() 
  
# check we've got equal numbers of loc_plots at the start and end
left_join(invert_filtered_2timeOnly %>% 
            filter(Number > 0) %>% 
            group_by(Datasource_ID, loc_plot) %>% 
            # still have > 2 years
            mutate(fYear = case_when(Year==min(Year) ~ 'start',
                                     Year==max(Year) ~ 'end',
                                     (Year!=min(Year) | Year!=max(Year)) ~ 'intermediate')) %>% 
            ungroup() %>% 
            filter(fYear=='start') %>% 
            group_by(Datasource_ID) %>% 
            summarise(n_loc_plots_start = n_distinct(loc_plot)),
          invert_filtered_2timeOnly %>% 
            filter(Number > 0) %>% 
            group_by(Datasource_ID, loc_plot) %>% 
            mutate(fYear = case_when(Year==min(Year) ~ 'start',
                                     Year==max(Year) ~ 'end',
                                     (Year!=min(Year) | Year!=max(Year)) ~ 'intermediate')) %>% 
            ungroup() %>% 
            filter(fYear=='end') %>% 
            group_by(Datasource_ID) %>% 
            summarise(n_loc_plots_end = n_distinct(loc_plot))) %>% 
  filter(n_loc_plots_start!=n_loc_plots_end)


# want to know how many samples per year per loc_plot (within studies)
raw_new <- invert_filtered_2timeOnly %>% 
  # remove zeroes
  filter(Number > 0) %>% 
  group_by(Datasource_ID, loc_plot) %>% 
  # remove intermediate years
  mutate(fYear = case_when(Year==min(Year) ~ 'start',
                           Year==max(Year) ~ 'end',
                           (Year!=min(Year) | Year!=max(Year)) ~ 'intermediate')) %>% 
  filter(fYear!='intermediate') %>% 
  ungroup() %>% 
  unite(ObsEventID, c(Datasource_ID, loc_plot, Year), remove = F) %>% 
  group_by(Datasource_ID, loc_plot, Year) %>% 
  mutate(sample_effort = n_distinct(ObsEventID)) %>% 
  select(Datasource_ID, loc_plot, Year, sample_effort, Taxon, Number) %>% 
  rename(N = Number) %>% 
  group_by(Datasource_ID, loc_plot, Year, sample_effort) %>% 
  nest(data = c(Taxon, N)) %>% 
  ungroup() %>% 
  group_by(Datasource_ID, loc_plot) %>% 
  mutate(fYear = ifelse(Year==min(Year), 'start', 'end')) %>% 
  ungroup() 

# no study has effort > 1, so in the next step we just take a subsample of 1 from each loc_plot
raw_new %>%
  filter(sample_effort > 1) %>% 
  distinct(Datasource_ID)

local_S <- raw_new %>% 
  mutate(S = map(data, ~n_distinct(.x$Taxon)),
         eH = map(data, ~exp(vegan::diversity(.x$N, index= 'shannon'))),
         S_PIE = map(data, ~vegan::diversity(.x$N, index= 'invsimpson')))

regional_S <- raw_new %>% 
  unnest(data) %>% 
  group_by(Datasource_ID, fYear, Taxon) %>% 
  summarise(N = sum(N)) %>% 
  group_by(Datasource_ID, fYear) %>% 
  summarise(S = n_distinct(Taxon),
            eH = exp(vegan::diversity(N, index = 'shannon')),
            S_PIE = vegan::diversity(N, index = 'invsimpson')) %>% 
  ungroup() %>% 
  # put the Year back in for start and finish time points
  left_join(local_S %>% 
              distinct(Datasource_ID, fYear, Year))

# visual sanity check
left_join(local_S %>% 
            unnest(S) %>% 
            rename(local_S = S) %>% 
            select(-eH, -S_PIE),
          regional_S %>% 
            rename(regional_S = S) %>% 
            select(-eH, -S_PIE)) %>% 
  ggplot() +
  geom_point(aes(x = local_S, y = regional_S)) +
  geom_abline(intercept = 0, slope = 1, lty = 2)

# also want to jackknife resample for regional-scale analysis
# add the number of loc_plots to each combination of study/fYear combination
prep <- raw_new %>% 
  group_by(Datasource_ID, fYear) %>% 
  mutate(n_loc_plots = n_distinct(loc_plot)) %>% 
  ungroup()

# suppress summarise statement (so counter is visible)
options(dplyr.summarise.inform = FALSE)

regional_jknife <- NULL
# calculate jacknife resampled regional S for each study in a loop 
for(i in 1:length(unique(prep$Datasource_ID))){
  print(paste('study ', i, ' in ', length(unique(prep$Datasource_ID))))
  
  # get a study
  study_start = prep %>% 
    filter(Datasource_ID==unique(prep$Datasource_ID)[i] & fYear=='start')
  study_end = prep %>% 
    filter(Datasource_ID==unique(prep$Datasource_ID)[i] & fYear=='end')
  
  # initial temporary storage for each study
  study_jknife = NULL
  for(j in 1:unique(study_start$n_loc_plots)){
    # drop on row and calculate regional richness
    start_temp = study_start %>% 
      slice(-j) %>% 
      unnest(data) %>% 
      group_by(Datasource_ID, fYear, Taxon) %>% 
      summarise(N = sum(N)) %>% 
      group_by(Datasource_ID, fYear) %>% 
      summarise(S_jk = n_distinct(Taxon),
                eH_jk = exp(vegan::diversity(N, index = 'shannon')),
                S_PIE_jk = vegan::diversity(N, index = 'invsimpson')) %>% 
      ungroup() %>% 
      mutate(jacknife = j)
    
    end_temp = study_end %>% 
      slice(-j) %>% 
      unnest(data) %>% 
      group_by(Datasource_ID, fYear, Taxon) %>% 
      summarise(N = sum(N)) %>% 
      group_by(Datasource_ID, fYear) %>% 
      summarise(S_jk = n_distinct(Taxon),
                eH_jk = exp(vegan::diversity(N, index = 'shannon')),
                S_PIE_jk = vegan::diversity(N, index = 'invsimpson')) %>% 
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
            group_by(Datasource_ID, fYear) %>% 
            summarise(S_jk_mu = mean(S_jk))) %>% 
  ggplot() +
  geom_point(aes(x = S_jk_mu, y = S)) +
  geom_abline(intercept = 0, slope = 1, lty = 2)

# calculate local LR
invert_local_LR <- left_join(local_S %>%
                           unnest(cols = c(S, eH, S_PIE)) %>% 
                           filter(fYear=='start') %>% 
                           rename(S_historical = S,
                                  eH_historical = eH,
                                  S_PIE_historical = S_PIE,
                                  t1 = Year) %>% 
                           select(-fYear, -sample_effort, -data),
                         local_S %>%
                           unnest(cols = c(S, eH, S_PIE)) %>% 
                           filter(fYear=='end') %>% 
                           rename(S_modern = S,
                                  eH_modern = eH,
                                  S_PIE_modern = S_PIE,
                                  t2 = Year) %>% 
                           select(-fYear, -sample_effort, -data)) %>% 
  mutate(alpha_LR = log(S_modern/S_historical),
         alpha_LR_eH = log(eH_modern / eH_historical),
         alpha_LR_S_PIE = log(S_PIE_modern / S_PIE_historical),
         deltaT = t2 - t1 + 1,
         ES = alpha_LR / deltaT,
         ES_eH = alpha_LR_eH / deltaT,
         ES_S_PIE = alpha_LR_S_PIE / deltaT)

# 
# invert_local_mean_LR <- invert_local_LR %>% 
#   group_by(Datasource_ID) %>% 
#   summarise(alpha_LR_mu = mean(alpha_LR),
#             check = mean(log(S_modern/S_historical)),
#             alpha_LR_sd = sd(alpha_LR),
#             ES_mu = mean(ES),
#             ES_se = sd(ES)/(sqrt(n_distinct(loc_plot)))) %>% 
#   ungroup()
# 
invert_regional_LR <- left_join(regional_S %>% 
                              filter(fYear=='start') %>% 
                                rename(S_historical = S,
                                       eH_historical = eH,
                                       S_PIE_historical = S_PIE,
                                       t1 = Year) %>% 
                              select(-fYear),
                            regional_S %>% 
                              filter(fYear=='end') %>% 
                              rename(S_modern = S,
                                     eH_modern = eH,
                                     S_PIE_modern = S_PIE,
                                     t2 = Year) %>% 
                              select(-fYear)) %>% 
  mutate(gamma_LR = log(S_modern/S_historical),
         gamma_LR_eH = log(eH_modern / eH_historical),
         gamma_LR_S_PIE = log(S_PIE_modern / S_PIE_historical),
         deltaT = t2 - t1 + 1,
         ES = gamma_LR / deltaT,
         ES_eH = gamma_LR_eH / deltaT,
         ES_S_PIE = gamma_LR_S_PIE / deltaT)

# calculate jacknife regional LR
# first put Year back in for effect size calculation
invert_regional_jknife <- left_join(regional_jknife, 
                                local_S %>% 
                                  distinct(Datasource_ID, fYear, Year))

invert_regional_jknife_LR <- left_join(invert_regional_jknife %>% 
                                     filter(fYear=='start') %>% 
                                     rename(S_historical = S_jk, 
                                            eH_historical = eH_jk,
                                            S_PIE_historical = S_PIE_jk,
                                            t1 = Year) %>% 
                                     select(-fYear),
                                   invert_regional_jknife %>% 
                                     filter(fYear=='end') %>% 
                                     rename(S_modern = S_jk,
                                            eH_modern = eH_jk,
                                            S_PIE_modern = S_PIE_jk,
                                            t2 = Year) %>% 
                                     select(-fYear)) %>% 
  mutate(gamma_LR = log(S_modern/S_historical),
         gamma_LR_eH = log(eH_modern / eH_historical),
         gamma_LR_S_PIE = log(S_PIE_modern / S_PIE_historical),
         deltaT = t2 - t1 + 1,
         ES = gamma_LR / deltaT,
         ES_eH = gamma_LR_eH / deltaT,
         ES_S_PIE = gamma_LR_S_PIE / deltaT)

ggplot() +
  geom_point(data = left_join(invert_local_mean_LR, 
                              invert_regional_LR),
             aes(x = alpha_LR_mu, y = gamma_LR)) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2)

# 
ggplot() +
  geom_point(data = left_join(invert_local_mean_LR, 
                              invert_regional_LR),
             aes(x = ES_mu, y = ES)) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2)

ggplot() +
  geom_point(data = invert_local_mean_LR,
             aes(x = alpha_LR_mu, y = ES_mu)) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2)

ggplot() +
  geom_point(data = invert_regional_LR,
             aes(x = gamma_LR, y = ES)) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2)

ggplot() +
  geom_point(data = left_join(invert_regional_jknife_LR %>% 
                                group_by(Datasource_ID) %>% 
                                summarise(mean_jk_LR = mean(ES)), 
                              invert_regional_LR),
             aes(x = mean_jk_LR, y = ES)) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2)



save(invert_local_LR, 
     # invert_local_mean_LR,
     invert_regional_LR,
     invert_regional_jknife_LR,
     invert_regional_jknife,
     file = '~/Dropbox/1current/spatial_composition_change/results/invert_LRR.Rdata')

