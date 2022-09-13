# calculate metrics for invert data

load('~/Dropbox/1current/spatial_composition_change/data/invert-location-plots.Rdata')

invert_years_max_loc <- invert_years_max_loc %>% 
  unite(study_yr1, c(Datasource_ID, year1), remove = FALSE) %>% 
  unite(study_yr2, c(Datasource_ID, year2), remove = FALSE) 

# check how many Years for each study, identify median Year for those with > 3 Years sampled
study_yr_median <- tibble()
for(s in 1:nrow(invert_years_max_loc)){
  print(paste('study ', s, 'in ', nrow(invert_years_max_loc)))
  # sth study
  study_s = invert_years_max_loc$Datasource_ID[s]
  
  study_s_dat <- invert_filtered %>% 
    filter(Datasource_ID==study_s)
  
  # if there are only 3 distinct Years, we can't average for initial and last observations
  n_Years = length(unique(study_s_dat$Year))
  
  temp = tibble(Datasource_ID = study_s,
                n_yrs = n_Years,
                med_yr = ifelse(n_Years > 3, (min(study_s_dat$Year) + max(study_s_dat$Year))/ 2,
                                NA)) %>% 
    mutate(n_before = sum(unique(study_s_dat$Year) < med_yr),
           n_after = sum(unique(study_s_dat$Year) > med_yr))
  
  study_yr_median <- bind_rows(study_yr_median,
                               temp)
}

# separate studies where only first and last Year can be used (i.e., those with ≤3 Years sampled)
first_last_only <- study_yr_median %>% 
  filter(is.na(med_yr))

fl_filter <- invert_years_max_loc %>% 
  unite(study_yr1, c(Datasource_ID, year1), remove = FALSE) %>% 
  unite(study_yr2, c(Datasource_ID, year2), remove = FALSE) %>% 
  filter(Datasource_ID %in% first_last_only$Datasource_ID)

fl_dat <- invert_filtered %>% 
  unite(study_yr, c(Datasource_ID, Year), remove = FALSE) %>% 
  filter(study_yr %in% fl_filter$study_yr1 | study_yr %in% fl_filter$study_yr2) %>% 
  select(-study_yr) %>% 
  group_by(Datasource_ID, loc_plot) %>% 
  mutate(period = case_when(Year==min(Year) ~ 'first',
                            Year==max(Year) ~ 'second',
                            (Year!=min(Year) | Year!=max(Year)) ~ 'intermediate')) %>% 
  ungroup() 


# check we've got equal numbers of loc_plots at the start and end
left_join(fl_dat %>% 
            filter(period=='first') %>% 
            group_by(Datasource_ID) %>% 
            summarise(n_loc_plots_start = n_distinct(loc_plot)),
          fl_dat %>% 
            filter(period=='second') %>% 
            group_by(Datasource_ID) %>% 
            summarise(n_loc_plots_end = n_distinct(loc_plot))) %>% 
  filter(n_loc_plots_start!=n_loc_plots_end)

# get the data for locations within studies where we can average multiple Years before-after median Year
# chose the earliest and latest Years possible when the number of samples before-after median is unequal
multi_yr_dat <- invert_filtered %>% 
  filter(!Datasource_ID %in% first_last_only$Datasource_ID) %>% 
  group_by(Datasource_ID, loc_plot, Year) %>% 
  nest(data = c(Plot_ID:Region)) %>% 
  ungroup() %>% 
  # put the median Year in
  left_join(study_yr_median) %>% 
  mutate(period = ifelse(Year < med_yr, 'first', 'second')) %>% 
  ungroup() %>% 
  # tidy up 
  select(-n_yrs, -med_yr, -n_before, -n_after)

# need to find target_num_Years for each region 
target_n_yrs <- multi_yr_dat %>% 
  # first, how many Years in each period for each location (within regions)
  group_by(Datasource_ID, loc_plot, period) %>% 
  summarise(n_yr = n_distinct(Year)) %>% 
  # now get minimum number of Years for each region
  group_by(Datasource_ID) %>% 
  mutate(target_num_yrs = min(n_yr)) %>% 
  ungroup()

# now get the target number of Years from the start of each time series
multi_yr_dat1 <- multi_yr_dat %>% 
  filter(period=='first') %>% 
  # put target number in
  left_join(target_n_yrs %>% 
              filter(period == 'first') %>% 
              select(Datasource_ID, period, loc_plot, target_num_yrs)) %>% 
  group_by(Datasource_ID, loc_plot) %>% 
  arrange(-desc(Year), .by_group = TRUE) %>% 
  filter(row_number() <= target_num_yrs) %>% 
  ungroup()

multi_yr_dat2 <- multi_yr_dat %>% 
  filter(period=='second') %>% 
  # put target number in
  left_join(target_n_yrs %>% 
              filter(period == 'second') %>% 
              select(Datasource_ID,  period, loc_plot, target_num_yrs)) %>% 
  group_by(Datasource_ID, loc_plot) %>% 
  arrange(desc(Year), .by_group = TRUE) %>% 
  filter(row_number() <= target_num_yrs) %>% 
  ungroup()

# tidy up 
rm(multi_yr_dat)

# check: do we have the target number of Years for each location and period?
bind_rows(multi_yr_dat1 %>% 
            select(Datasource_ID, Year, loc_plot, period, target_num_yrs),
          multi_yr_dat2 %>% 
            select(Datasource_ID, Year, loc_plot, period, target_num_yrs)) %>% 
  group_by(Datasource_ID, period, loc_plot) %>% 
  summarise(nyrs = n()) %>% 
  ungroup() %>% 
  left_join(target_n_yrs) %>% 
  filter(nyrs != target_num_yrs)


# check: are all locations sampled in the first and last periods
site_period_check <-bind_rows(multi_yr_dat1 %>% 
                                select(-data),
                              multi_yr_dat2 %>% 
                                select(-data)) %>% 
  group_by(Datasource_ID, loc_plot) %>% 
  summarise(n_period = n_distinct(period)) %>% 
  ungroup()

# these sites will need to be removed (they were not sampled in each time period)
remove_rows <- site_period_check %>% 
  filter(n_period!=2) %>% 
  unite(ss, c(Datasource_ID, loc_plot), remove = FALSE)

# remove sites, and recode Year to counter for Year
# Year needs to be recoded for calculating richness within Years 
# (we've got the same number of Years for each location, but 
# actual Years themselves can differ, we need to make these consistent to 
# calculate averages over 'Years')
multi_yr_dat1 <- multi_yr_dat1 %>% 
  unite(ss, c(Datasource_ID, loc_plot), remove = FALSE) %>% 
  filter(!ss %in% remove_rows$ss) %>% 
  select(-ss) %>% 
  group_by(Datasource_ID, loc_plot, period) %>% 
  mutate(yr_i = 1:n()) %>% 
  ungroup()

multi_yr_dat2 <- multi_yr_dat2 %>% 
  unite(ss, c(Datasource_ID, loc_plot), remove = FALSE) %>% 
  filter(!ss %in% remove_rows$ss) %>% 
  select(-ss) %>% 
  group_by(Datasource_ID, loc_plot, period) %>% 
  mutate(yr_i = 1:n()) %>% 
  ungroup()

# POTENTIAL TODO: some studies only have a single Year as target number
# move them to first-last only pipepline? 
# singleyr_studies <- bind_rows(multi_yr_dat1 %>% 
#                                 select(Datasource_ID, Year, loc_plot, period, target_num_yrs),
#                               multi_yr_dat2 %>% 
#                                 select(Datasource_ID, Year, loc_plot, period, target_num_yrs)) %>% 
#   filter(target_num_yrs==1) %>% 
#   distinct(Datasource_ID)

# check number of Years for study-loc_plot are equal in each period
check_yrs_local <- bind_rows(multi_yr_dat1 %>% 
                               select(Datasource_ID, Year, loc_plot, period),
                             multi_yr_dat2 %>% 
                               select(Datasource_ID, Year, loc_plot, period)) %>% 
  group_by(Datasource_ID, loc_plot, period) %>% 
  summarise(nyrs = n_distinct(Year)) %>% 
  ungroup()

check_yrs_local %>% 
  group_by(Datasource_ID, loc_plot) %>% 
  filter(nyrs[period=='first'] != nyrs[period=='second'])

# check number of locations are equal in each period for the regional level
# does make sense to count unique Years, as different locations within regions may
# be using different Year, but the number of Years is equal at all locations (see previous check)
check_loc_regional <- bind_rows(multi_yr_dat1 %>% 
                                  select(Datasource_ID, Year, loc_plot, period),
                                multi_yr_dat2 %>% 
                                  select(Datasource_ID, Year, loc_plot, period)) %>% 
  group_by(Datasource_ID, period) %>% 
  summarise(nplots = n_distinct(loc_plot)) %>% 
  ungroup()

check_loc_regional %>% 
  group_by(Datasource_ID) %>% 
  filter(nplots[period=='first'] != nplots[period=='second'])

# ok ready do calculations
# tidy up data with first and last Years only
fl_dat <- fl_dat %>% 
  group_by(Datasource_ID, loc_plot, Year, period) %>% 
  nest(data = c(Plot_ID:Region)) %>% 
  ungroup()

# calculate local richness (same as primary analysis)
fl_local_S <- fl_dat %>% 
  mutate(S = map(data, ~n_distinct(.x$Taxon)),
         eH = map(data, ~exp(vegan::diversity(.x$Number, index = 'shannon'))),
         S_PIE = map(data, ~vegan::diversity(.x$Number, index = 'invsimpson')))

# calculate regional richness 
fl_regional_S <- fl_dat %>% 
  unnest(data) %>% 
  group_by(Datasource_ID, period, Taxon) %>% 
  summarise(N = sum(Number)) %>% 
  group_by(Datasource_ID, period) %>% 
  summarise(S = n_distinct(Taxon),
            eH = exp(vegan::diversity(N, index = 'shannon')),
            S_PIE = vegan::diversity(N, index = 'invsimpson')) %>% 
  ungroup() 

# and regional richness jackknife
# first some metadata needed in the loop code: number of plots and fYear 
fl_dat <- fl_dat %>% 
  group_by(Datasource_ID) %>% 
  mutate(n_loc_plots = n_distinct(loc_plot)) %>% 
  ungroup() 

# initialise storage for results
fl_regional_jacknife <- NULL
# suppress summarise statement (so counter is visible)
options(dplyr.summarise.inform = FALSE)
# calculate jacknife resampled regional S for each study in a loop 
for(i in 1:length(unique(fl_dat$Datasource_ID))){
  print(paste('study ', i, ' in ', length(unique(fl_dat$Datasource_ID))))
  
  # get a study
  study_start = fl_dat %>% 
    filter(Datasource_ID==unique(fl_dat$Datasource_ID)[i] & period=='first')
  study_end = fl_dat %>% 
    filter(Datasource_ID==unique(fl_dat$Datasource_ID)[i] & period=='second')
  
  # initial temporary storage for each study
  study_jknife = NULL
  for(j in 1:unique(study_start$n_loc_plots)){
    # drop on row and calculate regional richness
    start_temp = study_start %>% 
      slice(-j) %>% 
      unnest(data) %>% 
      group_by(Datasource_ID, period, Taxon) %>% 
      summarise(N = sum(Number)) %>% 
      group_by(Datasource_ID, period) %>% 
      summarise(S_jk = n_distinct(Taxon),
                eH_jk = exp(vegan::diversity(N, index = 'shannon')),
                S_PIE_jk = vegan::diversity(N, index = 'invsimpson')) %>% 
      ungroup() %>% 
      mutate(jacknife = j)
    
    end_temp = study_end %>% 
      slice(-j) %>% 
      unnest(data) %>% 
      group_by(Datasource_ID, period, Taxon) %>% 
      summarise(N = sum(Number)) %>% 
      group_by(Datasource_ID, period) %>% 
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
  fl_regional_jacknife <- bind_rows(fl_regional_jacknife,
                                    study_jknife)
}


# for the study/locations with multiple Years, calculate average richness in each period
multi_yr_local_S <- bind_rows(multi_yr_dat1 %>% 
                                select(-target_num_yrs), 
                              multi_yr_dat2 %>% 
                                select(-target_num_yrs)) %>% 
  # first, richness in each plot in each Year
  mutate(S_yr = map(data, ~n_distinct(.x$Taxon)),
         eH_yr = map(data, ~exp(vegan::diversity(.x$Number, index = 'shannon'))),
         S_PIE_yr = map(data, ~vegan::diversity(.x$Number, index = 'invsimpson'))) %>% 
  unnest(cols = c(S_yr, eH_yr, S_PIE_yr)) %>% 
  # now average over the Years in each period for each plot
  group_by(Datasource_ID, loc_plot, period) %>% 
  summarise(S = mean(S_yr), 
            eH = mean(eH_yr),
            S_PIE = mean(S_PIE_yr),
            nyr = n_distinct(Year)) %>% 
  ungroup()

# redundant check
multi_yr_local_S %>% 
  group_by(Datasource_ID, loc_plot) %>% 
  filter(nyr[period=='first'] != nyr[period=='second'])

multi_yr_regional_S <- bind_rows(multi_yr_dat1 %>% 
                                   select(-target_num_yrs), 
                                 multi_yr_dat2 %>% 
                                   select(-target_num_yrs)) %>% 
  unnest(data) %>% 
  # first get regional diversity in each Year (i.e., combine plots within regions and Years)
  group_by(Datasource_ID, yr_i, period, Taxon) %>%
  summarise(N = sum(Number)) %>% 
  group_by(Datasource_ID, yr_i, period) %>% 
  summarise(S_yr = n_distinct(Taxon),
            eH_yr = exp(vegan::diversity(N, index = 'shannon')),
            S_PIE_yr = vegan::diversity(N, index = 'invsimpson')) %>%
  group_by(Datasource_ID, period) %>% 
  summarise(S = mean(S_yr),
            eH = mean(eH_yr),
            S_PIE = mean(S_PIE_yr)) %>% 
  ungroup()

#  sanity checks: is regional_S > local_S?
left_join(fl_local_S %>% 
            unnest(S) %>% 
            rename(local_S = S) %>% 
          select(-eH, -S_PIE),
          fl_regional_S %>% 
            rename(regional_S = S) %>% 
            select(-eH, -S_PIE)) %>% #filter(local_S > regional_S) 
  ggplot() +
  geom_point(aes(x = local_S, y = regional_S)) +
  geom_abline(intercept = 0, slope = 1, lty = 2)

# multiyr regional jacknife
multiyr_jacknife_prep <- bind_rows(multi_yr_dat1 %>% 
                                     select(-target_num_yrs), 
                                   multi_yr_dat2 %>% 
                                     select(-target_num_yrs)) %>% 
  group_by(Datasource_ID) %>% 
  mutate(n_loc_plots = n_distinct(loc_plot)) %>% 
  ungroup()

# initialise storage for results
multiyr_regional_jacknife_allyrs <- NULL
# suppress summarise statement (so counter is visible)
options(dplyr.summarise.inform = FALSE)
# calculate jacknife resampled regional S for each study in a loop 
for(i in 1:length(unique(multiyr_jacknife_prep$Datasource_ID))){
  print(paste('study ', i, ' in ', length(unique(multiyr_jacknife_prep$Datasource_ID))))
  
  # get a study
  study_start = multiyr_jacknife_prep %>% 
    filter(Datasource_ID==unique(multiyr_jacknife_prep$Datasource_ID)[i] & period=='first')
  study_end = multiyr_jacknife_prep %>% 
    filter(Datasource_ID==unique(multiyr_jacknife_prep$Datasource_ID)[i] & period=='second')
  
  # initial temporary storage for each study
  study_jknife = NULL
  for(j in 1:unique(study_start$n_loc_plots)){
    
    # need to get a jackknife resample (leave one site out) for each Year
    nyr = unique(study_start$yr_i)
    
    # initialise some temporary storage 
    start_temp = NULL
    end_temp = NULL
    for(yr in 1:length(nyr)){
      
      # drop one row and calculate regional richness
      temp1 = study_start %>% 
        filter(yr_i==nyr[yr]) %>% 
        slice(-j) %>% 
        unnest(data) %>% 
        group_by(Datasource_ID, yr_i, period, Taxon) %>% 
        summarise(N = sum(Number)) %>% 
        group_by(Datasource_ID, yr_i, period) %>% 
        summarise(S_jk_yr = n_distinct(Taxon),
                  eH_jk_yr = exp(vegan::diversity(N, index='shannon')),
                  S_PIE_jk_yr = vegan::diversity(N, index = 'invsimpson')) %>% 
        ungroup() %>% 
        mutate(jacknife = j)
      
      start_temp = bind_rows(start_temp, 
                             temp1)
      
      temp2 = study_end %>% 
        filter(yr_i==nyr[yr]) %>% 
        slice(-j) %>% 
        unnest(data) %>% 
        group_by(Datasource_ID, yr_i, period, Taxon) %>% 
        summarise(N = sum(Number)) %>% 
        group_by(Datasource_ID, yr_i, period) %>% 
        summarise(S_jk_yr = n_distinct(Taxon),
                  eH_jk_yr = exp(vegan::diversity(N, index='shannon')),
                  S_PIE_jk_yr = vegan::diversity(N, index = 'invsimpson')) %>%  
        ungroup() %>% 
        mutate(jacknife = j)
      
      end_temp = bind_rows(end_temp,
                           temp2)
    }
    
    # join
    study_jknife = bind_rows(study_jknife, 
                             start_temp,
                             end_temp) %>% 
      mutate(n_loc_plots = unique(study_start$n_loc_plots)[1])
  }
  # join studies
  multiyr_regional_jacknife_allyrs <- bind_rows(multiyr_regional_jacknife_allyrs,
                                                study_jknife)
}

# need to average over Years for each period
multiyr_regional_jacknife <- multiyr_regional_jacknife_allyrs %>% 
  group_by(Datasource_ID, period, jacknife, n_loc_plots) %>% 
  summarise(S_jk = mean(S_jk_yr),
            eH_jk = mean(eH_jk_yr),
            S_PIE_jk = mean(S_PIE_jk_yr)) %>% 
  ungroup()

# calculate local LR
local_S <- bind_rows(fl_local_S %>% 
                       unnest(cols = c(S, eH, S_PIE)) %>% 
                       select(-data),
                     multi_yr_local_S %>% 
                       select(-nyr))

invert_local_LR <- left_join(local_S %>%
                           filter(period=='first') %>% 
                           rename(S_historical = S,
                                  eH_historical = eH,
                                  S_PIE_historical = S_PIE) %>% 
                           select(-Year, -period),
                         local_S %>%
                           filter(period=='second') %>% 
                           rename(S_modern = S,
                                  eH_modern = eH,
                                  S_PIE_modern = S_PIE) %>% 
                           select(-Year, -period)) %>% 
  mutate(alpha_LR = log(S_modern/S_historical),
         alpha_LR_eH = log(eH_modern / eH_historical),
         alpha_LR_S_PIE = log(S_PIE_modern / S_PIE_historical))



# regional log-ratio
regional_S <- bind_rows(fl_regional_S,
                        multi_yr_regional_S)

invert_regional_LR <- left_join(regional_S %>% 
                              filter(period=='first') %>% 
                              rename(S_historical = S,
                                     eH_historical = eH,
                                     S_PIE_historical = S_PIE) %>% 
                              select(-period),
                            regional_S %>% 
                              filter(period=='second') %>% 
                              rename(S_modern = S,
                                     eH_modern = eH,
                                     S_PIE_modern = S_PIE) %>% 
                              select(-period)) %>% 
  mutate(gamma_LR = log(S_modern/S_historical),
         gamma_LR_eH = log(eH_modern / eH_historical),
         gamma_LR_S_PIE = log(S_PIE_modern / S_PIE_historical))

# regional log-ratio
regional_S_jacknife <- bind_rows(fl_regional_jacknife,
                                 multiyr_regional_jacknife)

invert_regional_jacknife_LR <- left_join(regional_S_jacknife %>% 
                                       filter(period=='first') %>% 
                                       rename(S_historical = S_jk,
                                              eH_historical = eH_jk,
                                              S_PIE_historical = S_PIE_jk) %>% 
                                       select(-period),
                                     regional_S_jacknife %>% 
                                       filter(period=='second') %>% 
                                       rename(S_modern = S_jk,
                                              eH_modern = eH_jk,
                                              S_PIE_modern = S_PIE_jk) %>% 
                                       select(-period)) %>% 
  mutate(gamma_LR = log(S_modern/S_historical),
         gamma_LR_eH = log(eH_modern / eH_historical),
         gamma_LR_S_PIE = log(S_PIE_modern / S_PIE_historical))


# use the same dt (duration) as initial analyses
load('~/Dropbox/1current/spatial_composition_change/results/allLRR.Rdata')
dt <- local_LRR %>% 
  filter(database=='Invertebrates') %>% 
  separate(regional_level, c('i', 'Datasource_ID'), extra = 'merge') %>% 
  distinct(Datasource_ID, dt) %>% 
  # remove other invertebrate data
  filter(!Datasource_ID %in% c('Chicago', 'Lee (Florida (United States))',
                              'Manatee', 'idaho-n_dakota-montana', 'indiana',
                              'iowa', 'virginia', 'enc_butterflies', 
                              'enc_moths', 'enc_beetles')) %>% 
  mutate(Datasource_ID = as.numeric(Datasource_ID))

invert_local_LR <- left_join(invert_local_LR,
                         dt)

invert_regional_LR <- left_join(invert_regional_LR,
                            dt)

invert_regional_jacknife_LR <- left_join(invert_regional_jacknife_LR,
                                     dt)

save(invert_local_LR, 
     invert_regional_LR,
     invert_regional_jacknife_LR,
     file = '~/Dropbox/1current/spatial_composition_change/results/invert_multiyr_LRR.Rdata')

