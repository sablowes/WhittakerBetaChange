rm(list=ls()) 

defaultW <- getOption("warn")
options(warn = -1)

library(tidyverse)

indiana <- readRDS('~/Dropbox/1current/spatial_composition_change/data/Indiana 2021.rds') %>% 
  as_tibble()

# one locations
indiana %>% distinct(Locations)

# use long-lat to creat 'plot'
indiana <- indiana %>% 
  unite(col = plot, c(Longitudes, Latitudes), remove = FALSE)

ggplot() +
  facet_wrap(~Locations) +
  geom_point(data = indiana %>% distinct(Locations, Longitudes, Latitudes),
             aes(x = Longitudes, y = Latitudes))

# there are a bunch of different attractants: can we lump carbon dioxide, visible light with visible light
indiana %>% 
  group_by(Attractants) %>% 
  summarise(n = n())

# one attractant only
indiana_vl <- indiana %>% 
  filter(Attractants=='visible light')

# one collection protocol
indiana_vl %>% distinct(Collection.protocols)

#  collection duration: one day for the visible light data
indiana_vl %>% distinct(Collection.duration..days.)
indiana_vl %>% 
  group_by(Collection.duration..days.) %>% 
  summarise(n())

indiana_vl <- indiana_vl %>% 
  filter(Collection.duration..days.==1)

# tidy dates
indiana_vl$date <- as.Date(substring(indiana_vl$Collection.date.range, 1,10))
indiana_vl$year<- as.numeric(substring(indiana_vl$Collection.date.range, 1,4))
indiana_vl$month <- as.numeric(substring(indiana_vl$Collection.date.range, 6,7))

# location and duration metadata to determine which plots for analysis
indiana_meta <- indiana_vl %>% 
  group_by(plot) %>% 
  summarise(duration = max(year) - min(year) + 1)

indiana_10 <- indiana_meta %>% 
  filter(duration > 9)

indiana10 <- indiana_vl %>% 
  filter(plot %in% indiana_10$plot)

# have multiple days and months sampled within each year
count_months <- indiana10 %>% 
  group_by(plot, year) %>% 
  summarise(n_month = n_distinct(month))

count_days <- indiana10 %>% 
  group_by(plot, year, month) %>% 
  summarise(n_days = n_distinct(date))

# get at least 20 days
ggplot() +
  facet_wrap(~plot) +
  geom_density(data = count_days,
               aes(x = n_days))


# we get full duration using 20 days in month nine
ggplot() +
  facet_wrap(~plot) +
  geom_point(data = count_days %>% filter(n_days > 5),
             aes(x = year, y = month, colour = n_days)) +
  theme(legend.position = 'none')

# want to identify same years and months for > 4 sites:
# can use months 6-9 inclusive with these years for 11 sites
ggplot() +
  facet_wrap(~plot) +
  geom_point(data = count_days %>% 
               filter(n_days > 5) %>% 
               # look at different durations to maximise number of sites (and months to combine)
               filter(year > 2008 ) %>% 
               filter(month> 5 & month < 9),
             aes(x = year, y = month, colour = n_days)) +
  theme(legend.position = 'none')



indiana_sites <- count_days %>% 
  filter(n_days > 5) %>% 
  # look at different durations to maximise number of sites (and months to combine)
  filter(year > 2008 ) %>% 
  filter(month> 5 & month < 9) %>% 
  unite(filter, c(plot, year, month), remove = FALSE)

# need min number of days sampled (want same effort at all sites)
min_days <- indiana_sites %>% 
  ungroup() %>% 
  summarise(min_days = min(n_days))

indiana_filtered <- indiana10 %>% 
  unite(pym, c(plot, year, month), remove = FALSE) %>% 
  filter(pym %in% indiana_sites$filter)

indiana_filtered$min_days = min_days$min_days

# want to combine min_days from months 6-9 for each plot / year combination
local_resamps <- NULL

indiana_nest <- indiana_filtered %>% 
  select(plot, date, year, month, Species, Specimens.collected, min_days) %>% 
  group_by(plot) %>% 
  nest(data = c(date, year, month, Species, Specimens.collected, min_days)) %>% 
  ungroup()

# suppress summarise statement (so counter is visible)
options(dplyr.summarise.inform = FALSE)

for(p in 1:n_distinct(indiana_nest$plot)){
  # combine min_days from months 6-9 for each plot / year combination 
  plot = indiana_nest %>% 
    slice(p) %>% 
    unnest()
  
  # time counter
  time = unique(plot$year)
  for(t in 1:length(time)){
    plot_t <- plot %>% 
      filter(year==time[t]) %>% 
      group_by(plot, month, date) %>% 
      nest(data = c(Species, Specimens.collected)) %>% 
      ungroup()
    
    
    # get min_days from each month 200 times
    for(resamps in 1:200){
      print(paste('resample ', resamps, ' of 200, for year', t, ' of ', length(time), 'in plot', p, 'of ', n_distinct(indiana_nest$plot)))
      
      samp = plot_t %>% 
        group_by(month) %>% 
        slice_sample(n = as.numeric(min_days))
      
      # combine and calculate richness for year
      alpha_samp <- samp %>% 
        ungroup() %>% 
        unnest() %>% 
        group_by(plot, year, Species) %>% 
        summarise(N = sum(Specimens.collected)) %>% 
        ungroup() %>% 
        filter(N > 0) %>% 
        mutate(resample = resamps)
      
      local_resamps = bind_rows(local_resamps, alpha_samp)
    }
  }
}

alpha_S <- local_resamps %>% 
  group_by(plot, year, resample) %>% 
  summarise(S_resamp = n_distinct(Species),
            eH_resamp = exp(vegan::diversity(N, index = 'shannon')),
            S_PIE_resamp = vegan::diversity(N, index = 'invsimpson'),
            J_resamp = sum(N)) %>% 
  ungroup() %>% 
  group_by(plot, year) %>% 
  summarise(S = median(S_resamp),
            eH = median(eH_resamp),
            S_PIE = median(S_PIE_resamp),
            J = median(J_resamp)) %>% 
  ungroup() %>% 
  mutate(region = 'indiana')

gamma_S <- local_resamps %>% 
  group_by(year, resample, Species) %>% 
  summarise(N = sum(N)) %>% 
  group_by(year, resample) %>% 
  summarise(S_resamp = n_distinct(Species),
            eH_resamp = exp(vegan::diversity(N, index = 'shannon')),
            S_PIE_resamp = vegan::diversity(N, index = 'invsimpson'),
            J_resamp = sum(N)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  summarise(S = median(S_resamp),
            eH = median(eH_resamp),
            S_PIE = median(S_PIE_resamp),
            J = median(J_resamp)) %>% 
  ungroup() %>% 
  mutate(region = 'indiana')

# also want regional jack knife resample
regional_jknife <- NULL
n_plots <- length(unique(local_resamps$plot))
# calculate jacknife resampled regional S for each study in a loop 
# only first and last years for now...
study_start = local_resamps %>% 
  filter(year==min(local_resamps$year))
study_end = local_resamps %>% 
  filter(year==max(local_resamps$year))

# initial temporary storage for each study
study_jknife = NULL
for(j in 1:n_plots){
  # drop on row and calculate regional richness
  start_temp = study_start %>% 
    group_by(plot) %>% 
    nest(data = c(year, Species, N, resample)) %>% 
    ungroup() %>% 
    slice(-j) %>% 
    unnest(data) %>% 
    group_by(year, resample) %>% 
    summarise(S_resamp = n_distinct(Species),
              eH_resamp = exp(vegan::diversity(N, index = 'shannon')),
              S_PIE_resamp = vegan::diversity(N, index = 'invsimpson'),
              J_resamp = sum(N)) %>% 
    ungroup() %>% 
    group_by(year) %>% 
    summarise(S_jk = round(median(S_resamp)),
              eH_jk = median(eH_resamp),
              S_PIE_jk = median(S_PIE_resamp),
              J_jk = median(J_resamp)) %>% 
    ungroup() %>%
    mutate(region = 'indiana',
           jacknife = j)
  
  end_temp = study_end %>% 
    group_by(plot) %>% 
    nest(data = c(year, Species, N, resample)) %>% 
    ungroup() %>% 
    slice(-j) %>% 
    unnest(data) %>% 
    group_by(year, resample) %>% 
    summarise(S_resamp = n_distinct(Species),
              eH_resamp = exp(vegan::diversity(N, index = 'shannon')),
              S_PIE_resamp = vegan::diversity(N, index = 'invsimpson'),
              J_resamp = sum(N)) %>% 
    ungroup() %>% 
    group_by(year) %>% 
    summarise(S_jk = round(median(S_resamp)),
              eH_jk = median(eH_resamp),
              S_PIE_jk = median(S_PIE_resamp),
              J_jk = median(J_resamp)) %>% 
    ungroup() %>%
    mutate(region = 'indiana',
           jacknife = j)
  
  # join
  study_jknife = bind_rows(study_jknife, 
                           start_temp,
                           end_temp) %>% 
    mutate(n_loc_plots = n_plots)
}


study_jknife <- study_jknife %>% 
  mutate(fYear = case_when(year==2009 ~ 'start',
                           year==2019 ~ 'end'))

save(local_resamps, alpha_S, gamma_S, study_jknife,
     file = '~/Dropbox/1current/spatial_composition_change/code/invertebrate_data/clean_data/indiana_clean.Rdata')

options(warn = defaultW)

