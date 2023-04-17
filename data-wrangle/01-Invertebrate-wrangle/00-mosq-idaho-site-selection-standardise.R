rm(list=ls()) 

defaultW <- getOption("warn")
options(warn = -1)

library(tidyverse)

idaho <- readRDS('~/Dropbox/1current/spatial_composition_change/data/Montana, North Dakota, Idaho.rds') %>% 
  as_tibble()

# one locations
idaho %>% distinct(Locations)

# use long-lat to creat 'plot'
idaho <- idaho %>% 
  unite(col = plot, c(Longitudes, Latitudes), remove = FALSE)

ggplot() +
  facet_wrap(~Locations) +
  geom_point(data = idaho %>% distinct(Locations, Longitudes, Latitudes),
             aes(x = Longitudes, y = Latitudes))

idaho %>% 
  group_by(Locations) %>% 
  summarise(n_plots = n_distinct(plot)) %>% 
  filter(n_plots > 3)

# there are a bunch of different attractants: can we lump carbon dioxide, visible light with visible light
idaho %>% 
  group_by(Attractants) %>% 
  summarise(n = n())

# one attractant only
idaho_vl <- idaho %>% 
  filter(Attractants=='carbon dioxide,visible light')

# one collection protocol
idaho_vl %>% distinct(Collection.protocols)
idaho_vl %>% 
  group_by(Collection.protocols) %>% 
  summarise(n())

idaho_vl <- idaho_vl %>% 
  filter(Collection.protocols=='EVS trap catch')

#  collection duration: one day for the visible light data
idaho_vl %>% distinct(Collection.duration..days.)
idaho_vl %>% 
  group_by(Collection.duration..days.) %>% 
  summarise(n())

idaho_vl <- idaho_vl %>% 
  filter(Collection.duration..days.==1)

# tidy dates
idaho_vl$date <- as.Date(substring(idaho_vl$Collection.date.range, 1,10))
idaho_vl$year<- as.numeric(substring(idaho_vl$Collection.date.range, 1,4))
idaho_vl$month <- as.numeric(substring(idaho_vl$Collection.date.range, 6,7))

# location and duration metadata to determine which plots for analysis
idaho_meta <- idaho_vl %>% 
  group_by(plot) %>% 
  summarise(duration = max(year) - min(year) + 1)

idaho_10 <- idaho_meta %>% 
  filter(duration > 9)

idaho10 <- idaho_vl %>% 
  filter(plot %in% idaho_10$plot)

# have multiple days and months sampled within each year
count_months <- idaho10 %>% 
  group_by(plot, year) %>% 
  summarise(n_month = n_distinct(month))

count_days <- idaho10 %>% 
  filter(Specimens.collected > 0) %>% 
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
  geom_point(data = count_days %>% filter(n_days< 5),
             aes(x = year, y = month, colour = n_days)) +
  theme(legend.position = 'none')

# want to identify same years and months for > 4 sites:
# can use months 6-9 inclusive with these years for 11 sites
ggplot() +
  facet_wrap(~plot) +
  geom_point(data = count_days %>% 
               filter(n_days > 1) %>% 
               # look at different durations to maximise number of sites (and months to combine)
               filter(year > 2007 & year < 2018) %>% 
               filter(month==7 | month==8) %>% 
               filter(plot!='-116.096_43.548' & plot!='-116.222_43.6107' & plot!='-116.241_43.6478' &
                        plot!='-116.253_43.589' & plot!='-116.273_43.6615' & plot!='-116.338_43.5813' &
                        plot!='-116.346_43.6708' & plot!='-116.411_43.5096' & plot!='-116.42_43.5359' & 
                        plot!='-116.421_43.658' & plot!='-116.446_43.6164' & plot!='-116.464_43.6396' & plot!='-116.492_43.7062'),
             aes(x = year, y = month, colour = n_days)) +
  theme(legend.position = 'none')



idaho_sites <- count_days %>% 
  filter(n_days > 1) %>% 
  # look at different durations to maximise number of sites (and months to combine)
  filter(year > 2007 & year < 2018) %>% 
  filter(month==7 | month==8) %>% 
  filter(plot!='-116.096_43.548' & plot!='-116.222_43.6107' & plot!='-116.241_43.6478' &
           plot!='-116.253_43.589' & plot!='-116.273_43.6615' & plot!='-116.338_43.5813' &
           plot!='-116.346_43.6708' & plot!='-116.411_43.5096' & plot!='-116.42_43.5359' & 
           plot!='-116.421_43.658' & plot!='-116.446_43.6164' & plot!='-116.464_43.6396' & plot!='-116.492_43.7062') %>% 
  unite(filter, c(plot, year, month), remove = FALSE)

# need min number of days sampled (want same effort at all sites)
min_days <- idaho_sites %>% 
  ungroup() %>% 
  summarise(min_days = min(n_days))

idaho_filtered <- idaho10 %>% 
  unite(pym, c(plot, year, month), remove = FALSE) %>% 
  filter(pym %in% idaho_sites$filter)

idaho_filtered$min_days = min_days$min_days

# want to combine min_days from months 6-9 for each plot / year combination
local_resamps <- NULL

idaho_nest <- idaho_filtered %>% 
  select(plot, date, year, month, Species, Specimens.collected, min_days) %>% 
  group_by(plot) %>% 
  nest(data = c(date, year, month, Species, Specimens.collected, min_days)) %>% 
  ungroup()

# suppress summarise statement (so counter is visible)
options(dplyr.summarise.inform = FALSE)

for(p in 1:n_distinct(idaho_nest$plot)){
  # combine min_days from months 6-9 for each plot / year combination 
  plot = idaho_nest %>% 
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
      print(paste('resample ', resamps, ' of 200, for year', t, ' of ', length(time), 'in plot', p, 'of ', n_distinct(idaho_nest$plot)))
      
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
  mutate(region = 'idaho-n_dakota-montana')

gamma_S <- local_resamps %>% 
  group_by(year, resample,Species) %>% 
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
  mutate(region = 'idaho-n_dakota-montana')

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
    group_by(year, resample, Species) %>% 
    summarise(N = sum(N)) %>% 
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
    mutate(region = 'idaho-n_dakota-montana',
           jacknife = j)
  
  end_temp = study_end %>% 
    group_by(plot) %>% 
    nest(data = c(year, Species, N, resample)) %>% 
    ungroup() %>% 
    slice(-j) %>% 
    unnest(data) %>% 
    group_by(year, resample, Species) %>% 
    summarise(N = sum(N)) %>% 
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
    mutate(region = 'idaho-n_dakota-montana',
           jacknife = j)
  
  # join
  study_jknife = bind_rows(study_jknife, 
                           start_temp,
                           end_temp) %>% 
    mutate(n_loc_plots = n_plots)
}


study_jknife <- study_jknife %>% 
  mutate(fYear = case_when(year==2008 ~ 'start',
                           year==2017 ~ 'end'))

save(local_resamps, alpha_S, gamma_S, study_jknife,
     file = '~/Dropbox/1current/spatial_composition_change/data/idaho-n_dakota-montana_clean.Rdata')

options(warn = defaultW)

