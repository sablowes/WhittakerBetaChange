rm(list=ls()) 

defaultW <- getOption("warn")
options(warn = -1)

library(tidyverse)

florida <- readRDS('~/Dropbox/1current/spatial_composition_change/data/Florida 2021.rds') %>% 
  as_tibble()

# many locations
florida %>% distinct(Locations)

ggplot() +
  facet_wrap(~Locations) +
  geom_point(data = florida %>% distinct(Locations, Longitudes, Latitudes), 
             aes(x = Longitudes, y = Latitudes))

# use long-lat to creat 'plot'
florida <- florida %>% 
  unite(col = plot, c(Longitudes, Latitudes), remove = FALSE)

loc4 <- florida %>% 
  group_by(Locations) %>% 
  summarise(n_plots = n_distinct(plot)) %>% 
  ungroup() %>% 
  filter(n_plots > 3)

# there are a bunch of different attractants: can we lump carbon dioxide, visible light with visible light
florida %>% 
  group_by(Attractants) %>% 
  summarise(n = n())

# use visible light; reduce to locations with > 4 locations
florida4_vl <- florida %>% 
  filter(Attractants=='carbon dioxide,visible light') %>% 
  filter(Locations %in% loc4$Locations)

# one collection protocol
florida4_vl %>% distinct(Collection.protocols)
florida4_vl %>%
  group_by(Collection.protocols) %>% 
  summarise(n())

florida4_vl <- florida4_vl %>% 
  filter(Collection.protocols=='CDC light trap')

#  collection duration: one day for the visible light data
florida4_vl %>% distinct(Collection.duration..days.)

# tidy dates
florida4_vl$date <- as.Date(substring(florida4_vl$Collection.date.range, 1,10))
florida4_vl$year<- as.numeric(substring(florida4_vl$Collection.date.range, 1,4))
florida4_vl$month <- as.numeric(substring(florida4_vl$Collection.date.range, 6,7))

# location and duration metadata to determine which plots for analysis
florida_meta <- florida4_vl %>% 
  group_by(Locations, plot) %>% 
  summarise(duration = max(year) - min(year) + 1)

florida_10 <- florida_meta %>% 
  filter(duration > 9)

florida10 <- florida4_vl %>% 
  filter(plot %in% florida_10$plot) %>% 
  # reapply requirement for ≥ 4 plots per location
  group_by(Locations) %>% 
  mutate(n_plots = n_distinct(plot)) %>% 
  ungroup() %>% 
  filter(n_plots > 3) %>% 
  select(-n_plots)

# have multiple days and months sampled within each year
count_months <- florida10 %>% 
  group_by(Locations, plot, year) %>% 
  summarise(n_month = n_distinct(month))

count_days <- florida10 %>% 
  group_by(Locations, plot, year, month) %>% 
  summarise(n_days = n_distinct(date))

# get at least 20 days
ggplot() +
  facet_wrap(Locations~plot) +
  geom_density(data = count_days,
               aes(x = n_days))

# visual inspection to identify years and plots for each region
ggplot() +
  facet_grid(Locations~plot) +
  geom_point(data = count_days %>% filter(n_days > 1),
             aes(x = year, y = month, colour = n_days)) +
  theme(legend.position = 'none')

# want to identify same years and months for > 4 sites:
# visual inspection
ggplot() +
  facet_wrap(~plot) +
  geom_point(data = count_days %>% 
               filter(Locations=='Manatee' & n_days > 1) %>% 
               filter(plot!='-82.3617_27.4894' & plot!='-82.4358_27.4424' & plot!='-82.4798_27.4197' &
                        plot!='-82.5357_27.6031' & plot!='-82.5377_27.5163' & plot!='-82.5489_27.4394' &
                        plot!='-82.5824_27.531') %>% 
               # look at different durations to maximise number of sites (and months to combine)
               filter(year > 2006 & year < 2021) %>% 
               filter(month > 5 & month < 9),
             aes(x = year, y = month, colour = n_days)) +
  scale_y_continuous(breaks = 1:12) +
  scale_x_continuous(breaks = 2007:2020) +
  theme(legend.position = 'none')

manatee <- florida10 %>% 
  filter(Locations=='Manatee') %>% 
  filter(plot!='-82.3617_27.4894' & plot!='-82.4358_27.4424' & plot!='-82.4798_27.4197' &
           plot!='-82.5357_27.6031' & plot!='-82.5377_27.5163' & plot!='-82.5489_27.4394' &
           plot!='-82.5824_27.531')  %>% 
  filter(month > 5 & month < 9) %>% 
  filter(year > 2006 & year < 2021) 
  
  
# Lee county: retain the plots with samples in 2008 and 2018
ggplot() +
  facet_wrap(~plot) +
  geom_point(data = count_days %>% 
               filter(Locations!='Manatee' & n_days > 1) %>% 
               # look at different durations to maximise number of sites (and months to combine)
               filter(year > 2006 & year < 2021) %>% 
               filter(month > 5 & month < 11) %>% 
               filter(plot!='-81.6685_26.62' & plot!='-81.891_26.5197' & plot!='-82.1672_26.4846'),
             aes(x = year, y = month, colour = n_days)) +
  scale_y_continuous(breaks = 1:12) +
  scale_x_continuous(breaks = 2007:2020) +
  theme(legend.position = 'none')


# sites and years / months in Lee county
lee <- florida10 %>% 
  filter(Locations!='Manatee') %>% 
  filter(year > 2006 & year < 2021) %>% 
  filter(month > 3 & month < 11) %>% 
  filter(plot!='-81.6685_26.62' & plot!='-81.891_26.5197' & plot!='-82.1672_26.4846')

florida_filtered <- bind_rows(manatee, 
                        lee) %>% 
  # create filter to check num_days
  unite(lp, c(Locations, plot), remove = F)

# need min number of days sampled (want same effort at all sites)
count_days %>% 
  ungroup() %>% 
  unite(filter, c(Locations, plot), remove = FALSE) %>% 
  filter(filter %in% unique(florida_filtered$lp)) %>% 
  ggplot() +
  geom_density(aes(x = n_days))

min_days <- count_days %>% 
  ungroup() %>% 
  unite(filter, c(Locations, plot), remove = FALSE) %>% 
  filter(filter %in% unique(florida_filtered$lp)) %>% 
  group_by(Locations) %>% 
  summarise(min_days = min(n_days))


florida_filtered <- florida_filtered %>% 
  mutate(min_days = 1)

# florida_filtered %>% 
#   distinct(Collection.protocols, Collection.duration..days., Developmental.stage, Sex)

florida_nest <- florida_filtered %>% 
  select(Locations, plot, date, year, month, Species, Specimens.collected, min_days) %>% 
  group_by(Locations, plot) %>% 
  nest(data = c(date, year, month, Species, Specimens.collected, min_days)) %>% 
  ungroup()

# suppress summarise statement (so counter is visible)
options(dplyr.summarise.inform = FALSE)
regions = unique(florida_nest$Locations)

# want to combine min_days from months 6-9 for each plot / year combination
local_resamps <- NULL

for(r in 1:n_distinct(florida_nest$Locations)){
  region = florida_nest %>% 
    filter(Locations==regions[r])
    for(p in 1:n_distinct(region$plot)){
    # combine min_days from months 6-9 for each plot / year combination 
    plot = region %>% 
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
        print(paste('resample ', resamps, ' of 200, for year', t, ' of ', length(time), 'in plot', p, 'of ', n_distinct(region$plot)))
        
        samp = plot_t %>% 
          group_by(month) %>% 
          slice_sample(n = as.numeric(min_days$min_days[1]))
        
        # combine and calculate richness for year
        alpha_samp <- samp %>% 
          ungroup() %>% 
          unnest() %>% 
          group_by(Locations, plot, year, Species) %>% 
          summarise(N = sum(Specimens.collected)) %>% 
          ungroup() %>% 
          filter(N > 0) %>% 
          mutate(resample = resamps)
        
        local_resamps = bind_rows(local_resamps, alpha_samp)
      }
    }
  }
}


alpha_S <- local_resamps %>% 
  group_by(Locations, plot, year, resample) %>% 
  summarise(S_resamp = n_distinct(Species),
            S_PIE_resamp = vegan::diversity(N, index = 'invsimpson')) %>% 
  ungroup() %>% 
  group_by(Locations, plot, year) %>% 
  summarise(S = median(S_resamp),
            S_PIE = median(S_PIE_resamp)) %>% 
  ungroup() %>% 
  rename(region = plot)

gamma_S <- local_resamps %>% 
  group_by(Locations, year, resample, Species) %>% 
  summarise(N = sum(N)) %>% 
  group_by(Locations, resample, year) %>% 
  summarise(S_resamp = n_distinct(Species),
            S_PIE_resamp = vegan::diversity(N, index = 'invsimpson')) %>% 
  ungroup() %>% 
  group_by(Locations, year) %>% 
  summarise(S = median(S_resamp),
            S_PIE = median(S_PIE_resamp)) %>% 
  ungroup() %>% 
  rename(region = Locations)

# also want regional jack knife resample
study_jknife <- NULL
n_region <- length(unique(local_resamps$Locations))
region_id <- unique(local_resamps$Locations)


# calculate jacknife resampled regional S for each study in a loop 
# only first and last years for now...

# initial temporary storage for each study
study_temp = NULL
for(r in 1:n_region){
  
  region = local_resamps %>% 
    filter(Locations==region_id[r])
  
  study_start = region %>% 
    filter(year==min(region$year))
  study_end = region %>% 
    filter(year==max(region$year))
  
  n_plots <- length(unique(region$plot))
  
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
                S_PIE_resamp = vegan::diversity(N, index = 'invsimpson')) %>% 
      ungroup() %>% 
      group_by(year) %>% 
      summarise(S_jk = round(median(S_resamp)),
                S_PIE_jk = median(S_PIE_resamp)) %>% 
      ungroup() %>% 
      mutate(region = unique(region$Locations),
             jacknife = j)
    
    end_temp = study_end %>% 
      group_by(plot) %>% 
      nest(data = c(year, Species, N, resample)) %>% 
      ungroup() %>% 
      slice(-j) %>% 
      unnest() %>% 
      group_by(year, resample, Species) %>% 
      summarise(N = sum(N)) %>% 
      group_by(year, resample) %>% 
      summarise(S_resamp = n_distinct(Species),
                S_PIE_resamp = vegan::diversity(N, index = 'invsimpson')) %>% 
      ungroup() %>% 
      group_by(year) %>% 
      summarise(S_jk = round(median(S_resamp)),
                S_PIE_jk = median(S_PIE_resamp)) %>% 
      ungroup() %>% 
      mutate(region = unique(region$Locations),
             jacknife = j)
    
    # join
    study_temp = bind_rows(study_temp,
                             start_temp,
                             end_temp) %>% 
      mutate(n_loc_plots = n_plots)
  }
  study_jknife <- bind_rows(study_jknife, study_temp)
  study_temp = NULL # reset study storage
}

study_jknife <- study_jknife %>% 
  mutate(fYear = case_when(year==2008 & region=='Lee (Florida (United States))' ~ 'start',
                           year==2018 & region=='Lee (Florida (United States))' ~ 'end',
                           year==2007 & region=='Manatee' ~ 'start',
                           year==2020 & region=='Manatee' ~ 'end'))

save(local_resamps, alpha_S, gamma_S, study_jknife,
     file = '~/Dropbox/1current/spatial_composition_change/data/florida_clean.Rdata')

options(warn = defaultW)

