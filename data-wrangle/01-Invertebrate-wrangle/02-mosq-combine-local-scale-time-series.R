## combine mosquito data and create time series
library(tidyverse)
mosquito_alpha <- NULL
mosquito_gamma <- NULL
mosquito_jknife <- NULL

load('~/Dropbox/1current/spatial_composition_change/data/Chicago_clean.Rdata')
mosquito_alpha <- bind_rows(mosquito_alpha, alpha_S)
mosquito_gamma <- bind_rows(mosquito_gamma, gamma_S)
mosquito_jknife <- bind_rows(mosquito_jknife, study_jknife)
rm(alpha_S, gamma_S, study_jknife)

load('~/Dropbox/1current/spatial_composition_change/data/florida_clean.Rdata')
# fix column names
alpha_S <- alpha_S %>% 
  rename(plot = region,
         region = Locations)
mosquito_alpha <- bind_rows(mosquito_alpha, alpha_S)
mosquito_gamma <- bind_rows(mosquito_gamma, gamma_S)
mosquito_jknife <- bind_rows(mosquito_jknife, study_jknife)
rm(alpha_S, gamma_S, study_jknife)

load('~/Dropbox/1current/spatial_composition_change/data/idaho-n_dakota-montana_clean.Rdata')
mosquito_alpha <- bind_rows(mosquito_alpha, alpha_S)
mosquito_gamma <- bind_rows(mosquito_gamma, gamma_S)
mosquito_jknife <- bind_rows(mosquito_jknife, study_jknife)
rm(alpha_S, gamma_S, study_jknife)

load('~/Dropbox/1current/spatial_composition_change/data/indiana_clean.Rdata')
mosquito_alpha <- bind_rows(mosquito_alpha, alpha_S)
mosquito_gamma <- bind_rows(mosquito_gamma, gamma_S)
mosquito_jknife <- bind_rows(mosquito_jknife, study_jknife)
rm(alpha_S, gamma_S, study_jknife)

load('~/Dropbox/1current/spatial_composition_change/data/iowa_clean.Rdata')
mosquito_alpha <- bind_rows(mosquito_alpha, alpha_S)
mosquito_gamma <- bind_rows(mosquito_gamma, gamma_S)
# fix the region name for jknife
study_jknife <- study_jknife %>% 
  mutate(region = 'iowa')
mosquito_jknife <- bind_rows(mosquito_jknife, study_jknife)
rm(alpha_S, gamma_S, study_jknife)

load('~/Dropbox/1current/spatial_composition_change/data/virginia_clean.Rdata')
mosquito_alpha <- bind_rows(mosquito_alpha, alpha_S)
mosquito_gamma <- bind_rows(mosquito_gamma, gamma_S)
mosquito_jknife <- bind_rows(mosquito_jknife, study_jknife)
rm(alpha_S, gamma_S, study_jknife)

# want to get the same plots through time
mosquito_alpha %>% 
  ggplot() +
  facet_wrap(~region, scales = 'free') +
  geom_point(aes(x = year, y = plot)) +
  theme(legend.position = 'none')

# these regions need attention
regions2fix <- mosquito_alpha %>% 
  group_by(region, year) %>% 
  summarise(n_plots = n_distinct(plot)) %>% 
  filter(length(unique(n_plots)) > 1) %>% 
  distinct(region)

# visual inspection of the clean data
mosquito_clean <- mosquito_alpha %>% 
  filter(!region %in% regions2fix$region)

mosquito_clean %>% 
  ggplot() +
  facet_wrap(~region, scales = 'free') +
  geom_point(aes(x = year, y = plot)) +
  theme(legend.position = 'none')

# now fix regions one at a time
# idaho-n_dakota-montana: drop sites with missing years
idaho_clean <- mosquito_alpha %>% 
  filter(region == regions2fix$region[1]) %>% 
  group_by(plot) %>% 
  mutate(nyrs = n_distinct(year)) %>% 
  ungroup() %>% 
  filter(nyrs == max(nyrs))

idaho_clean %>% 
  ggplot() +
  facet_wrap(~region, scales = 'free') +
  geom_point(aes(x = year, y = plot)) +
  theme(legend.position = 'none')

# iowa: drop some years and sites
mosquito_alpha %>% 
  filter(region == regions2fix$region[2]) %>% 
  ggplot() +
  facet_wrap(~region, scales = 'free') +
  geom_point(aes(x = year, y = plot)) +
  theme(legend.position = 'none')

iowa_clean <- mosquito_alpha %>% 
  filter(region == regions2fix$region[2]) %>% 
  filter(!year %in% c(1997:2000, 2002)) %>% 
  group_by(plot) %>% 
  mutate(nyrs = n_distinct(year)) %>% 
  ungroup() %>% 
  filter(nyrs == max(nyrs))

iowa_clean %>% 
  ggplot() +
  facet_wrap(~region, scales = 'free') +
  geom_point(aes(x = year, y = plot)) +
  theme(legend.position = 'none')

# Manatee: drop sites not sampled every year
mosquito_alpha %>% 
  filter(region == regions2fix$region[3]) %>% 
  ggplot() +
  facet_wrap(~region, scales = 'free') +
  geom_point(aes(x = year, y = plot)) +
  theme(legend.position = 'none')

manatee_clean <- mosquito_alpha %>% 
  filter(region == regions2fix$region[3]) %>% 
  group_by(plot) %>% 
  mutate(nyrs = n_distinct(year)) %>% 
  ungroup() %>% 
  filter(nyrs == max(nyrs))

manatee_clean %>% 
  ggplot() +
  facet_wrap(~region, scales = 'free') +
  geom_point(aes(x = year, y = plot)) +
  theme(legend.position = 'none')

# virginia: drop sites not sampled every year
mosquito_alpha %>% 
  filter(region == regions2fix$region[4]) %>% 
  ggplot() +
  facet_wrap(~region, scales = 'free') +
  geom_point(aes(x = year, y = plot)) +
  theme(legend.position = 'none')

virginia_clean <- mosquito_alpha %>% 
  filter(region == regions2fix$region[4]) %>% 
  group_by(plot) %>% 
  mutate(nyrs = n_distinct(year)) %>% 
  ungroup() %>% 
  filter(nyrs == max(nyrs))

virginia_clean %>% 
  ggplot() +
  facet_wrap(~region, scales = 'free') +
  geom_point(aes(x = year, y = plot)) +
  theme(legend.position = 'none')

# join with other balanced data
mosquito_clean <- bind_rows(mosquito_clean,
                            idaho_clean,
                            iowa_clean,
                            manatee_clean,
                            virginia_clean) %>% 
  select(-nyrs)

mosquito_clean %>% 
  ggplot() +
  facet_wrap(~region, scales = 'free') +
  geom_point(aes(x = year, y = plot)) +
  theme(legend.position = 'none')

mosquito_alpha_timeSeries <- mosquito_clean
rm(mosquito_alpha, mosquito_clean)

# need to get regional richness for these combinations of region-plot-year
mosq_sites <- mosquito_alpha_timeSeries %>% 
  distinct(region, plot, year)

save(mosquito_alpha_timeSeries,
     mosq_sites,
     file = '~/Dropbox/1current/spatial_composition_change/results/mosquito_alpha_timeSeries.Rdata')

