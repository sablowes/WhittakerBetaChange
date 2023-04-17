# compile time series to fit  models for beta-diversity metrics directly

rm(list=ls())
library(tidyverse)


load('~/Dropbox/1current/spatial_composition_change/results/bt_timeSeries_metrics-new.Rdata.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/rft_metric-time-series.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/resurvey_timeSeries_metrics.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/invert_metric-time-series.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/mosquito_metric_timeSeries.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/enc_timeSeries.Rdata')

# before joining, wrangle each of the data sources separately
# BioTIME
bt_betaC <- bt_metrics %>% 
  unnest(S_c) %>% 
  rename(betaC = beta,
         alphaC = alpha_value,
         gammaC = gamma_value) %>% 
  select(STUDY_ID, YEAR, betaC, alphaC, gammaC)

# get the region-year combos (some will have failed due to coverage calcuations,
# e.g., no singletons or doubletons)
ry <- bt_betaC %>% 
  distinct(STUDY_ID, YEAR) %>% 
  unite(ry, c(STUDY_ID, YEAR))

bt_betaS <- bt_metrics %>% 
  unnest(S) %>% 
  filter(scale == 'beta') %>% 
  unite(ry2, c(STUDY_ID, YEAR), remove = FALSE) %>% 
  filter(ry2 %in% ry$ry) %>% 
  select(-ry2) %>% 
  select(STUDY_ID, YEAR, value) %>% 
  rename(betaS = value)

bt_betaS_PIE <- bt_metrics %>% 
  unnest(S_PIE) %>% 
  filter(scale == 'beta') %>% 
  unite(ry2, c(STUDY_ID, YEAR), remove = FALSE) %>% 
  filter(ry2 %in% ry$ry) %>% 
  select(-ry2) %>% 
  select(STUDY_ID, YEAR, value) %>% 
  rename(betaS_PIE = value)

# combine (note that betaS_PIE is the least complete)
# will also need to confirm we have time series before modelling
bt_beta <- left_join(bt_betaC,
                     bt_betaS) %>% 
  left_join(bt_betaS_PIE) %>% 
  mutate(regional_level = paste0('bt_', STUDY_ID),
         database = 'BioTIME') %>% 
  rename(year = YEAR) %>% 
  select(-STUDY_ID) %>% 
  group_by(regional_level) %>% 
  filter(n_distinct(year) > 1) %>% 
  ungroup()

# RivFishTime
rft_betaC <- rft_metrics %>% 
  ungroup() %>% 
  unnest(S_c) %>% 
  rename(betaC = beta,
         alphaC = alpha_value,
         gammaC = gamma_value) %>% 
  select(SourceID, Year, betaC, alphaC, gammaC)

# get the region-year combos (some will have failed due to coverage calcuations,
# e.g., no singletons or doubletons)
ry <- rft_betaC %>% 
  distinct(SourceID, Year) %>% 
  unite(ry, c(SourceID, Year))

rft_betaS <- rft_metrics %>% 
  ungroup() %>% 
  unnest(S) %>% 
  filter(scale == 'beta') %>% 
  unite(ry2, c(SourceID, Year), remove = FALSE) %>% 
  filter(ry2 %in% ry$ry) %>% 
  select(-ry2) %>% 
  select(SourceID, Year, value) %>% 
  rename(betaS = value)

rft_betaS_PIE <- rft_metrics %>% 
  ungroup() %>% 
  unnest(S_PIE) %>% 
  filter(scale == 'beta') %>% 
  unite(ry2, c(SourceID, Year), remove = FALSE) %>% 
  filter(ry2 %in% ry$ry) %>% 
  select(-ry2) %>% 
  select(SourceID, Year, value) %>% 
  rename(betaS_PIE = value)

# combine (note that betaS_PIE is the least complete)
# will also need to confirm we have time series before modelling
rft_beta <- left_join(rft_betaC,
                     rft_betaS) %>% 
  left_join(rft_betaS_PIE) %>% 
  mutate(regional_level = as.character(SourceID),
         database = 'RivFishTime') %>% 
  rename(year = Year) %>% 
  select(-SourceID) %>% 
  group_by(regional_level) %>% 
  filter(n_distinct(year) > 1) %>% 
  ungroup() %>% 
  mutate(regional_level = paste0('rft_', regional_level))

# metacommunity resurvey
resurvey_betaC <- resurvey_metrics %>% 
  unnest(S_c) %>% 
  rename(betaC = beta,
         alphaC = alpha_value,
         gammaC = gamma_value) %>% 
  select(regional_level, year, betaC, alphaC, gammaC)

# get the region-year combos (some will have failed due to coverage calcuations,
# e.g., no singletons or doubletons)
ry <- resurvey_betaC %>% 
  distinct(regional_level, year) %>% 
  unite(ry, c(regional_level, year))

resurvey_betaS <- resurvey_metrics %>% 
  unnest(S) %>% 
  filter(scale == 'beta') %>% 
  unite(ry2, c(regional_level, year), remove = FALSE) %>% 
  filter(ry2 %in% ry$ry) %>% 
  select(-ry2) %>% 
  select(regional_level, year, value) %>% 
  rename(betaS = value)

resurvey_betaS_PIE <- resurvey_metrics %>% 
  unnest(S_PIE) %>% 
  filter(scale == 'beta') %>% 
  unite(ry2, c(regional_level, year), remove = FALSE) %>% 
  filter(ry2 %in% ry$ry) %>% 
  select(-ry2) %>% 
  select(regional_level, year, value) %>% 
  rename(betaS_PIE = value)

# combine (note that betaS_PIE is the least complete)
# will also need to confirm we have time series before modelling
resurvey_beta <- left_join(resurvey_betaC,
                           resurvey_betaS) %>% 
  left_join(resurvey_betaS_PIE) %>% 
  mutate(database = 'Resurvey') %>% 
  group_by(regional_level) %>% 
  filter(n_distinct(year) > 1) %>% 
  ungroup()

# there are some duplicates between Roel's data and the data that Alban has compiled
dupes <- c('countryside_survey_plants_2017_England', 'magnuson_2020_North Temperate Lakes', 
           'schuch_2011_Lower Saxony', 'valtonen_2018_Hungary')

resurvey_beta <- resurvey_beta %>% 
  filter(!regional_level %in% dupes)

# tidy up regional levels in Resurvey database
rs_rl <- resurvey_alpha %>% 
  distinct(regional_level) %>% 
  mutate(rl2 = paste0('resurvey_', 1:n()))

resurvey_beta <- left_join(resurvey_beta,
          rs_rl) %>% 
  select(-regional_level) %>% 
  rename(regional_level = rl2)

# invertebrate
invert_betaC <- invert_metrics %>% 
  unnest(S_c) %>% 
  rename(betaC = beta,
         alphaC = alpha_value,
         gammaC = gamma_value) %>% 
  select(Datasource_ID, Year, betaC, alphaC, gammaC)

# get the region-year combos (some will have failed due to coverage calcuations,
# e.g., no singletons or doubletons)
ry <- invert_betaC %>% 
  distinct(Datasource_ID, Year) %>% 
  unite(ry, c(Datasource_ID, Year))

invert_betaS <- invert_metrics %>% 
  unnest(S) %>% 
  filter(scale == 'beta') %>% 
  unite(ry2, c(Datasource_ID, Year), remove = FALSE) %>% 
  filter(ry2 %in% ry$ry) %>% 
  select(-ry2) %>% 
  select(Datasource_ID, Year, value) %>% 
  rename(betaS = value)

invert_betaS_PIE <- invert_metrics %>% 
  unnest(S_PIE) %>% 
  filter(scale == 'beta') %>% 
  unite(ry2, c(Datasource_ID, Year), remove = FALSE) %>% 
  filter(ry2 %in% ry$ry) %>% 
  select(-ry2) %>% 
  select(Datasource_ID, Year, value) %>% 
  rename(betaS_PIE = value)

# combine (note that betaS_PIE is the least complete)
# will also need to confirm we have time series before modelling
invert_beta <- left_join(invert_betaC,
                         invert_betaS) %>% 
  left_join(invert_betaS_PIE) %>% 
  mutate(regional_level = as.character(Datasource_ID),
         database = 'Invertebrates') %>% 
  rename(year = Year) %>% 
  select(-Datasource_ID) %>% 
  group_by(regional_level) %>% 
  filter(n_distinct(year) > 1) %>% 
  ungroup() %>% 
  mutate(regional_level = paste0('i_', regional_level))

# mosquito (these data required sample-based rarefaction, so we need to average)
mosq_betaC <- mosq_metrics %>% 
  unnest(S_c) %>% 
  rename(betaC = beta,
         alphaC = alpha_value,
         gammaC = gamma_value) %>% 
  select(region, year, betaC, alphaC, gammaC) %>% 
  group_by(region, year) %>% 
  summarise(betaC = median(betaC),
            alphaC = median(alphaC),
            gammaC = median(gammaC)) %>% 
  ungroup()

# get the region-year combos (some will have failed due to coverage calcuations,
# e.g., no singletons or doubletons)
ry <- mosq_betaC %>% 
  distinct(region, year) %>% 
  unite(ry, c(region, year))

mosq_betaS <- mosq_metrics %>% 
  unnest(S) %>% 
  filter(scale == 'beta') %>% 
  unite(ry2, c(region, year), remove = FALSE) %>% 
  filter(ry2 %in% ry$ry) %>% 
  select(-ry2) %>% 
  select(region, year, value) %>% 
  rename(betaS = value) %>% 
  group_by(region, year) %>% 
  summarise(betaS = median(betaS)) %>% 
  ungroup()

mosq_betaS_PIE <- mosq_metrics %>% 
  unnest(S_PIE) %>% 
  filter(scale == 'beta') %>% 
  unite(ry2, c(region, year), remove = FALSE) %>% 
  filter(ry2 %in% ry$ry) %>% 
  select(-ry2) %>% 
  select(region, year, value) %>% 
  rename(betaS_PIE = value) %>% 
  group_by(region, year) %>% 
  summarise(betaS_PIE = median(betaS_PIE)) %>% 
  ungroup()

# combine (note that betaS_PIE is the least complete)
# will also need to confirm we have time series before modelling
mosq_beta <- left_join(mosq_betaC,
                       mosq_betaS) %>% 
  left_join(mosq_betaS_PIE) %>% 
  mutate(regional_level = as.character(region),
         database = 'Invertebrates') %>% 
  select(-region) %>% 
  group_by(regional_level) %>% 
  filter(n_distinct(year) > 1) %>% 
  ungroup() %>% 
  mutate(regional_level = paste0('i_', regional_level),
         regional_level = ifelse(regional_level=='i_Lee (Florida (United States))',
                                 'i_Lee', regional_level))

# enc data (also required sample-based rarefaction, so averaging required)
enc_betaC <- enc_metrics %>% 
  unnest(S_c) %>% 
  rename(betaC = beta,
         alphaC = alpha_value,
         gammaC = gamma_value) %>% 
  select(region, year, betaC, alphaC, gammaC) %>% 
  group_by(region, year) %>% 
  summarise(betaC = median(betaC),
            alphaC = median(alphaC),
            gammaC = median(gammaC)) %>% 
  ungroup()

# get the region-year combos (some will have failed due to coverage calcuations,
# e.g., no singletons or doubletons)
ry <- enc_betaC %>% 
  distinct(region, year) %>% 
  unite(ry, c(region, year))

enc_betaS <- enc_metrics %>% 
  unnest(S) %>% 
  filter(scale == 'beta') %>% 
  unite(ry2, c(region, year), remove = FALSE) %>% 
  filter(ry2 %in% ry$ry) %>% 
  select(-ry2) %>% 
  select(region, year, value) %>% 
  rename(betaS = value) %>% 
  group_by(region, year) %>% 
  summarise(betaS = median(betaS)) %>% 
  ungroup()

enc_betaS_PIE <- enc_metrics %>% 
  unnest(S_PIE) %>% 
  filter(scale == 'beta') %>% 
  unite(ry2, c(region, year), remove = FALSE) %>% 
  filter(ry2 %in% ry$ry) %>% 
  select(-ry2) %>% 
  select(region, year, value) %>% 
  rename(betaS_PIE = value) %>% 
  group_by(region, year) %>% 
  summarise(betaS_PIE = median(betaS_PIE)) %>% 
  ungroup()

# combine (note that betaS_PIE is the least complete)
# will also need to confirm we have time series before modelling
enc_beta <- left_join(enc_betaC,
                      enc_betaS) %>% 
  left_join(enc_betaS_PIE) %>% 
  mutate(regional_level = as.character(region),
         database = 'Invertebrates') %>% 
  select(-region) %>% 
  group_by(regional_level) %>% 
  filter(n_distinct(year) > 1) %>% 
  ungroup() %>% 
  mutate(regional_level = paste0('i_', regional_level))


all_beta <- bind_rows(bt_beta,
                      rft_beta,
                      resurvey_beta,
                      invert_beta,
                      mosq_beta,
                      enc_beta)

save(all_beta,
     file = '~/Dropbox/1current/spatial_composition_change/results/beta_timeSeries.Rdata')

ggplot() +
  # facet_wrap(~regional_level, scales = 'free') +
  stat_smooth(data = all_beta,
              aes(x = year, y = betaC),
              method = 'lm', colour = 'red') +
  stat_smooth(data = all_beta,
              aes(x = year, y = betaC, group = regional_level),
              method = 'lm', se = F, colour = 'red', size = 0.5) +
  # stat_smooth(data = all_beta,
  #             aes(x = year, y = betaS),
  #             method = 'lm', colour = 'black') +
  # stat_smooth(data = all_beta,
  #             aes(x = year, y = betaS, group = regional_level),
  #             method = 'lm', se = F, colour = 'black', size = 0.5) +
  # stat_smooth(data = all_beta,
  #             aes(x = year, y = betaS_PIE),
  #             method = 'lm', colour = 'orange') +
  # stat_smooth(data = all_beta,
  #             aes(x = year, y = betaS_PIE, group = regional_level),
  #             method = 'lm', colour = 'orange', se = F, size = 0.5) +
  scale_y_continuous(trans = 'log2')
  

load('~/Dropbox/1current/spatial_composition_change/data/ts_meta.Rdata')

ts_meta %>% filter(is.na(realm))
all_beta %>% distinct(regional_level)



all_beta %>% 
  left_join(ts_meta) %>% 
  ggplot() +
  facet_wrap(~taxon_mod, scales = 'free') +
  stat_smooth(aes(x = year, y = betaC),
              method = 'lm', colour = 'red') +
  stat_smooth(aes(x = year, y = betaC, group = regional_level),
              method = 'lm', se = F, colour = 'red', size = 0.5) +
  stat_smooth(aes(x = year, y = betaS),
              method = 'lm', colour = 'black') +
  stat_smooth(aes(x = year, y = betaS, group = regional_level),
              method = 'lm', se = F, colour = 'black', size = 0.5) +
scale_y_continuous(trans = 'log2')


# visual inspection of time series
r <- all_beta %>% 
  distinct(regional_level) %>% 
  pull()

pdf('~/Dropbox/1current/spatial_composition_change/figures/data-visualisation/beta-ts-inspection.pdf', width = 12, height = 9)

for(i in 1:length(r)){
  print(paste('region', i, 'of', length(r), 'regions'))
  
  p = ggplot() +
    geom_point(data = all_beta %>% 
                 filter(regional_level==r[i]),
               aes(x = year, y = betaC), colour = 'red') +
    geom_point(data = all_beta %>% 
                 filter(regional_level==r[i]),
               aes(x = year, y = betaS), colour = 'black') +
    stat_smooth(data = all_beta %>% 
                  filter(regional_level==r[i]),
                aes(x = year, y = betaC),
                method = 'lm', colour = 'red') +
    stat_smooth(data = all_beta %>% 
                  filter(regional_level==r[i]),
                aes(x = year, y = betaS),
                method = 'lm', colour = 'black') +
    scale_y_continuous(trans = 'log2', name = 'beta-diversity') +
    labs(subtitle = paste0(r[i])) +
    theme(legend.position = 'none')
  
  
  print(p)
}
dev.off()

