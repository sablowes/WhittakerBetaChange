# results from models fit to ES (LR / duration) with covariates
library(brms)
library(tidybayes)
library(tidyverse)
library(cowplot)

load('~/Dropbox/1current/spatial_composition_change/results/model_fits/results/local-es-norm-sampletype-8572831.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/model_fits/results/regional-es-norm-sample-type-8572837.Rdata')

load('~/Dropbox/1current/spatial_composition_change/results/model_fits/results/local-es-norm-realm-8572830.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/model_fits/results/regional-es-norm-realm-8572836.Rdata')

load('~/Dropbox/1current/spatial_composition_change/results/model_fits/results/local-es-norm-taxa-8572832.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/model_fits/results/regional-es-norm-taxa-8572838.Rdata')

load('~/Dropbox/1current/spatial_composition_change/results/model_fits/results/local-es-norm-lat-8572828.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/model_fits/results/regional-es-norm-lat-8572834.Rdata')

load('~/Dropbox/1current/spatial_composition_change/data/all_meta.Rdata')

# checklist vs resurvey
local_sample_type = gather_draws(local_ES_norm_sampletype, `b_.*`, regex = TRUE)
local_sample_type_studyLevel <- spread_draws(local_ES_norm_sampletype, `b_.*`, r_regional_level[regional_level, term], regex = TRUE) %>% 
  rename(local_checklist_intercept = b_sample_typechecklist,
         local_resurvey_intercept = b_sample_typeresurvey,
         local_study_intercept = r_regional_level) %>% 
  left_join(all_meta %>% distinct(regional_level, sample_type))

regional_sample_type = gather_draws(regional_ES_norm_sampletype, `b_sample_.*`, regex = TRUE)
regional_sample_type_studyLevel <- spread_draws(regional_ES_norm_sampletype, `b_sample_.*`, r_regional_level[regional_level, term], regex = TRUE) %>% 
  rename(regional_checklist_intercept = b_sample_typechecklist,
         regional_resurvey_intercept = b_sample_typeresurvey,
         regional_study_intercept = r_regional_level) %>% 
  left_join(all_meta %>% distinct(regional_level, sample_type))

local_sample_type_summary <- local_sample_type %>% 
  group_by(.variable) %>% 
  summarise(local_median = median(.value),
            local_Q95 = quantile(.value, probs = 0.95),
            local_Q05 = quantile(.value, probs = 0.05)) %>% 
  ungroup()

regional_sample_type_summary <- regional_sample_type %>% 
  group_by(.variable) %>% 
  summarise(regional_median = median(.value),
            regional_Q95 = quantile(.value, probs = 0.95),
            regional_Q05 = quantile(.value, probs = 0.05)) %>% 
  ungroup()

sample_type_summary <- left_join(local_sample_type_summary,
                                 regional_sample_type_summary) %>% 
  mutate(.variable = case_when(.variable=='b_sample_typechecklist' ~ 'Checklist',
                               .variable=='b_sample_typeresurvey' ~ 'Resurvey'))

sample_type_label <- all_meta %>% 
  group_by(sample_type) %>% 
  count() %>% 
  mutate(sample_type = case_when(sample_type == 'checklist' ~ 'Checklist',
                                 sample_type == 'resurvey' ~ 'Resurvey'))


# taxon groups
local_taxon = gather_draws(local_ES_norm_taxa, `b_.*`, regex = TRUE)

regional_taxon = gather_draws(regional_ES_norm_taxa, `b_.*`, regex = TRUE)

local_taxon_summary <- local_taxon %>% 
  group_by(.variable) %>% 
  summarise(local_median = median(.value),
            local_Q95 = quantile(.value, probs = 0.95),
            local_Q05 = quantile(.value, probs = 0.05)) %>% 
  ungroup()

regional_taxon_summary <- regional_taxon %>% 
  group_by(.variable) %>% 
  summarise(regional_median = median(.value),
            regional_Q95 = quantile(.value, probs = 0.95),
            regional_Q05 = quantile(.value, probs = 0.05)) %>% 
  ungroup()

taxon_summary <- left_join(local_taxon_summary,
                           regional_taxon_summary) %>% 
  mutate(.variable = case_when(.variable=='b_taxon_modBenthos' ~ 'Benthos',
                               .variable=='b_taxon_modPlants' ~ 'Plants',
                               .variable=='b_taxon_modMammals' ~ 'Mammals',
                               .variable=='b_taxon_modFish' ~ 'Fish',
                               .variable=='b_taxon_modInvertebrates' ~ 'Invertebrates',
                               .variable=='b_taxon_modMultipletaxa' ~ 'Multiple taxa',
                               .variable=='b_taxon_modMarineinvertebratesDplants' ~ 'Marine invertebrates/plants',
                               .variable=='b_taxon_modHerpetofauna' ~ 'Herpetofauna',
                               .variable=='b_taxon_modBirds' ~ 'Birds'))

# taxon group colour scheme
taxon_colour = c('Plants' = '#a6cee3',
                 'Mammals' = '#e31a1c',
                 'Birds' = '#b2df8a',
                 # 'Benthos' = '#33a02c',
                 'Invertebrates' = '#fb9a99',
                 'Fish' = '#1f78b4',
                 'Multiple taxa' = '#fdbf6f',
                 # 'Marine invertebrates/plants' = '#ff7f00',
                 'Herpetofauna' = '#cab2d6')

taxon_label <- all_meta %>% 
  group_by(taxon_mod) %>% 
  count()

# realms
local_realm = gather_draws(local_ES_norm_realm, `b_.*`, regex = TRUE)

regional_realm = gather_draws(regional_ES_norm_realm, `b_.*`, regex = TRUE)

local_realm_summary <- local_realm %>% 
  group_by(.variable) %>% 
  summarise(local_median = median(.value),
            local_Q95 = quantile(.value, probs = 0.95),
            local_Q05 = quantile(.value, probs = 0.05)) %>% 
  ungroup()

regional_realm_summary <- regional_realm %>% 
  group_by(.variable) %>% 
  summarise(regional_median = median(.value),
            regional_Q95 = quantile(.value, probs = 0.95),
            regional_Q05 = quantile(.value, probs = 0.05)) %>% 
  ungroup()

realm_summary <- left_join(local_realm_summary,
                           regional_realm_summary) %>% 
  mutate(.variable = case_when(.variable=='b_realmFreshwater' ~ 'Freshwater',
                               .variable=='b_realmMarine' ~ 'Marine',
                               .variable=='b_realmTerrestrial' ~ 'Terrestrial'))

# realm colour scheme
realm_colour = c('Marine' = '#0a393d',
                 'Freshwater' = '#3a813b',
                 'Terrestrial' = '#ffa600')

realm_label <- all_meta %>% 
  group_by(realm) %>% 
  count()

# latitudinal band (use 4 bands due to preponderance of temperate data)
local_lat4 = gather_draws(local_ES_norm_lat, `b_.*`, regex = TRUE)

regional_lat4 = gather_draws(regional_ES_norm_lat, `b_.*`, regex = TRUE)

local_lat4_summary <- local_lat4 %>% 
  group_by(.variable) %>% 
  summarise(local_median = median(.value),
            local_Q95 = quantile(.value, probs = 0.95),
            local_Q05 = quantile(.value, probs = 0.05)) %>% 
  ungroup()

regional_lat4_summary <- regional_lat4 %>% 
  group_by(.variable) %>% 
  summarise(regional_median = median(.value),
            regional_Q95 = quantile(.value, probs = 0.95),
            regional_Q05 = quantile(.value, probs = 0.05)) %>% 
  ungroup()

lat4_summary <- left_join(local_lat4_summary,
                          regional_lat4_summary) %>% 
  mutate(.variable = case_when(.variable=='b_lat_band_4polar' ~ 'Polar',
                               .variable=='b_lat_band_4subtropical' ~ 'Subtropical',
                               .variable=='b_lat_band_4temperate' ~ 'Temperate',
                               .variable=='b_lat_band_4tropical' ~ 'Tropical'))

# lat4 colour scheme
lat4_colour = c('Polar' = '#ffd7ff',
                'Temperate' = '#e198ef',
                'Subtropical' = '#b959e2',
                'Tropical' = '#8200d9')

lat4_label <- all_meta %>% 
  group_by(lat_band_4) %>% 
  count() %>% 
  mutate(lat_band_4 = case_when(lat_band_4=='polar' ~ 'Polar',
                                lat_band_4=='temperate' ~ 'Temperate',
                                lat_band_4=='subtropical' ~ 'Subtropical',
                                lat_band_4=='tropical' ~ 'Tropical'))

