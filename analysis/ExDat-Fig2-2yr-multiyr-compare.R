# results from simplest two-stage analysis: overall intercept-only model fit to the log-ratio / duration (ES)
library(brms)
library(tidybayes)
library(tidyverse)
library(cowplot)

# main results
load('~/Dropbox/1current/spatial_composition_change/results/model_fits/results/local-es-norm-8572827.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/model_fits/local-es-norm-multi-9015552.Rdata')

load('~/Dropbox/1current/spatial_composition_change/results/model_fits/results/regional-es-norm-8572833.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/model_fits/regional-es-norm-multi-9052159.Rdata')

# sample type
load('~/Dropbox/1current/spatial_composition_change/results/model_fits/results/local-es-norm-sampletype-8572831.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/model_fits/local-es-norm-sampletype-multi-9015562.Rdata')

load('~/Dropbox/1current/spatial_composition_change/results/model_fits/results/regional-es-norm-sample-type-8572837.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/model_fits/regional-es-norm-sample-type-multi-9052164.Rdata')

# realm
load('~/Dropbox/1current/spatial_composition_change/results/model_fits/results/local-es-norm-realm-8572830.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/model_fits/local-es-norm-realm-multi-9015561.Rdata')

load('~/Dropbox/1current/spatial_composition_change/results/model_fits/results/regional-es-norm-realm-8572836.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/model_fits/regional-es-norm-realm-multi-9052163.Rdata')

# latitude
load('~/Dropbox/1current/spatial_composition_change/results/model_fits/results/local-es-norm-lat-8572828.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/model_fits/local-es-norm-lat-multi-9015553.Rdata')

load('~/Dropbox/1current/spatial_composition_change/results/model_fits/results/regional-es-norm-lat-8572834.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/model_fits/regional-es-norm-lat-multi-9052160.Rdata')

# taxon groups
load('~/Dropbox/1current/spatial_composition_change/results/model_fits/results/local-es-norm-taxa-8572832.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/model_fits/local-es-norm-taxa-multi-9015563.Rdata')

load('~/Dropbox/1current/spatial_composition_change/results/model_fits/results/regional-es-norm-taxa-8572838.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/model_fits/regional-es-norm-taxa-multi-9052165.Rdata')



# want to compare all three scales: local (alpha), regional (gamma) and beta
# distance to line function for beta-scale
# x is local scale, y is regional scale, dist is beta scale
source('~/Dropbox/1current/spatial_composition_change/WhittakerBetaChange/analysis/dist2line.R')

main <- bind_cols(
  bind_cols(gather_draws(local_ES_norm, `b_.*`, regex = TRUE) %>%
              ungroup() %>%
              select(xvar = .variable,
                     x = .value),
            gather_draws(regional_ES_norm, `b_.*`, regex = TRUE) %>%
              ungroup() %>%
              select(yvar = .variable, 
                     y = .value)) %>%
    filter(xvar == yvar) %>% 
    select(variable = xvar, x, y,
           -yvar) %>% 
    mutate(d = dist2line(x, y)),
  bind_cols(gather_draws(local_ES_norm_multi, `b_.*`, regex = TRUE) %>%
              ungroup() %>%
              select(xvar = .variable,
                     xmulti = .value),
            gather_draws(regional_ES_norm_multi, `b_.*`, regex = TRUE) %>%
              ungroup() %>%
              select(yvar = .variable, 
                     ymulti = .value)) %>%
    filter(xvar == yvar) %>% 
    select(variable2 = xvar, xmulti, ymulti,
           -yvar) %>% 
    mutate(dmulti = dist2line(xmulti, ymulti))
) %>% 
  filter(variable==variable2)


sampletype <- bind_cols(
  bind_cols(gather_draws(local_ES_norm_sampletype, `b_.*`, regex = TRUE) %>%
                          ungroup() %>%
                          select(xvar = .variable,
                                 x = .value),
                        gather_draws(regional_ES_norm_sampletype, `b_.*`, regex = TRUE) %>%
                          ungroup() %>%
                          select(yvar = .variable, 
                                 y = .value)) %>%
  filter(xvar == yvar) %>% 
  select(variable = xvar, x, y,
         -yvar) %>% 
  mutate(d = dist2line(x, y),
         variable = case_when(variable=='b_sample_typechecklist' ~ 'Checklist',
                              variable!='b_sample_typechecklist' ~ 'Resurvey')),
  bind_cols(gather_draws(local_ES_norm_sampletype_multi, `b_.*`, regex = TRUE) %>%
                               ungroup() %>%
                               select(xvar = .variable,
                                      xmulti = .value),
                             gather_draws(regional_ES_norm_sampletype_multi, `b_.*`, regex = TRUE) %>%
                               ungroup() %>%
                               select(yvar = .variable, 
                                      ymulti = .value)) %>%
  filter(xvar == yvar) %>% 
  select(variable2 = xvar, xmulti, ymulti,
         -yvar) %>% 
  mutate(dmulti = dist2line(xmulti, ymulti),
         variable2 = case_when(variable2=='b_sample_typechecklist' ~ 'Checklist',
                              variable2!='b_sample_typechecklist' ~ 'Resurvey'))
) %>% 
  filter(variable==variable2)


realm <- bind_cols(
  bind_cols(gather_draws(local_ES_norm_realm, `b_.*`, regex = TRUE) %>%
              ungroup() %>%
              select(xvar = .variable,
                     x = .value),
            gather_draws(regional_ES_norm_realm, `b_.*`, regex = TRUE) %>%
              ungroup() %>%
              select(yvar = .variable, 
                     y = .value)) %>%
    filter(xvar == yvar) %>% 
    select(variable = xvar, x, y,
           -yvar) %>% 
    mutate(d = dist2line(x, y),
           variable = case_when(variable=='b_realmFreshwater' ~ 'Freshwater',
                                variable=='b_realmMarine' ~ 'Marine',
                                variable=='b_realmTerrestrial' ~ 'Terrestrial')),
  bind_cols(gather_draws(local_ES_norm_realm_multi, `b_.*`, regex = TRUE) %>%
              ungroup() %>%
              select(xvar = .variable,
                     xmulti = .value),
            gather_draws(regional_ES_norm_realm_multi, `b_.*`, regex = TRUE) %>%
              ungroup() %>%
              select(yvar = .variable, 
                     ymulti = .value)) %>%
    filter(xvar == yvar) %>% 
    select(variable2 = xvar, xmulti, ymulti,
           -yvar) %>% 
    mutate(dmulti = dist2line(xmulti, ymulti),
           variable2 = case_when(variable2=='b_realmFreshwater' ~ 'Freshwater',
                                variable2=='b_realmMarine' ~ 'Marine',
                                variable2=='b_realmTerrestrial' ~ 'Terrestrial'))
) %>% 
  filter(variable==variable2)

lat <- bind_cols(
  bind_cols(gather_draws(local_ES_norm_lat, `b_.*`, regex = TRUE) %>%
              ungroup() %>%
              select(xvar = .variable,
                     x = .value),
            gather_draws(regional_ES_norm_lat, `b_.*`, regex = TRUE) %>%
              ungroup() %>%
              select(yvar = .variable, 
                     y = .value)) %>%
    filter(xvar == yvar) %>% 
    select(variable = xvar, x, y,
           -yvar) %>% 
    mutate(d = dist2line(x, y),
           variable = case_when(variable=='b_lat_band_4polar' ~ 'Polar',
                                variable=='b_lat_band_4subtropical' ~ 'Subtropical',
                                variable=='b_lat_band_4temperate' ~ 'Temperate',
                                variable=='b_lat_band_4tropical' ~ 'Tropical')),
  bind_cols(gather_draws(local_ES_norm_lat_multi, `b_.*`, regex = TRUE) %>%
              ungroup() %>%
              select(xvar = .variable,
                     xmulti = .value),
            gather_draws(regional_ES_norm_lat_multi, `b_.*`, regex = TRUE) %>%
              ungroup() %>%
              select(yvar = .variable, 
                     ymulti = .value)) %>%
    filter(xvar == yvar) %>% 
    select(variable2 = xvar, xmulti, ymulti,
           -yvar) %>% 
    mutate(dmulti = dist2line(xmulti, ymulti),
           variable2 = case_when(variable2=='b_lat_band_4polar' ~ 'Polar',
                                variable2=='b_lat_band_4subtropical' ~ 'Subtropical',
                                variable2=='b_lat_band_4temperate' ~ 'Temperate',
                                variable2=='b_lat_band_4tropical' ~ 'Tropical'))
) %>% 
  filter(variable==variable2)

taxon <- bind_cols(
  bind_cols(gather_draws(local_ES_norm_taxa, `b_.*`, regex = TRUE) %>%
              ungroup() %>%
              select(xvar = .variable,
                     x = .value),
            gather_draws(regional_ES_norm_taxa, `b_.*`, regex = TRUE) %>%
              ungroup() %>%
              select(yvar = .variable, 
                     y = .value)) %>%
    filter(xvar == yvar) %>% 
    select(variable = xvar, x, y,
           -yvar) %>% 
    mutate(d = dist2line(x, y),
           variable = case_when(variable=='b_taxon_modBenthos' ~ 'Benthos',
                                variable=='b_taxon_modBirds' ~ 'Birds',
                                variable=='b_taxon_modFish' ~ 'Fish',
                                variable=='b_taxon_modHerpetofauna' ~ 'Herpetofauna',
                                variable=='b_taxon_modInvertebrates' ~ 'Invertebrates',
                                variable=='b_taxon_modMammals' ~ 'Mammals',
                                variable=='b_taxon_modMarineinvertebratesDplants' ~ 'Marine invertebrates/plants',
                                variable=='b_taxon_modMultipletaxa' ~ 'Multiple taxa',
                                variable=='b_taxon_modPlants' ~ 'Plants')),
  bind_cols(gather_draws(local_ES_norm_taxa_multi, `b_.*`, regex = TRUE) %>%
              ungroup() %>%
              select(xvar = .variable,
                     xmulti = .value),
            gather_draws(regional_ES_norm_taxa_multi, `b_.*`, regex = TRUE) %>%
              ungroup() %>%
              select(yvar = .variable, 
                     ymulti = .value)) %>%
    filter(xvar == yvar) %>% 
    select(variable2 = xvar, xmulti, ymulti,
           -yvar) %>% 
    mutate(dmulti = dist2line(xmulti, ymulti),
           variable2 = case_when(variable2=='b_taxon_modBenthos' ~ 'Benthos',
                                variable2=='b_taxon_modBirds' ~ 'Birds',
                                variable2=='b_taxon_modFish' ~ 'Fish',
                                variable2=='b_taxon_modHerpetofauna' ~ 'Herpetofauna',
                                variable2=='b_taxon_modInvertebrates' ~ 'Invertebrates',
                                variable2=='b_taxon_modMammals' ~ 'Mammals',
                                variable2=='b_taxon_modMarineinvertebratesDplants' ~ 'Marine invertebrates/plants',
                                variable2=='b_taxon_modMultipletaxa' ~ 'Multiple taxa',
                                variable2=='b_taxon_modPlants' ~ 'Plants'))
) %>% 
  filter(variable==variable2)


# want plot showing relationships between parameter estimates from the models fit to the different data
compare_coefs <- bind_rows(main,
          sampletype,
          realm,
          lat, 
          taxon) %>% 
  group_by(variable) %>% 
  summarise(two_year_local = median(x),
            lower_2y_local = quantile(x, probs = 0.05),
            upper_2y_local = quantile(x, probs = 0.95),
            two_year_regional = median(y),
            lower_2y_regional = quantile(y, probs = 0.05),
            upper_2y_regional = quantile(y, probs = 0.95),
            two_year_beta = median(d),
            lower_2y_beta = quantile(d, probs = 0.05),
            upper_2y_beta = quantile(d, probs = 0.95),
            # multi-yr data
            multi_year_local = median(xmulti),
            lower_multi_local = quantile(xmulti, probs = 0.05),
            upper_multi_local = quantile(xmulti, probs = 0.95),
            multi_year_regional = median(ymulti),
            lower_multi_regional = quantile(ymulti, probs = 0.05),
            upper_multi_regional = quantile(ymulti, probs = 0.95),
            multi_year_beta = median(dmulti),
            lower_multi_beta = quantile(dmulti, probs = 0.05),
            upper_multi_beta = quantile(dmulti, probs = 0.95)) %>% 
  ungroup() %>% 
  mutate(model = case_when(variable=='b_Intercept' ~ 'Main result',
                           (variable=='Checklist' | variable=='Resurvey') ~ 'Sample type',
                           (variable=='Freshwater' | variable=='Terrestrial' | variable=='Marine') ~ 'Realm',
                           (variable=='Subtropical' | variable=='Temperate' |
                              variable=='Tropical' | variable=='Polar') ~ 'Latitude',
                           (variable=='Birds' | variable=='Fish' |
                              variable=='Herpetofauna' | variable=='Invertebrates' |
                              variable=='Mammals' | variable=='Plants') ~ 'Taxon group'))

# order factors for clarity
compare_coefs$model <- factor(compare_coefs$model,
                              levels = c('Main result',
                                         'Realm',
                                         'Latitude',
                                         'Sample type',
                                         'Taxon group'),
                              labels = c('a. Main result',
                                         'b. Realm',
                                         'c. Latitude',
                                         'd. Sample type',
                                         'e. Taxon group'))

compare_coefs$variable <- factor(compare_coefs$variable,
                              levels = c('b_Intercept',
                                         'Marine', 'Terrestrial', 'Freshwater',
                                         'Polar', 'Temperate', 'Subtropical', 'Tropical',
                                         'Checklist', 'Resurvey',
                                         'Invertebrates', 'Fish', 'Birds',
                                         'Plants', 'Herpetofauna', 'Mammals'),
                              labels = c('Intercept',
                                         'Marine', 'Terrestrial', 'Freshwater',
                                         'Polar', 'Temperate', 'Subtropical', 'Tropical',
                                         'Checklist', 'Resurvey',
                                         'Invertebrates', 'Fish', 'Birds',
                                         'Plants', 'Herpetofauna', 'Mammals'))

# set the colours
colour_scheme = c('Intercept' = '#000000',
                  'Marine' = '#0a393d',
                  'Freshwater' = '#3a813b',
                  'Terrestrial' = '#ffa600',
                  'Polar' = '#7aeeff',
                  'Temperate' = '#4fb5d0',
                  'Subtropical' = '#297e9f',
                  'Tropical' = '#004c6d',
                  'Checklist' = '#2c2c2c',
                  'Resurvey' = '#b0b0b0',
                  'Plants' = '#a6cee3',
                  'Mammals' = '#e31a1c',
                  'Birds' = '#b2df8a',
                  'Invertebrates' = '#fb9a99',
                  'Fish' = '#1f78b4',
                  'Herpetofauna' = '#cab2d6')


ggplot() +
  facet_wrap(~model, scales = 'free') +
  geom_point(data = compare_coefs,
             aes(x = multi_year_local, y = two_year_local, colour = variable, shape = 'alpha-scale'),
             size = 2) +
  geom_linerange(data = compare_coefs,
                 aes(x = multi_year_local, ymin = lower_2y_local, ymax = upper_2y_local,
                     colour = variable),
                 alpha = 0.5) +
  geom_linerange(data = compare_coefs,
                 aes(y = two_year_local, xmin = lower_multi_local, xmax = upper_multi_local,
                     colour = variable),
                 alpha = 0.5) +
  geom_point(data = compare_coefs,
             aes(x = multi_year_regional, y = two_year_regional, colour = variable, shape = 'gamma-scale'),
             size = 2) +
  geom_linerange(data = compare_coefs,
                 aes(x = multi_year_regional, ymin = lower_2y_regional, ymax = upper_2y_regional,
                     colour = variable),
                 alpha = 0.5) +
  geom_linerange(data = compare_coefs,
                 aes(y = two_year_regional, xmin = lower_multi_regional, xmax = upper_multi_regional,
                     colour = variable),
                 alpha = 0.5) +
  geom_point(data = compare_coefs,
             aes(x = multi_year_beta, y = two_year_beta, colour = variable, shape = 'beta-scale'),
             size = 2) +
  geom_linerange(data = compare_coefs,
                 aes(x = multi_year_beta, ymin = lower_2y_beta, ymax = upper_2y_beta,
                     colour = variable),
                 alpha = 0.5) +
  geom_linerange(data = compare_coefs,
                 aes(y = two_year_beta, xmin = lower_multi_beta, xmax = upper_multi_beta,
                     colour = variable),
                 alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  scale_shape_manual(name = 'Scale',
                     values = c('alpha-scale' = 15,
                                'gamma-scale' = 16,
                                'beta-scale' = 17),
                     labels = c(expression(paste(alpha, '- scale')),
                                expression(paste(gamma, '- scale')),
                                expression(paste(beta, '- scale')))) +
  scale_colour_manual(name = '', values = colour_scheme) +
  labs(x = 'Change estimated with multiple years\nin first and second period',
       y = 'Change estimated with a single year\n(first and last)') +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        strip.text = element_text(hjust = 0))

ggsave('~/Dropbox/MoBD (Measurements of Beta diversity)/Homogenization Paper/MS-Nature/figures/ExDat-Fig2.pdf',
       width = 270, 
       height = 200, 
       units = 'mm')
