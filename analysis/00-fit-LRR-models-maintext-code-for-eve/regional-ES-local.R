# local analysis of 'raw' data: log-ratio of change through time
library(tidyverse)
library(brms)



load('~/Dropbox/1current/spatial_composition_change/WhittakerBetaChange/analysis/data/all_meta-new.Rdata')
load('~/Dropbox/1current/spatial_composition_change/WhittakerBetaChange/analysis/data/allLRR_meta-new.Rdata')
# id for regions in time series analyses
load('~/Dropbox/1current/spatial_composition_change/WhittakerBetaChange/analysis/data/ts-regions.Rdata')

regional_jk_summary <- regional_jknife_LRR %>% 
  group_by(regional_level) %>% 
  summarise(ES_gamma = median(ES_gamma),
            ES_gamma_eH = median(ES_gamma_eH),
            ES_gamma_S_PIE = median(ES_gamma_S_PIE),
            dt = unique(dt)) %>% 
  ungroup() %>% 
  left_join(all_meta)

regional_ES_jk_norm_sigma2 <- brm(bf(ES_gamma ~ 1 + (1 | regional_level),
                                        sigma ~ sample_type * logdt),
                                     data = regional_jk_summary %>%
                                       mutate(logdt = log(dt),
                                              regional_level = ifelse(regional_level == 'i_Lee (Florida (United States))',
                                                                      'i_Lee', regional_level)),
                                     prior = c(prior(normal(0,1), class = Intercept),
                                               prior(normal(0,1), class = sd),
                                               prior(normal(0,1), class = Intercept, dpar = sigma),
                                               prior(normal(0,1), class = b, coef = logdt, dpar = sigma)),
                                     control = list(adapt_delta = 0.999),
                                     cores = 8, chains = 8,
                                     backend ='cmdstanr',
                                     iter = 2000, thin = 2)

save(regional_ES_jk_norm_sigma2,
     file = '~/Dropbox/1current/spatial_composition_change/results/model_fits/regional-ES-jk-norm-sigma.Rdata')

# supplementary analysis: fit to data in time series analysis
regional_ES_jk_norm_sigma2_ts <- brm(bf(ES_gamma ~ 1 + (1 | regional_level),
                                        sigma ~ logdt),
                                     data = regional_jk_summary %>%
                                       mutate(logdt = log(dt),
                                              regional_level = ifelse(regional_level == 'i_Lee (Florida (United States))',
                                                                      'i_Lee', regional_level)) %>% 
                                       filter(regional_level %in% ts_regions$regional_level),
                                     prior = c(prior(normal(0,1), class = Intercept),
                                               prior(normal(0,1), class = sd),
                                               prior(normal(0,1), class = Intercept, dpar = sigma),
                                               prior(normal(0,1), class = b, coef = logdt, dpar = sigma)),
                                     control = list(adapt_delta = 0.999),
                                     cores = 8, chains = 8,
                                     backend ='cmdstanr',
                                     iter = 2000, thin = 2)

save(regional_ES_jk_norm_sigma2_ts,
     file = '~/Dropbox/1current/spatial_composition_change/results/model_fits/regional-ES-jk-norm-sigma-ts.Rdata')

# supplementary analysis: fit to data NOT in the time series analysis
regional_ES_jk_norm_sigma2_ts_anti <- brm(bf(ES_gamma ~ 1 + (1 | regional_level),
			   sigma ~ sample_type * logdt),
			   data = regional_jk_summary %>%
			     mutate(logdt = log(dt),
			            regional_level = ifelse(regional_level == 'i_Lee (Florida (United States))',
			                                    'i_Lee', regional_level)) %>% 
			     filter(!regional_level %in% ts_regions$regional_level),
			   prior = c(prior(normal(0,1), class = Intercept),
			             prior(normal(0,1), class = sd),
			             prior(normal(0,1), class = Intercept, dpar = sigma),
			             prior(normal(0,1), class = b, coef = logdt, dpar = sigma)),
			   control = list(adapt_delta = 0.999),
			   cores = 8, chains = 8,
			   backend ='cmdstanr',
			   iter = 2000, thin = 2)

save(regional_ES_jk_norm_sigma2_ts_anti,
     file = '~/Dropbox/1current/spatial_composition_change/results/model_fits/regional-ES-jk-norm-sigma-ts-anti.Rdata')

