library(tidyverse)
library(brms)

load('~/Dropbox/1current/spatial_composition_change/results/two-scale-time-series-plus-meta.Rdata')
# need to remove these regions before fitting models
load('~/Dropbox/1current/spatial_composition_change/results/regions2remove.Rdata')

gamma_ts <- gamma_ts %>% 
  mutate(cyear = year - mean(year)) %>% 
  filter(!regional_level %in% regions2remove$regional_level)

m.gammaS <- brm(S ~ cyear + (cyear | regional_level),
                family = lognormal(),
                data = gamma_ts,
                prior = c(prior(normal(3,1), class = Intercept),
                          prior(normal(0,1), class = b),
                          prior(normal(0,1), class = sd),
                          prior(normal(0,2), class = sigma)),
                iter = 4000, warmup = 1000, thin = 6,
                backend = 'cmdstanr',
                cores = 8, chains = 8)

save(m.gammaS, 
     file = '~/Dropbox/1current/spatial_composition_change/results/model_fits/gammaS-ts-model.Rdata')

m.gamma.S_PIE <- brm(S_PIE ~ cyear + (cyear | regional_level),
                  family = lognormal(),
                  data = gamma_ts,
                  prior = c(prior(normal(2,1), class = Intercept),
                            prior(normal(0,1), class = b),
                            prior(normal(0,1), class = sd),
                            prior(normal(0,2), class = sigma)),
                  iter = 4000, warmup = 1000, thin = 6,
                  backend = 'cmdstanr',
                  cores = 8, chains = 8)



save(m.gamma.S_PIE, 
     file = '~/Dropbox/1current/spatial_composition_change/results/model_fits/gamma-S_PIE-ts-model.Rdata')

# preliminary visual inspection of model fit
pp_check(m.gammaS) + 
  scale_x_continuous(trans = 'log2')
pp_check(m.gamma.S_PIE) +
  scale_x_continuous(trans = 'log2')


