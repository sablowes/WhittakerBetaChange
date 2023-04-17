library(tidyverse)
library(brms)

# rstan problem solver
options(buildtools.check = function(action) TRUE)

load('~/Dropbox/1current/spatial_composition_change/results/all_timeSeries.Rdata')

gamma_ts <- gamma_ts %>% 
  mutate(cyear = year - mean(year))

m.gammaS <- brm(S ~ cyear + (cyear | regional_level),
                family = lognormal(),
                data = gamma_ts,
                prior = c(prior(normal(3,1), class = Intercept),
                          prior(normal(0,1), class = b),
                          prior(normal(0,1), class = sd),
                          prior(normal(0,2), class = sigma)),
                iter = 4000, warmup = 1000, thin = 6,
                cores = 8, chains = 8)


# m.gamma.eH <- brm(eH ~ cyear + (cyear | regional_level),
#                 family = lognormal(),
#                 data = gamma_ts,
#                 prior = c(prior(normal(2,1), class = Intercept),
#                           prior(normal(0,1), class = b),
#                           prior(normal(0,1), class = sd),
#                           prior(normal(0,2), class = sigma)),
#                 iter = 4000, warmup = 1000, thin = 6,
#                 cores = 8, chains = 8)


m.gamma.S_PIE <- brm(S_PIE ~ cyear + (cyear | regional_level),
                  family = lognormal(),
                  data = gamma_ts,
                  prior = c(prior(normal(2,1), class = Intercept),
                            prior(normal(0,1), class = b),
                            prior(normal(0,1), class = sd),
                            prior(normal(0,2), class = sigma)),
                  iter = 4000, warmup = 1000, thin = 6,
                  cores = 8, chains = 8)

save(m.gammaS, 
     file = '~/Dropbox/1current/spatial_composition_change/results/model_fits/gammaS-ts-model.Rdata')

# save(m.gamma.eH, 
#      file = '~/Dropbox/1current/spatial_composition_change/results/model_fits/gamma-eH-ts-model.Rdata')

save(m.gamma.S_PIE, 
     file = '~/Dropbox/1current/spatial_composition_change/results/model_fits/gamma-S_PIE-ts-model.Rdata')


pp_check(m.gammaS) +scale_x_continuous(trans = 'log2')
# pp_check(m.gamma.eH) +scale_x_continuous(trans = 'log2')
pp_check(m.gamma.S_PIE) +scale_x_continuous(trans = 'log2')


