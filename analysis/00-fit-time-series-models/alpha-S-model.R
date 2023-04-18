library(tidyverse)
library(brms)

# load('~/Dropbox/1current/spatial_composition_change/results/all_timeSeries.Rdata')
load('/data/idiv_chase/sablowes/homogenisation/data/all_timeSeries.Rdata')

alpha_ts <- alpha_ts %>% 
  mutate(cyear = year - mean(year),
         Sround = round(S))

m.alphaS_lnorm <- brm(bf(S ~ cyear + (cyear | regional_level/local_level)),
                family = lognormal(),
                data = alpha_ts,
		init_r = 0.05,
                cores = 8, chains = 8,
		iter = 4000, warmup = 1000, thin = 3,
                prior = c(prior(normal(2,1), class = Intercept),
                          prior(normal(0,0.5), class = b),
                          prior(normal(0,1), class = sd, coef = Intercept, group = regional_level),
                          prior(normal(0,1), class = sd, coef = cyear, group = regional_level),
                          prior(normal(0,1), class = sd, coef = Intercept, group = regional_level:local_level),
                          prior(normal(0,1), class = sd, coef = cyear, group = regional_level:local_level)))

save(m.alphaS_lnorm,
     file = Sys.getenv('OFILE'))
