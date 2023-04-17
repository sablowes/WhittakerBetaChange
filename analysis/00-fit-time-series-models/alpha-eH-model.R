library(tidyverse)
library(brms)

# load('~/Dropbox/1current/spatial_composition_change/results/all_timeSeries.Rdata')
load('/data/idiv_chase/sablowes/homogenisation/data/all_timeSeries.Rdata')

alpha_ts <- alpha_ts %>% 
  mutate(cyear = year - mean(year))

m.alpha.eH <- brm(S ~ cyear + (cyear | regional_level),
                family = lognormal(),
                data = alpha_ts,
                cores = 4, chains = 4,
                prior = c(prior(cauchy(1.5,2), class = Intercept),
                          prior(normal(0,1), class = b),
                          prior(normal(0,1), class = sd),
                          prior(normal(0,2), class = sigma)))

save(m.alpha.eH,
     file = Sys.getenv('OFILE'))
