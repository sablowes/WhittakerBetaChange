library(tidyverse)
library(brms)

# load('~/Dropbox/1current/spatial_composition_change/results/all_timeSeries.Rdata')
load('/data/idiv_chase/sablowes/homogenisation/data/all_timeSeries.Rdata')

alpha_ts <- alpha_ts %>% 
  mutate(cyear = year - mean(year),
         Sround = round(S))

m.alphaS_pois <- brm(Sround ~ cyear + (cyear | regional_level / local_level),
                family = poisson,
                data = alpha_ts,
                cores = 4, chains = 4,
                prior = c(prior(cauchy(2,2), class = Intercept),
                          prior(normal(0,1), class = b),
                          prior(normal(0,1), class = sd)))

save(m.alphaS_pois,
     file = Sys.getenv('OFILE'))
