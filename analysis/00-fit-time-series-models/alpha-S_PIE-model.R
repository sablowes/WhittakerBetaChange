library(tidyverse)
library(brms)

load('/data/idiv_chase/sablowes/homogenisation/data/two-scale-time-series-plus-meta.Rdata')
# need to remove these regions before fitting models
load('/data/idiv_chase/sablowes/homogenisation/data/regions2remove.Rdata')

alpha_ts <- alpha_ts %>% 
  mutate(cyear = year - mean(year)) %>% 
  filter(!regional_level %in% regions2remove$regional_level)

m.alpha.S_PIE_lnorm <- brm(bf(S_PIE ~ cyear + (cyear | regional_level/local_level)),
                  family = lognormal(),
                  data = alpha_ts,
                  cores = 4, chains = 4,
                  prior = c(prior(normal(1.25,1), class = Intercept),
                            prior(normal(0,0.5), class = b),
                            prior(normal(0,1), class = sd, coef = Intercept, group = regional_level),
                            prior(normal(0,1), class = sd, coef = cyear, group = regional_level),
                            prior(normal(0,1), class = sd, coef = Intercept, group = regional_level:local_level),
                            prior(normal(0,1), class = sd, coef = cyear, group = regional_level:local_level)))

save(m.alpha.S_PIE_lnorm,
     file = Sys.getenv('OFILE'))
