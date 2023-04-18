library(tidyverse)
library(brms)

# load('~/Dropbox/1current/spatial_composition_change/results/all_timeSeries.Rdata')
load('/data/idiv_chase/sablowes/homogenisation/data/all_timeSeries.Rdata')

alpha_ts <- alpha_ts %>% 
  mutate(cyear = year - mean(year),
	 S_PIEround = round(S_PIE))

m.alpha.S_PIE_lnorm <- brm(bf(S_PIE ~ cyear + (cyear | regional_level/local_level)),
                  family = lognormal(),
                  data = alpha_ts,
		  #init = 0,
                  cores = 4, chains = 4,
		  prior = c(prior(normal(1.25,1), class = Intercept),
                          prior(normal(0,0.5), class = b),
                          prior(normal(0,1), class = sd, coef = Intercept, group = regional_level),
                          prior(normal(0,1), class = sd, coef = cyear, group = regional_level),
                          prior(normal(0,1), class = sd, coef = Intercept, group = regional_level:local_level),
                          prior(normal(0,1), class = sd, coef = cyear, group = regional_level:local_level)))

save(m.alpha.S_PIE_lnorm,
     file = Sys.getenv('OFILE'))
