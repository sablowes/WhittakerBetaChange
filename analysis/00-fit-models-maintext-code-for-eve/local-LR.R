# local analysis of 'raw' data: log-ratio of change through time
library(tidyverse)
library(brms)


load('/data/idiv_chase/sablowes/homogenisation/data/allLRR_meta.Rdata')

local_LR_norm <- brm(bf(alpha_LR ~ 1 + (1 | regional_level)),
                        data = local_LRR,
                        prior = c(prior(normal(0,1), class = Intercept),
                                  prior(normal(0,1), class = sd)),
                        cores = 4, chains = 4,
                        iter = 20000, thin = 10)

save(local_LR_norm,
     file = Sys.getenv('OFILE'))
