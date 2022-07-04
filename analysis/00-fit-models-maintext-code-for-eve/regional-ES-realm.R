# local analysis of 'raw' data: log-ratio of change through time
library(tidyverse)
library(brms)


load('/data/idiv_chase/sablowes/homogenisation/data/allLRR_meta.Rdata')

regional_ES_norm_realm <- brm(bf(ES_gamma ~ 0 + realm + (1 | regional_level)),
                        data = regional_jknife_LRR,
                        prior = c(prior(normal(0,1), class = b),
                                  prior(normal(0,1), class = sd)),
                        cores = 4, chains = 4,
                        iter = 20000, thin = 10)

save(regional_ES_norm_realm,
     file = Sys.getenv('OFILE'))
