# local analysis of 'raw' data: log-ratio of change through time
library(tidyverse)
library(brms)


load('/data/idiv_chase/sablowes/homogenisation/data/allLRR_meta.Rdata')

local_ES_norm_sampletype <- brm(bf(ES ~ 0 + sample_type + (1 | regional_level)),
                           data = local_LRR,
                           prior = c(prior(normal(0,1), class = b),
                                     prior(normal(0,1), class = sd)),
                           cores = 4, chains = 4,
                           iter = 20000, thin = 10)

save(local_ES_norm_sampletype,
     file = Sys.getenv('OFILE'))
