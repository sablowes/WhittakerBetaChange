# local analysis of 'raw' data: log-ratio of change through time
library(tidyverse)
library(brms)


load('/data/idiv_chase/sablowes/homogenisation/data/allLRR_multiyr.Rdata')

regional_ES_norm_taxa_multi <- brm(bf(ES_gamma ~ 0 + taxon_mod + (1 | regional_level)),
                          data = regional_jknife_LRR,
                          prior = c(prior(normal(0,1), class = b),
                                    prior(normal(0,1), class = sd)),
                          cores = 4, chains = 4,
                          iter = 20000, thin = 10)

save(regional_ES_norm_taxa_multi,
     file = Sys.getenv('OFILE'))
