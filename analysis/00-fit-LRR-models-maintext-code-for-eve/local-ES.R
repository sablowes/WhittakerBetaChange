# local analysis of 'raw' data: log-ratio of change through time
library(tidyverse)
library(brms)


load('/data/idiv_chase/sablowes/homogenisation/data/allLRR_meta-new.Rdata')


local_ES_norm_sigma2 <- brm(bf(ES ~ 1 + (1 | regional_level),
			      sigma ~ sample_type * logdt),
                     data = local_LRR %>%
				mutate(logdt = log(dt),
				       regional_level = ifelse(regional_level == 'i_Lee (Florida (United States))',
								'i_Lee', regional_level)),
				prior = c(prior(normal(0,1), class = Intercept),
				          prior(normal(0,1), class = sd),
				          #prior(normal(0,1), class = Intercept, dpar = sigma),	
				          prior(normal(0,1), class = b, coef = logdt, dpar = sigma)),
				cores = 4, chains = 4,
				iter = 6000, thin = 3)

save(local_ES_norm_sigma2,
     file = Sys.getenv('OFILE'))
