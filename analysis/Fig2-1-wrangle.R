# results from simplest two-stage analysis: overall intercept-only model fit to the log-ratio / duration (ES)
library(tidyverse)
library(brms)
library(tidybayes)
library(cowplot)


load('~/Dropbox/1current/spatial_composition_change/results/model_fits/local-es-norm-sigma2-30871399.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/model_fits/regional-es-jk-norm-sigma2-30871398.Rdata')
load('~/Dropbox/1current/spatial_composition_change/data/all_meta.Rdata')

# fix one name for joining data
all_meta <- all_meta %>% 
  mutate(regional_level = ifelse(regional_level=='i_Lee (Florida (United States))',
                                 'i_Lee', regional_level))

set.seed(101)
local_overall_post <- gather_draws(local_ES_norm_sigma2, b_Intercept) %>% 
  # 90% credible interval of intercept
  median_qi(.width = 0.9)  

regional_overall_post <- gather_draws(regional_ES_jk_norm_sigma2, b_Intercept) %>% 
  median_qi(.width = 0.9)

overall_intercept <- bind_cols(local_overall_post %>% 
                                 rename(local_intercept = .value,
                                        local_Q95 = .upper,
                                        local_Q05 = .lower) %>% 
                                 select(local_intercept, local_Q95, local_Q05),
                               regional_overall_post %>% 
                                 rename(regional_intercept = .value,
                                        regional_Q95 = .upper,
                                        regional_Q05 = .lower) %>% 
                                 select(regional_intercept, regional_Q95, regional_Q05))

local_posterior_ES <- local_ES_norm_sigma2$data %>% 
  add_predicted_draws(object = local_ES_norm_sigma2, ndraws = 1000)

local_summary_ES <- local_posterior_ES %>% 
  group_by(regional_level) %>% 
  summarise(local_mu.i = mean(ES),
            # local_se.i = unique(glm_coef.std.error),
            local_mu.hat = mean(.prediction),
            l95 = quantile(.prediction, probs = 0.95),
            l05 = quantile(.prediction, probs = 0.05)) %>% 
  #rank by local estimated effect size
  arrange(local_mu.hat) %>% 
  mutate(local_rank = 1:n())

regional_posterior <- regional_ES_jk_norm_sigma2$data %>% 
  add_predicted_draws(object = regional_ES_jk_norm_sigma2, ndraws = 1000)


regional_summary <- regional_posterior %>% 
  group_by(regional_level) %>% 
  summarise(regional_mu.i = mean(ES_gamma),
            # regional_se.i = unique(std.error),
            regional_mu.hat = mean(.prediction),
            r95 = quantile(.prediction, probs = 0.95),
            r05 = quantile(.prediction, probs = 0.05))

regional_posterior <- regional_posterior %>% 
                                  rename(regional_dS = .prediction) %>% 
  ungroup()

local_posterior_ES <- local_posterior_ES %>% 
  rename(local_dS = .prediction) %>% 
  ungroup()


pattern_summary <- left_join(local_summary_ES, 
                             regional_summary) %>% 
  mutate(spatial_pattern = ifelse(regional_mu.hat > local_mu.hat, 'differentiation', 'homogenisation'),
         spatial_pattern_obs = case_when(regional_mu.i > local_mu.i ~ 'differentiation',
                                         regional_mu.i < local_mu.i ~ 'homogenisation',
                                         regional_mu.i == local_mu.i ~ 'no change beta-diversity')) %>% 
  left_join(all_meta) %>%
  mutate(concept = case_when((spatial_pattern=='differentiation' & regional_mu.hat > 0 & local_mu.hat > 0) ~ 'Gain low occupancy',
                             (spatial_pattern=='differentiation' & regional_mu.hat > 0 & local_mu.hat < 0) ~ 'Low occupancy replace high',
                             (spatial_pattern=='differentiation' & regional_mu.hat < 0 & local_mu.hat < 0) ~ 'Lose high occupancy',
                             (spatial_pattern!='differentiation' & regional_mu.hat > 0 & local_mu.hat > 0) ~ 'Gain high occupancy',
                             (spatial_pattern!='differentiation' & regional_mu.hat < 0 & local_mu.hat > 0) ~ 'High occupancy replace low',
                             (spatial_pattern!='differentiation' & regional_mu.hat < 0 & local_mu.hat < 0) ~ 'Lose low occupancy'),
         # now use CIs to determine category counts that incorporate uncertainty
         concept_CIs = case_when((spatial_pattern=='differentiation' & (regional_mu.hat > 0 & local_mu.hat > 0) &
                                    ((l05 > 0 & l95 > 0) & (r05 > 0 & r95 > 0))) ~ 'Gain low occupancy',
                             (spatial_pattern=='differentiation' & (regional_mu.hat > 0 & local_mu.hat < 0) &
                                ((l05 < 0 & l95 < 0) & (r05 > 0 & r95 > 0))) ~ 'Low occupancy replace high',
                             (spatial_pattern=='differentiation' & (regional_mu.hat < 0 & local_mu.hat < 0) &
                                ((l05 < 0 & l95 < 0) & (r05 < 0 & r95 < 0))) ~ 'Lose high occupancy',
                             (spatial_pattern!='differentiation' & (regional_mu.hat > 0 & local_mu.hat > 0) &
                                ((l05 > 0 & l95 > 0) & (r05 > 0 & r95 > 0))) ~ 'Gain high occupancy',
                             (spatial_pattern!='differentiation' & (regional_mu.hat < 0 & local_mu.hat > 0) &
                                ((l05 > 0 & l95 > 0) & (r05 < 0 & r95 < 0))) ~ 'High occupancy replace low',
                             (spatial_pattern!='differentiation' & (regional_mu.hat < 0 & local_mu.hat < 0) &
                                ((l05 < 0 & l95 < 0) & (r05 < 0 & r95 < 0))) ~ 'Lose low occupancy'),
         # empirical (observed) data: border cases (i.e., where change in either local or regional richness = 0 are omitted here,
         # fractional counts to categories added later)
         concept_obs = case_when((spatial_pattern_obs=='differentiation' & regional_mu.i > 0 & local_mu.i > 0) ~ 'Gain low occupancy',
                                 (spatial_pattern_obs=='differentiation' & regional_mu.i > 0 & local_mu.i < 0) ~ 'Low occupancy replace high',
                                 (spatial_pattern_obs=='differentiation' & regional_mu.i < 0 & local_mu.i < 0) ~ 'Lose high occupancy',
                                 (spatial_pattern_obs!='differentiation' & regional_mu.i > 0 & local_mu.i > 0) ~ 'Gain high occupancy',
                                 (spatial_pattern_obs!='differentiation' & regional_mu.i < 0 & local_mu.i > 0) ~ 'High occupancy replace low',
                                 (spatial_pattern_obs!='differentiation' & regional_mu.i < 0 & local_mu.i < 0) ~ 'Lose low occupancy'),
         # document changes at local and regional scale where estimates differ from zero
         local_change = case_when((l05 > 0 & l95 > 0) ~ 'local gains',
                                  (l05 < 0 & l95 < 0) ~ 'local Losees',
                                  (l05 < 0 & l95 > 0) ~ 'local no change'),
         regional_change = case_when((r05 > 0 & r95 > 0) ~ 'regional gains',
                                     (r05 < 0 & r95 < 0) ~ 'regional gains',
                                     (r05 < 0 & r95 > 0) ~ 'regional no change'))

# this leaves a few ties unbroken where empirical estimates lie on boundaries
# pattern_summary %>% 
#   select(concept_obs) %>% 
#   filter(!complete.cases(.))
# 
# pattern_summary %>% 
#   group_by(concept_CIs) %>% 
#   summarise(n())
# 
# pattern_summary %>% 
#   group_by(local_change) %>% 
#   summarise(n())
# 
# pattern_summary %>% 
#   group_by(regional_change) %>% 
#   summarise(n())
concept_colour = c('Gain low occupancy' = '#61CDE0',
                   'Low occupancy replace high' = '#2CABA4',
                   'Lose high occupancy' = '#155F49',
                   'Lose low occupancy' = '#D17538',
                   'High occupancy replace low' = '#E9AE27',
                   'Gain high occupancy' = '#D9D956')
