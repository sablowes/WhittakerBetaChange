# wrangle linear models of alpha- and gamma-scale diversity through time
library(tidyverse)
library(brms)
library(tidybayes)


load('~/Dropbox/1current/spatial_composition_change/results/model_fits/local-S-lnorm-30871432.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/model_fits/local-S_PIE-lnorm-30871434.Rdata')

load('~/Dropbox/1current/spatial_composition_change/results/model_fits/gammaS-ts-model.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/model_fits/gamma-S_PIE-ts-model.Rdata')

local_deltaS = gather_draws(m.alphaS_lnorm, `b_cyear.*`, regex = TRUE)

regional_levels <- m.alphaS_lnorm$data %>% 
  as_tibble() %>% 
  distinct(regional_level) %>% 
  mutate(level = regional_level) %>%
  nest(data = level) 

slopes_regional_variation <- regional_levels %>%
  mutate(local_S = purrr::map(data, ~posterior_samples(m.alphaS_lnorm, 
                                                     pars = paste('r_regional_level[', as.character(.x$level), ',cyear]', sep=''),
                                                     exact = TRUE,
                                                     subset = floor(runif(n = 1000,
                                                                          min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
         # local_eH = purrr::map(data, ~posterior_samples(m.alpha.eH_lnorm, 
         #                                            pars = paste('r_regional_level[', as.character(.x$level), ',cyear]', sep=''),
         #                                            exact = TRUE,
         #                                            subset = floor(runif(n = 1000,
         #                                                                 min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
         local_S_PIE = purrr::map(data, ~posterior_samples(m.alpha.S_PIE_lnorm,
                                                  pars = paste('r_regional_level[', as.character(.x$level), ',cyear]', sep=''),
                                                  exact = TRUE,
                                                  subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
         # models fit to regional scale data,
         regional_S = purrr::map(data, ~posterior_samples(m.gammaS, 
                                                       pars = paste('r_regional_level[', as.character(.x$level), ',cyear]', sep=''),
                                                       exact = TRUE,
                                                       subset = floor(runif(n = 1000,
                                                                            min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
         # regional_eH = purrr::map(data, ~posterior_samples(m.gamma.eH,
         #                                                pars = paste('r_regional_level[', as.character(.x$level), ',cyear]', sep=''),
         #                                                exact = TRUE,
         #                                                subset = floor(runif(n = 1000,
         #                                                                     min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
         regional_S_PIE = purrr::map(data, ~posterior_samples(m.gamma.S_PIE,
                                                           pars = paste('r_regional_level[', as.character(.x$level), ',cyear]', sep=''),
                                                           exact = TRUE,
                                                           subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()))


slopes_global_level <- bind_cols(gather_draws(m.alphaS_lnorm, `b_cyear`, regex = TRUE, ndraws = 1000) %>% 
                                   ungroup() %>% 
                                   rename(local_S_global = .value) %>% 
                                   select(local_S_global),
                                 # gather_draws(m.alpha.eH_lnorm, `b_cyear`, regex = TRUE, ndraws = 1000) %>% 
                                 #   ungroup() %>% 
                                 #   rename(local_eH_global = .value) %>% 
                                 #   select(local_eH_global),
                                 gather_draws(m.alpha.S_PIE_lnorm, `b_cyear`, regex = TRUE, ndraws = 1000) %>% 
                                   ungroup() %>% 
                                   rename(local_S_PIE_global = .value) %>% 
                                   select(local_S_PIE_global),
                                 # regional_scale
                                 gather_draws(m.gammaS, `b_cyear`, regex = TRUE, ndraws = 1000) %>% 
                                   ungroup() %>% 
                                   rename(regional_S_global = .value) %>% 
                                   select(regional_S_global),
                                 # gather_draws(m.gamma.eH, `b_cyear`, regex = TRUE, ndraws = 1000) %>% 
                                 #   ungroup() %>% 
                                 #   rename(regional_eH_global = .value) %>% 
                                 #   select(regional_eH_global),
                                 gather_draws(m.gamma.S_PIE, `b_cyear`, regex = TRUE, ndraws = 1000) %>% 
                                   ungroup() %>% 
                                   rename(regional_S_PIE_global = .value) %>% 
                                   select(regional_S_PIE_global))

load('~/Dropbox/1current/spatial_composition_change/data/ts_meta.Rdata')
slopes_regional_variation <- left_join(slopes_regional_variation,
                                       ts_meta)


# add the conceptual information to a summary (median and 90% quantile)
regional_ts_concept <-
slopes_regional_variation %>% 
  unnest(cols = c(local_S,  local_S_PIE, regional_S, regional_S_PIE)) %>% #local_eH,, regional_eH, 
  # regional variation describes departures from global estimate, put global in
  bind_cols(slopes_global_level %>% 
              slice(rep(1:n(), times = 165))) %>% 
  # summarise(alpha_S = median(local_S + local_S_global),
  #           gamma_S = median(regional_S + regional_S_global),
  #           alpha_S_PIE = median(local_S_PIE + local_S_PIE_global),
  #           gamma_S_PIE = median(regional_S_PIE + regional_S_PIE_global))
  group_by(regional_level) %>% 
  summarise(alpha_S_hat = median(local_S + local_S_global),
            # alpha_eH_hat = median(local_eH),
            alpha_S_PIE_hat = median(local_S_PIE + local_S_PIE_global),
            alpha_S_hat_Q05 = quantile(local_S + local_S_global, prob = 0.05),
            alpha_S_hat_Q95 = quantile(local_S + local_S_global, prob = 0.95),
            # alpha_eH_hat_Q05 = quantile(local_eH, prob = 0.05),
            # alpha_eH_hat_Q95 = quantile(local_eH, prob = 0.95),
            alpha_S_PIE_hat_Q05 = quantile(local_S_PIE + local_S_PIE_global, prob = 0.05),
            alpha_S_PIE_hat_Q95 = quantile(local_S_PIE + local_S_PIE_global, prob = 0.95),
            # regional_scale
            gamma_S_hat = median(regional_S + regional_S_global),
            # gamma_eH_hat = median(regional_eH),
            gamma_S_PIE_hat = median(regional_S_PIE + regional_S_PIE_global),
            gamma_S_hat_Q05 = quantile(regional_S + regional_S_global, prob = 0.05),
            gamma_S_hat_Q95 = quantile(regional_S + regional_S_global, prob = 0.95),
            # gamma_eH_hat_Q05 = quantile(regional_eH, prob = 0.05),
            # gamma_eH_hat_Q95 = quantile(regional_eH, prob = 0.95),
            gamma_S_PIE_hat_Q05 = quantile(regional_S_PIE + regional_S_PIE_global, prob = 0.05),
            gamma_S_PIE_hat_Q95 = quantile(regional_S_PIE + regional_S_PIE_global, prob = 0.95)) %>% 
  ungroup() %>% 
  mutate(S_spatial_pattern = ifelse(gamma_S_hat > alpha_S_hat, 'differentiation', 'homogenisation'),
         # eH_spatial_pattern = ifelse(gamma_eH_hat > alpha_eH_hat, 'differentiation', 'homogenisation'),
         S_PIE_spatial_pattern = ifelse(gamma_S_PIE_hat > alpha_S_PIE_hat, 'differentiation', 'homogenisation'),
         # not sure these concepts are as applicable to occupancy when q > 0?
       S_concept = case_when((S_spatial_pattern=='differentiation' & gamma_S_hat > 0 & alpha_S_hat > 0) ~ 'Gain low occupancy',
                    (S_spatial_pattern=='differentiation' & gamma_S_hat > 0 & alpha_S_hat < 0) ~ 'Low occupancy replace high',
                    (S_spatial_pattern=='differentiation' & gamma_S_hat < 0 & alpha_S_hat < 0) ~ 'Lose high occupancy',
                    (S_spatial_pattern!='differentiation' & gamma_S_hat > 0 & alpha_S_hat > 0) ~ 'Gain high occupancy',
                    (S_spatial_pattern!='differentiation' & gamma_S_hat < 0 & alpha_S_hat > 0) ~ 'High occupancy replace low',
                    (S_spatial_pattern!='differentiation' & gamma_S_hat < 0 & alpha_S_hat < 0) ~ 'Lose low occupancy'),
       # now use CIs to determine category counts that incorporate uncertainty
       S_concept_CIs = case_when((S_spatial_pattern=='differentiation' & (gamma_S_hat > 0 & alpha_S_hat > 0) &
                           ((alpha_S_hat_Q05 > 0 & alpha_S_hat_Q95 > 0) & (gamma_S_hat_Q05 > 0 & gamma_S_hat_Q95 > 0))) ~ 'Gain low occupancy',
                        (S_spatial_pattern=='differentiation' & (gamma_S_hat > 0 & alpha_S_hat < 0) &
                           ((alpha_S_hat_Q05 < 0 & alpha_S_hat_Q95 < 0) & (gamma_S_hat_Q05 > 0 & gamma_S_hat_Q95 > 0))) ~ 'Low occupancy replace high',
                        (S_spatial_pattern=='differentiation' & (gamma_S_hat < 0 & alpha_S_hat < 0) &
                           ((alpha_S_hat_Q05 < 0 & alpha_S_hat_Q95 < 0) & (gamma_S_hat_Q05 < 0 & gamma_S_hat_Q95 < 0))) ~ 'Lose high occupancy',
                        (S_spatial_pattern!='differentiation' & (gamma_S_hat > 0 & alpha_S_hat > 0) &
                           ((alpha_S_hat_Q05 > 0 & alpha_S_hat_Q95 > 0) & (gamma_S_hat_Q05 > 0 & gamma_S_hat_Q95 > 0))) ~ 'Gain high occupancy',
                        (S_spatial_pattern!='differentiation' & (gamma_S_hat < 0 & alpha_S_hat > 0) &
                           ((alpha_S_hat_Q05 > 0 & alpha_S_hat_Q95 > 0) & (gamma_S_hat_Q05 < 0 & gamma_S_hat_Q95 < 0))) ~ 'High occupancy replace low',
                        (S_spatial_pattern!='differentiation' & (gamma_S_hat < 0 & alpha_S_hat < 0) &
                           ((alpha_S_hat_Q05 < 0 & alpha_S_hat_Q95 < 0) & (gamma_S_hat_Q05 < 0 & gamma_S_hat_Q95 < 0))) ~ 'Lose low occupancy'))

two_scales <- slopes_regional_variation %>% 
  unnest(cols = c(local_S,  local_S_PIE, regional_S, regional_S_PIE)) %>% #local_eH,, regional_eH, 
  # regional variation describes departures from global estimate, put global in
  bind_cols(slopes_global_level %>% 
              slice(rep(1:n(), times = 165))) %>% 
  select(-data)
