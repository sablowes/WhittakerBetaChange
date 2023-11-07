# results from simplest two-stage analysis: overall intercept-only model fit to the log-ratio / duration (ES)
library(tidyverse)
library(brms)
library(tidybayes)
library(cowplot)


load('~/Dropbox/1current/spatial_composition_change/results/model_fits/local-ES-norm-sigma-641269.Rdata')
load('~/Dropbox/1current/spatial_composition_change/results/model_fits/regional-ES-jk-norm-sigma.Rdata')


dt <- local_ES_norm_sigma2$data %>% 
  as_tibble() %>% 
  mutate(dt = exp(logdt)) %>% 
  group_by(sample_type) %>% 
  summarise(min_dt = min(dt),
            max_dt = max(dt),
            min_logdt = min(logdt),
            max_logdt = max(logdt)) %>% 
  pivot_wider(names_from = sample_type,
              values_from = min_dt:max_logdt)
  

sigma_model_summary <- gather_draws(local_ES_norm_sigma2, 
             b_sigma_Intercept,
             b_sigma_sample_typeresurvey,
             b_sigma_logdt,
             `b_sigma_sample_typeresurvey:logdt`) %>% 
  # 90% credible interval of intercept
  median_qi(.width = 0.9)  %>% 
  mutate(.variable = paste0('alpha_', .variable)) %>% 
  bind_rows(
    gather_draws(regional_ES_jk_norm_sigma2, 
             b_sigma_Intercept,
             b_sigma_sample_typeresurvey,
             b_sigma_logdt,
             `b_sigma_sample_typeresurvey:logdt`) %>%
      median_qi(.width = 0.9) %>% 
      mutate(.variable = paste0('gamma_', .variable))
    ) %>% 
  select(-c(.width, .point, .interval)) %>% 
  pivot_wider(names_from = .variable,
              values_from = c(.value, .lower, .upper)) %>% 
  bind_cols(dt) 

sigma_local_CI <- local_ES_norm_sigma2$data %>% 
  as_tibble() %>%
  group_by(sample_type) %>% 
  modelr::data_grid(logdt, n = 51) %>% 
  ungroup() %>% 
  mutate(dt = exp(logdt),
         Q05 = case_when(sample_type == 'checklist' ~ 
                           exp(sigma_model_summary$.lower_alpha_b_sigma_Intercept +
                                 (sigma_model_summary$.lower_alpha_b_sigma_logdt * logdt)),
                         sample_type == 'resurvey' ~
                           exp((sigma_model_summary$.lower_alpha_b_sigma_Intercept +
                                 sigma_model_summary$.lower_alpha_b_sigma_sample_typeresurvey) +
                                 (sigma_model_summary$.lower_alpha_b_sigma_logdt +
                                    sigma_model_summary$`.lower_alpha_b_sigma_sample_typeresurvey:logdt`)* logdt)
                         ),
         Q95 = case_when(sample_type == 'checklist' ~ 
                           exp(sigma_model_summary$.upper_alpha_b_sigma_Intercept +
                                 (sigma_model_summary$.upper_alpha_b_sigma_logdt * logdt)),
                         sample_type == 'resurvey' ~
                           exp((sigma_model_summary$.upper_alpha_b_sigma_Intercept +
                                  sigma_model_summary$.upper_alpha_b_sigma_sample_typeresurvey) +
                                 (sigma_model_summary$.upper_alpha_b_sigma_logdt +
                                    sigma_model_summary$`.upper_alpha_b_sigma_sample_typeresurvey:logdt`)* logdt)
                         )
  )

sigma_regional_CI <- regional_ES_jk_norm_sigma2$data %>% 
  as_tibble() %>%
  group_by(sample_type) %>% 
  modelr::data_grid(logdt, n = 51) %>% 
  ungroup() %>% 
  mutate(dt = exp(logdt),
         Q05 = case_when(sample_type == 'checklist' ~ 
                           exp(sigma_model_summary$.lower_gamma_b_sigma_Intercept +
                                 (sigma_model_summary$.lower_gamma_b_sigma_logdt * logdt)),
                         sample_type == 'resurvey' ~
                           exp((sigma_model_summary$.lower_gamma_b_sigma_Intercept +
                                  sigma_model_summary$.lower_gamma_b_sigma_sample_typeresurvey) +
                                 (sigma_model_summary$.lower_gamma_b_sigma_logdt +
                                    sigma_model_summary$`.lower_gamma_b_sigma_sample_typeresurvey:logdt`)* logdt)
         ),
         Q95 = case_when(sample_type == 'checklist' ~ 
                           exp(sigma_model_summary$.upper_gamma_b_sigma_Intercept +
                                 (sigma_model_summary$.upper_gamma_b_sigma_logdt * logdt)),
                         sample_type == 'resurvey' ~
                           exp((sigma_model_summary$.upper_gamma_b_sigma_Intercept +
                                  sigma_model_summary$.upper_gamma_b_sigma_sample_typeresurvey) +
                                 (sigma_model_summary$.upper_gamma_b_sigma_logdt +
                                    sigma_model_summary$`.upper_gamma_b_sigma_sample_typeresurvey:logdt`)* logdt)
         )
  )

local_checklist_label = c("Checklist",
                          paste("beta[logdt] == ",
                                round(sigma_model_summary$.value_alpha_b_sigma_logdt, 2),
                                "~(", round(sigma_model_summary$.lower_alpha_b_sigma_logdt, 2),
                                "~-~", round(sigma_model_summary$.upper_alpha_b_sigma_logdt, 2),
                                ")"))
local_resurvey_label = c("Resurvey",
                          paste("beta[logdt] == ",
                                round(sigma_model_summary$.value_alpha_b_sigma_logdt +
                                        sigma_model_summary$`.value_alpha_b_sigma_sample_typeresurvey:logdt`, 2),
                                "~(", round(sigma_model_summary$.lower_alpha_b_sigma_logdt +
                                             sigma_model_summary$`.lower_alpha_b_sigma_sample_typeresurvey:logdt`, 2),
                                "~-~", round(sigma_model_summary$.upper_alpha_b_sigma_logdt +
                                               sigma_model_summary$`.upper_alpha_b_sigma_sample_typeresurvey:logdt`, 2),
                                ")"))

sigma_local <- ggplot() +
    geom_segment(data = sigma_model_summary, 
                 aes(x = min_dt_checklist,
                     xend = max_dt_checklist,
                     y = exp(.value_alpha_b_sigma_Intercept +
                       (.value_alpha_b_sigma_logdt * min_logdt_checklist)),
                     yend = exp(.value_alpha_b_sigma_Intercept +
                       (.value_alpha_b_sigma_logdt * max_logdt_checklist)),
                     linetype = 'Checklist')) +
  geom_segment(data = sigma_model_summary, 
               aes(x = min_dt_resurvey,
                   xend = max_dt_resurvey,
                   y = exp((.value_alpha_b_sigma_Intercept +
                             .value_alpha_b_sigma_sample_typeresurvey) +
                             (.value_alpha_b_sigma_logdt +
                                `.value_alpha_b_sigma_sample_typeresurvey:logdt`) * 
                             min_logdt_resurvey),
                   yend = exp((.value_alpha_b_sigma_Intercept +
                                .value_alpha_b_sigma_sample_typeresurvey) +
                                (.value_alpha_b_sigma_logdt +
                                   `.value_alpha_b_sigma_sample_typeresurvey:logdt`) * 
                                max_logdt_resurvey),
                   linetype = 'Resurvey')) +
  geom_ribbon(data = sigma_local_CI,
              aes(x = dt,
                  ymin = Q05, ymax = Q95, group = sample_type),
              alpha = 0.33) +
  # add regression coefficient and uncertainty interval
  annotate('text', x = 120, y = c(0.005, 0.005*0.8), hjust = 0.1, vjust = 1.4,
           label = local_checklist_label,
           parse = T, size = 2) +
  annotate('text', x = 10, y = c(0.01, 0.01*0.8), hjust = 0.1, vjust = 1.4,
           label = local_resurvey_label,
           parse = T, size = 2) +
  scale_x_continuous(trans = 'log', name = 'Duration [years, log-scale]',
                     labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(trans = 'log', name = expression(paste('log(', sigma, ')')),
                     labels = scales::number_format(accuracy = 0.001)) +
  scale_linetype_manual(name = 'Sample type',
                        values = c('Checklist' = 1,
                                     'Resurvey' = 2)) +
  labs(subtitle = expression(paste(alpha, '-scale'))) +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.justification = c(1,1))

regional_checklist_label = c("Checklist",
                          paste("beta[logdt] == ",
                                round(sigma_model_summary$.value_gamma_b_sigma_logdt, 2),
                                "~(", round(sigma_model_summary$.lower_gamma_b_sigma_logdt, 2),
                                "~-~", round(sigma_model_summary$.upper_gamma_b_sigma_logdt, 2),
                                ")"))
regional_resurvey_label = c("Resurvey",
                         paste("beta[logdt] == ",
                               round(sigma_model_summary$.value_gamma_b_sigma_logdt +
                                       sigma_model_summary$`.value_gamma_b_sigma_sample_typeresurvey:logdt`, 2),
                               "~(", round(sigma_model_summary$.lower_gamma_b_sigma_logdt +
                                             sigma_model_summary$`.lower_gamma_b_sigma_sample_typeresurvey:logdt`, 2),
                               "~-~", round(sigma_model_summary$.upper_gamma_b_sigma_logdt +
                                              sigma_model_summary$`.upper_gamma_b_sigma_sample_typeresurvey:logdt`, 2),
                               ")"))

                     
sigma_regional <- ggplot() +
  geom_segment(data = sigma_model_summary, 
               aes(x = min_dt_checklist,
                   xend = max_dt_checklist,
                   y = exp(.value_gamma_b_sigma_Intercept +
                             (.value_gamma_b_sigma_logdt * min_logdt_checklist)),
                   yend = exp(.value_gamma_b_sigma_Intercept +
                                (.value_gamma_b_sigma_logdt * max_logdt_checklist)),
                   linetype = 'Checklist')) +
  geom_segment(data = sigma_model_summary, 
               aes(x = min_dt_resurvey,
                   xend = max_dt_resurvey,
                   y = exp((.value_gamma_b_sigma_Intercept +
                              .value_gamma_b_sigma_sample_typeresurvey) +
                             (.value_gamma_b_sigma_logdt +
                                `.value_gamma_b_sigma_sample_typeresurvey:logdt`) * 
                             min_logdt_resurvey),
                   yend = exp((.value_gamma_b_sigma_Intercept +
                                 .value_gamma_b_sigma_sample_typeresurvey) +
                                (.value_gamma_b_sigma_logdt +
                                   `.value_gamma_b_sigma_sample_typeresurvey:logdt`) * 
                                max_logdt_resurvey),
                   linetype = 'Resurvey')) +
  geom_ribbon(data = sigma_regional_CI,
              aes(x = dt,
                  ymin = Q05, ymax = Q95, group = sample_type),
              alpha = 0.33) +
  # add regression coefficient and uncertainty interval
  annotate('text', x = 120, y = c(0.0001, 0.0001*0.8), hjust = 0.1, vjust = 1.4,
           label = regional_checklist_label,
           parse = T, size = 2) +
  annotate('text', x = 10, y = c(0.08, 0.08*0.8), hjust = 0.1, vjust = 1.4,
           label = regional_resurvey_label,
           parse = T, size = 2) +
  scale_x_continuous(trans = 'log', name = 'Duration [years, log-scale]',
                     labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(trans = 'log', name = expression(paste('log(', sigma, ')')),
                     labels = scales::number_format(accuracy = 0.0001)) +
  scale_linetype_manual(name = 'Sample type',
                        values = c('Checklist' = 1,
                                   'Resurvey' = 2)) +
  labs(subtitle = expression(paste(gamma, '-scale'))) +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        legend.justification = c(1,1))

cowplot::plot_grid(sigma_local,
                   sigma_regional, 
                   labels = c('a', 'b'))

ggsave('~/Dropbox/1current/spatial_composition_change/figures/results/FigS4.pdf',
       width = 185, height = 100, units = 'mm')
