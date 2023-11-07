# code to wrangle the checklists that did not pass visual inspection
path2dir <- '~/Dropbox/1current/spatial_composition_change/WhittakerBetaChange/data-wrangle/01-resurvey-checklist-wrangle/balanced-site-years-checklist/'

##	Get the raw data 
cl_dat <- read_csv('~/Dropbox/BioTimeX/Local-Regional Homogenization/_data_extraction/checklist_change_communities.csv') %>% 
  unite(regional_level, c(dataset_id, regional), remove = F) 

cl_dat <- cl_dat %>% 
  group_by(regional_level) %>% 
  mutate(fyear = case_when(year == min(year) ~ 'first',
                           year == max(year) ~ 'last', 
                           (year!=min(year) & year!=max(year)) ~ 'intermediate')) %>%
  ungroup()

# reduce to regions with at least four sites 
cl_dat <- cl_dat %>% 
  group_by(regional_level) %>% 
  mutate(n_site = n_distinct(local)) %>% 
  filter(n_site >= 4) %>% 
  ungroup()

# regional level botham_2022_Channel Islands
cl_dat %>% 
  filter(regional_level=='botham_2022_Channel Islands') %>% 
  filter(year %in% c(2004, 2021)) %>% 
  group_by(local) %>%
  filter(n_distinct(year) == 2) %>%
  # write_csv(paste0(path2dir, '/botham_2022_Channel Islands', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level botham_2022_England
cl_dat %>% 
  filter(regional_level=='botham_2022_England') %>% 
  filter(year %in% c(1990, 2021)) %>% 
  group_by(local) %>% 
  filter(n_distinct(year) == 2) %>% 
  # write_csv(paste0(path2dir, '/botham_2022_England', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level botham_2022_Northern Ireland
cl_dat %>% 
  filter(regional_level=='botham_2022_Northern Ireland') %>% 
  filter(year %in% c(2010, 2021)) %>% 
  group_by(local) %>% 
  filter(n_distinct(year) == 2) %>% 
  # write_csv(paste0(path2dir, '/botham_2022_Northern Ireland', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level botham_2022_Scotland
cl_dat %>% 
  filter(regional_level=='botham_2022_Scotland') %>% 
  filter(year %in% c(2010, 2021)) %>%
  group_by(local) %>% 
  filter(n_distinct(year) == 2) %>% 
  # write_csv(paste0(path2dir, '/botham_2022_Scotland', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level botham_2022_Wales
cl_dat %>% 
  filter(regional_level=='botham_2022_Wales') %>% 
  filter(year %in% c(2010, 2021)) %>%
  group_by(local) %>%
  filter(n_distinct(year) == 2) %>%
  # write_csv(paste0(path2dir, '/botham_2022_Wales', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level holoplainen_2022_Finland
cl_dat %>% 
  filter(regional_level=='holoplainen_2022_Finland') %>% 
  group_by(year) %>% 
  mutate(nsites = n_distinct(local)) %>% 
  ungroup() %>% 
  filter(nsites > max(nsites) * 0.6) %>% 
  filter(year == min(year) | year==max(year)) %>% 
  group_by(local) %>% 
  filter(n_distinct(year)==2) %>% 
  # write_csv(paste0(path2dir, '/holoplainen_2022_Finland', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level ogan_2022_Rhineland-Palatinate
cl_dat %>% 
  filter(regional_level=='ogan_2022_Rhineland-Palatinate') %>% 
  filter(year %in% c(1989, 2019)) %>% 
  group_by(local) %>% 
  filter(n_distinct(year)==2) %>%   
  # write_csv(paste0(path2dir, '/ogan_2022_Rhineland-Palatinate', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))
