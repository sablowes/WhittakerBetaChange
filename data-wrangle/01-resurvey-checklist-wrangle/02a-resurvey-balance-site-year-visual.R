path2dir <- '~/Dropbox/1current/spatial_composition_change/WhittakerBetaChange/data-wrangle/01-resurvey-checklist-wrangle/balanced-site-years'
# manual processing of metacommunity resurvey sampling sites and years
load('~/Dropbox/1current/spatial_composition_change/data/resurvey-location-plots-maxRect.Rdata')

##	Get the raw data 
dat <- read_csv('~/Dropbox/BioTimeX/Local-Regional Homogenization/_data_extraction/metacommunity-survey-communities.csv')

# create new regional_level for resurvey data
dat_rs <- dat %>% 
  unite(regional_level, c(dataset_id, regional), remove = FALSE) 

# regional level alber_2022_GCE1
dat_rs %>% 
  filter(regional_level=='alber_2022_GCE1') %>% 
  group_by(year) %>%
  mutate(nsites = n_distinct(local)) %>%
  ungroup() %>%
  filter(nsites >= 4) %>%
  distinct(year, local) %>% 
  filter(local %in% c('GCE1_CB1', 'GCE1_MM8', 'GCE1_MM6', 'GCE1_MM3')) %>% 
  filter(!year %in% c(2014, 2016, 2018)) %>% 
  # write_csv(paste0(path2dir, '/alber_2022_GCE1.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level alber_2022_GCE10
rs_filtered %>% 
  filter(regional_level=='alber_2022_GCE10') %>% 
  group_by(year) %>%
  mutate(nsites = n_distinct(local)) %>%
  ungroup() %>%
  filter(nsites == 11) %>%
  distinct(year, local) %>% 
  # write_csv(paste0(path2dir, '/alber_2022_GCE10.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level alber_2022_GCE4
dat_rs %>% 
  filter(regional_level=='alber_2022_GCE4') %>% 
  filter(local %in% c('GCE4_MM8', 'GCE4_MM6', 'GCE4_MM3', 'GCE4_MM1',
                      'GCE4_CB8', 'GCE4_CB1')) %>% 
  filter(!year %in% c(2011,2015)) %>% 
  distinct(year, local) %>% 
  # write_csv(paste0(path2dir, '/alber_2022_GCE4.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level alber_2022_GCE5
dat_rs %>% 
  filter(regional_level=='alber_2022_GCE5') %>% 
  filter(local %in% c('GCE5_MM8', 'GCE5_MM6', 'GCE5_MM3', 'GCE5_MM1',
                      'GCE5_CB6', 'GCE4_CB1')) %>%
  filter(!year %in% c(2007, 2008, 2015, 2018, 2019)) %>%
  distinct(year, local) %>% 
  # write_csv(paste0(path2dir, '/alber_2022_GCE5.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level 
dat_rs %>% 
  filter(regional_level=='alber_2022_GCE6') %>% 
  filter(local %in% c('GCE6_MM8', 'GCE6_MM6', 'GCE6_MM3', 'GCE6_MM1',
                      'GCE6_CB8', 'GCE6_CB43', 'GCE6_CB1')) %>%
  filter(!year %in% c(2003:2009, 2011, 2014, 2015:2018)) %>%
  distinct(year, local) %>% 
  # write_csv(paste0(path2dir, '/alber_2022_GCE6.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level alber_2022_GCE9
dat_rs %>% 
  filter(regional_level=='alber_2022_GCE9') %>% 
  filter(local %in% c('GCE9_MM8', 'GCE9_MM6', 'GCE9_MM3', 'GCE9_MM1',
                      'GCE9_CB18', 'GCE9_CB11')) %>%
  filter(!year %in% c(2000, 2010, 2012:2015, 2017, 2020)) %>%
  distinct(year, local) %>%
  # write_csv(paste0(path2dir, '/alber_2022_GCE9.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level alston_2021c_CENTRAL
dat_rs %>% 
  filter(regional_level=='alston_2021c_CENTRAL') %>% 
  group_by(local) %>% 
  mutate(nyrs = n_distinct(year)) %>% 
  ungroup() %>% 
  filter(nyrs == max(nyrs)) %>% 
  distinct(year, local) %>%
  # write_csv(paste0(path2dir, '/alston_2021c_CENTRAL', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level alston_2021c_NORTH
dat_rs %>% 
  filter(regional_level=='alston_2021c_NORTH') %>% 
  group_by(local) %>% 
  mutate(nyrs = n_distinct(year)) %>% 
  ungroup() %>% 
  filter(nyrs == max(nyrs)) %>% 
  distinct(year, local) %>%
  # write_csv(paste0(path2dir, '/alston_2021c_NORTH', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level alves_2022_Belizean Barrier Reef
dat_rs %>% 
  filter(regional_level=='alves_2022_Belizean Barrier Reef') %>% 
  filter(year %in% c(1997, 1999, 2005, 2016)) %>%
  distinct(year, local) %>%
  group_by(local) %>%
  mutate(nyrs = n_distinct(year)) %>%
  ungroup() %>%
  filter(nyrs == max(nyrs)) %>%
  # write_csv(paste0(path2dir, '/alves_2022_Belizean Barrier Reef', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level anderson_2019b_Wizard
rs_filtered %>% 
  filter(regional_level=='anderson_2019b_Wizard') %>% 
  filter(year %in% c(1997, 2001, 2007)) %>% 
  group_by(local) %>%
  mutate(nyrs = n_distinct(year)) %>%
  ungroup() %>%
  filter(nyrs == max(nyrs)) %>%
  distinct(year, local) %>% 
  # write_csv(paste0(path2dir, '/anderson_2019b_Wizard', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level burlakova_2021_Lake Ontario
dat_rs %>% 
  filter(regional_level=='burlakova_2021_Lake Ontario') %>% 
  distinct(year, local) %>%
  group_by(local) %>%
  mutate(nyrs = n_distinct(year)) %>%
  ungroup() %>%
  filter(nyrs == max(nyrs)) %>%
  # write_csv(paste0(path2dir, '/burlakova_2021_Lake Ontario', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level christensen_2021_Jornada Experimental Range, USA
dat_rs %>% 
  filter(regional_level=='christensen_2021_Jornada Experimental Range, USA') %>% 
  filter(year > 1925) %>% 
  distinct(year, local) %>%
  group_by(local) %>%
  mutate(nyrs = n_distinct(year)) %>%
  ungroup() %>%
  filter(nyrs > max(nyrs)/2) %>%
  group_by(year) %>% 
  mutate(nsites = n_distinct(local)) %>% 
  ungroup() %>% 
  filter(nsites > (max(nsites) * 0.9)) %>% 
  group_by(local) %>%
  mutate(nyrs = n_distinct(year)) %>%
  ungroup() %>%
  filter(nyrs == max(nyrs)) %>%
  # write_csv(paste0(path2dir, '/christensen_2021_Jornada Experimental Range, USA', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level gibb_2019_Main Camp area of the Ethabuka Reserve
dat_rs %>% 
  filter(regional_level=='gibb_2019_Main Camp area of the Ethabuka Reserve') %>% 
  filter(local %in% c('MC4_Swale', 'MC4_Crest', 'MC1_Swale', 'MC1_Crest')) %>%
  filter(year %in% c(1992, 1993, 1996, 2005, 2010, 2013)) %>%
  distinct(year, local) %>%
  # write_csv(paste0(path2dir, '/gibb_2019_Main Camp area of the Ethabuka Reserve', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level green_2021_Vancouver Island, Canada
dat_rs %>% 
  filter(regional_level=='green_2021_Vancouver Island, Canada') %>% 
  filter(local %in% c(2:4, 6, 7)) %>%
  group_by(year) %>% 
  mutate(n_site = n_distinct(local)) %>% 
  ungroup() %>% 
  filter(n_site == 5) %>%
  distinct(year, local) %>%
  # write_csv(paste0(path2dir, '/green_2021_Vancouver Island, Canada', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level moore_2022_Big Fill
dat_rs %>% 
  filter(regional_level=='moore_2022_Big Fill') %>% 
  filter(local %in% c('30720', '30719', '30717', '30713',
                      '12000','11999')) %>%
  filter(!year %in% c(2005:2006, 2009, 2011, 2015, 2016, 2019:2020)) %>%
  distinct(year, local) %>%
  # write_csv(paste0(path2dir, '/moore_2022_Big Fill', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level moore_2022_Reese Tank
dat_rs %>% 
  filter(regional_level=='moore_2022_Reese Tank') %>% 
  filter(year > 2005) %>% 
  filter(local %in% c('30707', '30706', '30705', '30703', 
                      '30702', '30701', '10 / 30710')) %>%
  filter(year %in% c(2006, 2013, 2015, 2016, 2018, 2021)) %>%
  distinct(year, local) %>%
  # write_csv(paste0(path2dir, '/moore_2022_Reese Tank', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level mushet_2017_Cottonwood Lake Study Area
dat_rs %>% 
  filter(regional_level=='mushet_2017_Cottonwood Lake Study Area') %>% 
  filter(local %in% c('T01', 'P08', 'P07', 'P06', 
                      'P04', 'P03', 'P02', 'P01')) %>%
  group_by(year) %>% 
  mutate(nsites = n_distinct(local)) %>% 
  ungroup() %>% 
  filter(nsites == max(nsites)) %>% 
  distinct(year, local) %>%
  # write_csv(paste0(path2dir, '/mushet_2017_Cottonwood Lake Study Area', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level russell-smith_2017_shrubs_Kakadu
dat_rs %>% 
  filter(regional_level=='russell-smith_2017_shrubs_Kakadu') %>% 
  filter(!year %in% c(1994, 1996, 2000, 2001, 2006, 2013)) %>% 
  filter(local != '46') %>% 
  group_by(local) %>%
  mutate(nyrs = n_distinct(year)) %>%
  ungroup() %>%
  filter(nyrs > 4) %>%
  distinct(year, local) %>%
  # write_csv(paste0(path2dir, '/russell-smith_2017_shrubs_Kakadu', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level russell-smith_2017_trees_Kakadu
dat_rs %>% 
  filter(regional_level=='russell-smith_2017_trees_Kakadu') %>% 
  filter(!year %in% c(1994, 1996, 2000, 2001, 2006, 2013)) %>%
  filter(local != '46') %>%
  group_by(local) %>%
  mutate(nyrs = n_distinct(year)) %>%
  ungroup() %>%
  filter(nyrs > 4) %>%
  distinct(year, local) %>%
  # write_csv(paste0(path2dir, '/russell-smith_2017_trees_Kakadu', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level valtonen_2018_Hungary
dat_rs %>% 
  filter(regional_level=='valtonen_2018_Hungary') %>% 
  group_by(local) %>%
  mutate(nyrs = n_distinct(year)) %>%
  ungroup() %>%
  filter(nyrs > (max(nyrs) * 0.6)) %>%
  group_by(year) %>%
  mutate(nsites = n_distinct(local)) %>%
  ungroup() %>%
  filter(nsites == (max(nsites))) %>%
  distinct(year, local) %>%
  # write_csv(paste0(path2dir, '/valtonen_2018_Hungary', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level van-cleve_2021_Bonanza Creek LTER
dat_rs %>% 
  filter(regional_level=='van-cleve_2021_Bonanza Creek LTER') %>%
  filter(year %in% c(1993, 1996, 2000, 2004, 2008, 2018)) %>% 
  group_by(local) %>%
  mutate(nyrs = n_distinct(year)) %>%
  ungroup() %>%
  filter(nyrs == (max(nyrs))) %>%
  group_by(year) %>%
  mutate(nsites = n_distinct(local)) %>%
  ungroup() %>%
  filter(nsites == (max(nsites))) %>%
  distinct(year, local) %>%
  # write_csv(paste0(path2dir, '/van-cleve_2021_Bonanza Creek LTER', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level wardle_2014_mammals_Simpson Desert
dat_rs %>% 
  filter(regional_level=='wardle_2014_mammals_Simpson Desert') %>%
  filter(local %in% c('MC4', 'MC2', 'MC11', 'MC10', 'MC1', 
                      'FRS2', 'CS5', 'CS2')) %>%
  group_by(year) %>%
  mutate(nsites = n_distinct(local)) %>%
  ungroup() %>%
  filter(nsites == (max(nsites))) %>%
  distinct(year, local) %>%
  # write_csv(paste0(path2dir, '/wardle_2014_mammals_Simpson Desert', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level wardle_2014_plants_Simpson Desert
rs_filtered %>% 
  filter(regional_level=='wardle_2014_plants_Simpson Desert') %>%
  group_by(year) %>%
  mutate(nsites = n_distinct(local)) %>%
  ungroup() %>%
  filter(nsites == (max(nsites))) %>%
  distinct(year, local) %>%
  # write_csv(paste0(path2dir, '/wardle_2014_plants_Simpson Desert', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level wardle_2014_reptile_Simpson Desert
rs_filtered %>% 
  filter(regional_level=='wardle_2014_reptile_Simpson Desert') %>%
  filter(local %in% c('MC4', 'MC2', 'MC11', 'MC10', 'MC1', 
                      'FRS2', 'CS5', 'CS2')) %>%
  group_by(year) %>%
  mutate(nsites = n_distinct(local)) %>%
  ungroup() %>%
  filter(nsites == (max(nsites))) %>%
  distinct(year, local) %>%
  # write_csv(paste0(path2dir, '/wardle_2014_reptile_Simpson Desert', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))
