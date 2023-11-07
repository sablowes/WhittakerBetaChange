path2dir <- '~/Dropbox/1current/spatial_composition_change/WhittakerBetaChange/data-wrangle/01-resurvey-checklist-wrangle/balanced-site-years'
# manual processing of metacommunity resurvey sampling sites and years
load('~/Dropbox/1current/spatial_composition_change/data/resurvey-location-plots-maxRect.Rdata')

##	Get the raw data 
dat <- read_csv('~/Dropbox/BioTimeX/Local-Regional Homogenization/_data_extraction/metacommunity-survey_communities-standardised.csv')

# create new regional_level for resurvey data
dat_rs <- dat %>% 
  unite(regional_level, c(dataset_id, regional), remove = FALSE) 

# regional level alber_2022_grain_0.5_m2_GCE10
dat_rs %>% 
  filter(regional_level=='alber_2022_grain_0.5_m2_GCE10') %>% 
  group_by(year) %>%
  mutate(nsites = n_distinct(local)) %>%
  ungroup() %>%
  filter(nsites >= 4) %>%
  distinct(year, local) %>% 
  filter(local %in% c('GCE10_CB1', 'GCE10_CB6', 'GCE10_CB8', 'GCE10_JN1', 
                      'GCE10_JN3', 'GCE10_JN6', 'GCE10_JN8', 'GCE10_MM1', 
                      'GCE10_MM3', 'GCE10_MM6', 'GCE10_MM8')) %>% 
  filter(year > 2008 & year < 2020 & year!=2016 & year!= 2018) %>%
  # write_csv(paste0(path2dir, '/alber_2022_grain_0.5_m2_GCE10.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level alber_2022_grain_0.5_m2_GCE2
rs_filtered %>% 
  filter(regional_level=='alber_2022_grain_0.5_m2_GCE2') %>% 
  group_by(year) %>%
  mutate(nsites = n_distinct(local)) %>%
  ungroup() %>%
  filter(nsites == 5) %>%
  distinct(year, local) %>% 
  # write_csv(paste0(path2dir, '/alber_2022_grain_0.5_m2_GCE2.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level alber_2022_grain_0.5_m2_GCE4
dat_rs %>% 
  filter(regional_level=='alber_2022_grain_0.5_m2_GCE4') %>% 
  filter(local %in% c('GCE4_MM8', 'GCE4_MM6', 'GCE4_MM3', 'GCE4_MM1',
                      'GCE4_CB8', 'GCE4_CB1')) %>% 
  filter(!year %in% c(2011,2015)) %>% 
  distinct(year, local) %>% 
  # write_csv(paste0(path2dir, '/alber_2022_grain_0.5_m2_GCE4.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level alber_2022_grain_0.5_m2_GCE5
dat_rs %>% 
  filter(regional_level=='alber_2022_grain_0.5_m2_GCE5') %>% 
  filter(local %in% c('GCE5_MM8', 'GCE5_MM6', 'GCE5_MM3', 'GCE5_MM1',
                      'GCE5_CB6', 'GCE4_CB1')) %>%
  filter(!year %in% c(2007, 2008, 2015, 2018, 2019)) %>%
  distinct(year, local) %>% 
  # write_csv(paste0(path2dir, '/alber_2022_grain_0.5_m2_GCE5.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level alber_2022_grain_0.5_m2_GCE6
dat_rs %>% 
  filter(regional_level=='alber_2022_grain_0.5_m2_GCE6') %>% 
  filter(local %in% c('GCE6_MM8', 'GCE6_MM6', 'GCE6_MM3', 'GCE6_MM1',
                      'GCE6_CB8', 'GCE6_CB43', 'GCE6_CB1')) %>%
  filter(!year %in% c(2003:2009, 2011, 2014, 2015:2018)) %>%
  distinct(year, local) %>% 
  # write_csv(paste0(path2dir, '/alber_2022_grain_0.5_m2_GCE6.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level alber_2022_grain_0.5_m2_GCE9
dat_rs %>% 
  filter(regional_level=='alber_2022_grain_0.5_m2_GCE9') %>% 
  filter(local %in% c('GCE9_MM8', 'GCE9_MM6', 'GCE9_MM3', 'GCE9_MM1',
                      'GCE9_CB18', 'GCE9_CB11')) %>%
  filter(!year %in% c(2000, 2010, 2012:2015, 2017, 2020)) %>%
  distinct(year, local) %>%
  # write_csv(paste0(path2dir, '/alber_2022_grain_0.5_m2_GCE9.csv'))
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

# regional level alves_2022_Middle_Caye
dat_rs %>% 
  filter(regional_level=='alves_2022_Middle_Caye') %>% 
  filter(year != 2009) %>%
  filter(local != 8) %>%
  distinct(year, local) %>%
  group_by(local) %>%
  mutate(nyrs = n_distinct(year)) %>%
  ungroup() %>%
  distinct(year, local) %>% 
  # write_csv(paste0(path2dir, '/alves_2022_Middle_Caye', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level alves_2022_Tacklebox
dat_rs %>% 
  filter(regional_level=='alves_2022_Tacklebox') %>% 
  filter(local < 7) %>%
  distinct(year, local) %>%
  # write_csv(paste0(path2dir, '/alves_2022_Tacklebox', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level bashevkin_2022_EMP_Macro_Upper San Francisco Estuary
rs_filtered %>% 
  filter(regional_level=='bashevkin_2022_EMP_Macro_Upper San Francisco Estuary') %>% 
  filter(local %in% c('NZ086', 'NZ074', 'NZ064',
                      'NZ060', 'NZ054', 'NZ048', 'NZ032')) %>% 
  group_by(year) %>%
  mutate(nsites = n_distinct(local)) %>%
  ungroup() %>%
  filter(nsites == max(nsites)) %>%
  distinct(year, local) %>% 
  # write_csv(paste0(path2dir, '/bashevkin_2022_EMP_Macro_Upper San Francisco Estuary', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level bashevkin_2022_EMP_Meso_Upper San Francisco Estuary
rs_filtered %>% 
  filter(regional_level=='bashevkin_2022_EMP_Meso_Upper San Francisco Estuary') %>% 
  filter(local %in% c('NZS42', 'NZM10', 'NZD28',
                      'NZ092', 'NZ086', 'NZ074', 'NZ064', 'NZ060', 'NZ054',
                      'NZ048', 'NZ032', 'NZ028')) %>% 
  group_by(year) %>%
  mutate(nsites = n_distinct(local)) %>%
  ungroup() %>%
  filter(nsites == max(nsites)) %>%
  distinct(year, local) %>% 
  # write_csv(paste0(path2dir, '/bashevkin_2022_EMP_Meso_Upper San Francisco Estuary', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level bashevkin_2022_EMP_Micro_Upper San Francisco Estuary
rs_filtered %>% 
  filter(regional_level=='bashevkin_2022_EMP_Micro_Upper San Francisco Estuary') %>% 
  filter(local %in% c('NZS42', 'NZM10', 'NZD28',
                      'NZ092', 'NZ086', 'NZ074', 'NZ064', 'NZ060', 'NZ054',
                      'NZ048', 'NZ032', 'NZ028')) %>% 
  group_by(year) %>%
  mutate(nsites = n_distinct(local)) %>%
  ungroup() %>%
  filter(nsites == max(nsites)) %>%
  distinct(year, local) %>% 
  # write_csv(paste0(path2dir, '/bashevkin_2022_EMP_Micro_Upper San Francisco Estuary', '.csv'))
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

# regional level cumming_2023_Great Barrier Reef
dat_rs %>% 
  filter(regional_level=='cumming_2023_Great Barrier Reef') %>% 
  filter(year > 1993) %>% 
  group_by(year) %>%
  mutate(nsites = n_distinct(local)) %>%
  ungroup() %>%
  filter(nsites > max(nsites) * 0.5) %>%
  group_by(local) %>%
  mutate(nyrs = n_distinct(year)) %>%
  ungroup() %>%
  filter(nyrs == max(nyrs)) %>%
  filter(!year %in% c(1994, 2003)) %>% 
  # write_csv(paste0(path2dir, '/cumming_2023_Great Barrier Reef', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level edgar_2022_cryptic_ATRC-ParkVic-FRDC_Kent Group
dat_rs %>% 
  filter(regional_level=='edgar_2022_cryptic_ATRC-ParkVic-FRDC_Kent Group') %>% 
  group_by(year) %>%
  mutate(nsites = n_distinct(local)) %>%
  ungroup() %>%
  filter(nsites > 0.6 * max(nsites)) %>%
  group_by(local) %>% 
  filter(n_distinct(year)==5) %>% 
  # write_csv(paste0(path2dir, '/edgar_2022_cryptic_ATRC-ParkVic-FRDC_Kent Group', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level edgar_2022_cryptic_ATRC-ParkVic-FRDC_Maria Island
dat_rs %>% 
  filter(regional_level=='edgar_2022_cryptic_ATRC-ParkVic-FRDC_Maria Island') %>% 
  group_by(local) %>%
  mutate(nyrs = n_distinct(year)) %>%
  ungroup() %>%
  filter(nyrs > max(nyrs) * 0.7) %>%
  group_by(year) %>% 
  mutate(nsites = n_distinct(local)) %>% 
  ungroup() %>% 
  filter(nsites==max(nsites)) %>% 
  # write_csv(paste0(path2dir, '/edgar_2022_cryptic_ATRC-ParkVic-FRDC_Maria Island', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level edgar_2022_cryptic_ATRC-ParkVic-FRDC_Port Davey
dat_rs %>% 
  filter(regional_level=='edgar_2022_cryptic_ATRC-ParkVic-FRDC_Port Davey') %>% 
  filter(local %in% c('PD-S14', 'PD-S18', 'PD-S23', 'PD-S26')) %>%
  group_by(year) %>% 
  filter(n_distinct(local)==4) %>% 
  # write_csv(paste0(path2dir, '/edgar_2022_cryptic_ATRC-ParkVic-FRDC_Port Davey', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level edgar_2022_cryptic_RLS_Jervis Bay
dat_rs %>% 
  filter(regional_level=='edgar_2022_cryptic_RLS_Jervis Bay') %>% 
  group_by(local) %>%
  mutate(nyrs = n_distinct(year)) %>%
  ungroup() %>%
  filter(nyrs > max(nyrs)*0.7) %>%
  group_by(year) %>%
  mutate(nsites = n_distinct(local)) %>%
  ungroup() %>%
  filter(nsites==max(nsites)) %>%
  # write_csv(paste0(path2dir, '/edgar_2022_cryptic_RLS_Jervis Bay', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))



# regional level edgar_2022_fish_Batemans
dat_rs %>% 
  filter(regional_level=='edgar_2022_fish_Batemans') %>% 
  filter(year %in% c(2006, 2007, 2017)) %>% 
  group_by(local) %>%
  mutate(nyrs = n_distinct(year)) %>%
  ungroup() %>%
  filter(nyrs == max(nyrs)) %>%
  # write_csv(paste0(path2dir, '/edgar_2022_fish_Batemans', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level edgar_2022_fish_Bunurong
dat_rs %>% 
  filter(regional_level=='edgar_2022_fish_Bunurong') %>% 
  filter(year %in% c(2000:2003, 2006, 2011, 2015)) %>% 
  # group_by(local) %>%
  # mutate(nyrs = n_distinct(year)) %>%
  # ungroup() %>%
  # filter(nyrs == max(nyrs)) %>%
  # write_csv(paste0(path2dir, '/edgar_2022_fish_Bunurong', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level edgar_2022_fish_Encounter
dat_rs %>% 
  filter(regional_level=='edgar_2022_fish_Encounter') %>% 
  group_by(year) %>% 
  mutate(nsites = n_distinct(local)) %>% 
  ungroup() %>% 
  filter(nsites > max(nsites) * 0.5) %>% 
  group_by(local) %>% 
  mutate(nyrs = n_distinct(year)) %>% 
  ungroup() %>% 
  filter(nyrs == max(nyrs)) %>% 
  # write_csv(paste0(path2dir, '/edgar_2022_fish_Encounter', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level edgar_2022_fish_Jervis Bay
dat_rs %>% 
  filter(regional_level=='edgar_2022_fish_Jervis Bay') %>% 
  group_by(year) %>% 
  mutate(nsites = n_distinct(local)) %>% 
  ungroup() %>% 
  filter(nsites > max(nsites) * 0.8) %>% 
  group_by(local) %>% 
  mutate(nyrs = n_distinct(year)) %>% 
  ungroup() %>% 
  filter(nyrs == max(nyrs)) %>% 
  # write_csv(paste0(path2dir, '/edgar_2022_fish_Jervis Bay', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level edgar_2022_fish_Kent Group
dat_rs %>% 
  filter(regional_level=='edgar_2022_fish_Kent Group') %>% 
  group_by(year) %>%
  mutate(nsites = n_distinct(local)) %>%
  ungroup() %>%
  filter(nsites > max(nsites) * 0.8) %>%
  group_by(local) %>%
  mutate(nyrs = n_distinct(year)) %>%
  ungroup() %>%
  filter(nyrs == max(nyrs)) %>%
  # write_csv(paste0(path2dir, '/edgar_2022_fish_Kent Group', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level edgar_2022_fish_Ningaloo Reef
dat_rs %>% 
  filter(regional_level=='edgar_2022_fish_Ningaloo Reef') %>% 
  group_by(year) %>%
  mutate(nsites = n_distinct(local)) %>%
  ungroup() %>%
  filter(nsites > max(nsites) * 0.5) %>%
  group_by(local) %>%
  mutate(nyrs = n_distinct(year)) %>%
  ungroup() %>%
  filter(nyrs == max(nyrs)) %>%
  # write_csv(paste0(path2dir, '/edgar_2022_fish_Ningaloo Reef', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level edgar_2022_fish_Pilbara
dat_rs %>% 
  filter(regional_level=='edgar_2022_fish_Pilbara') %>% 
  group_by(local) %>%
  mutate(nyrs = n_distinct(year)) %>%
  ungroup() %>%
  filter(nyrs == max(nyrs)) %>%
  # write_csv(paste0(path2dir, '/edgar_2022_fish_Pilbara', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level edgar_2022_fish_Port Davey
dat_rs %>% 
  filter(regional_level=='edgar_2022_fish_Port Davey') %>% 
  filter(year %in% c(2005, 2006, 2009, 2013, 2017, 2019)) %>% 
  group_by(local) %>%
  mutate(nyrs = n_distinct(year)) %>%
  ungroup() %>%
  filter(nyrs == max(nyrs)) %>%
  # write_csv(paste0(path2dir, '/edgar_2022_fish_Port Davey', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level edgar_2022_fish_Port Phillip Heads
dat_rs %>% 
  filter(regional_level=='edgar_2022_fish_Port Phillip Heads') %>% 
  group_by(year) %>%
  mutate(nsites = n_distinct(local)) %>%
  ungroup() %>%
  filter(nsites > max(nsites) * 0.5) %>%
  group_by(local) %>%
  mutate(nyrs = n_distinct(year)) %>%
  ungroup() %>%
  filter(nyrs == max(nyrs)) %>%
  # write_csv(paste0(path2dir, '/edgar_2022_fish_Port Phillip Heads', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level edgar_2022_fish_Port Stephens
dat_rs %>% 
  filter(regional_level=='edgar_2022_fish_Port Stephens') %>% 
  group_by(year) %>%
  mutate(nsites = n_distinct(local)) %>%
  ungroup() %>%
  filter(nsites > max(nsites) * 0.5) %>%
  group_by(local) %>%
  mutate(nyrs = n_distinct(year)) %>%
  ungroup() %>%
  filter(nyrs == max(nyrs)) %>%
  # write_csv(paste0(path2dir, '/edgar_2022_fish_Port Stephens', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level edgar_2022_fish_Solitary Islands
dat_rs %>% 
  filter(regional_level=='edgar_2022_fish_Solitary Islands') %>% 
  filter(year %in% c(2009, 2013, 2016, 2017, 2019, 2020)) %>% 
  group_by(local) %>%
  mutate(nyrs = n_distinct(year)) %>%
  ungroup() %>%
  filter(nyrs == max(nyrs)) %>%
  # write_csv(paste0(path2dir, '/edgar_2022_fish_Solitary Islands', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level edgar_2022_fish_Sydney
dat_rs %>% 
  filter(regional_level=='edgar_2022_fish_Sydney') %>% 
  group_by(year) %>%
  mutate(nsites = n_distinct(local)) %>%
  ungroup() %>%
  filter(nsites > max(nsites) * 0.5) %>%
  group_by(local) %>%
  mutate(nyrs = n_distinct(year)) %>%
  ungroup() %>%
  filter(nyrs > max(nyrs) * 0.7) %>%
  filter(year %in% c(2011, 2014, 2017, 2018, 2020)) %>% 
  # write_csv(paste0(path2dir, '/edgar_2022_fish_Sydney', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level edgar_2022_fish_Tasmania - North
dat_rs %>% 
  filter(regional_level=='edgar_2022_fish_Tasmania - North') %>% 
  filter(local %in% c('TAS307', 'TAS309', 'TAS310', 'TAS311', 'TAS312', 'TAS313',
                      'TAS314', 'TAS315', 'TAS316')) %>% 
  # write_csv(paste0(path2dir, '/edgar_2022_fish_Tasmania - North', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level edgar_2022_fish_Tasmania - North East
dat_rs %>% 
  filter(regional_level=='edgar_2022_fish_Tasmania - North East') %>% 
  filter(local %in% c('TNE-S3', 'TAS303', 'TAS302', 'TAS300', 'TAS295', 
                      'TAS294') &
           year > 1998) %>% 
  # write_csv(paste0(path2dir, '/edgar_2022_fish_Tasmania - North East', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level edgar_2022_fish_Tasmania - South
dat_rs %>% 
  filter(regional_level=='edgar_2022_fish_Tasmania - South') %>% 
  filter(year %in% c(1994, 2007, 2018)) %>% 
  group_by(local) %>% 
  mutate(nyrs = n_distinct(year)) %>% 
  ungroup() %>% 
  filter(nyrs == max(nyrs)) %>% 
  # write_csv(paste0(path2dir, '/edgar_2022_fish_Tasmania - South', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level edgar_2022_fish_Tasmania - South East
dat_rs %>% 
  filter(regional_level=='edgar_2022_fish_Tasmania - South East') %>% 
  filter(year %in% c(1994, 2006, 2018)) %>% 
  group_by(local) %>%
  mutate(nyrs = n_distinct(year)) %>%
  ungroup() %>%
  filter(nyrs == max(nyrs)) %>%
  # write_csv(paste0(path2dir, '/edgar_2022_fish_Tasmania - South East', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level edgar_2022_fish_Tasmania - West
dat_rs %>% 
  filter(regional_level=='edgar_2022_fish_Tasmania - West') %>% 
  filter(year %in% c(1994, 2006, 2018)) %>%
  group_by(local) %>%
  mutate(nyrs = n_distinct(year)) %>%
  ungroup() %>%
  filter(nyrs == max(nyrs)) %>%
  # write_csv(paste0(path2dir, '/edgar_2022_fish_Tasmania - West', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level edgar_2022_macroinvertebrates_ATRC-ParkVic-FRDC_Batemans
dat_rs %>% 
  filter(regional_level=='edgar_2022_macroinvertebrates_ATRC-ParkVic-FRDC_Batemans') %>% 
  group_by(year) %>% 
  mutate(nsites = n_distinct(local)) %>% 
  ungroup() %>% 
  filter(nsites > max(nsites) * 0.6) %>% 
  filter(year != 2008) %>% 
  group_by(local) %>%
  mutate(nyrs = n_distinct(year)) %>%
  ungroup() %>%
  filter(nyrs == max(nyrs)) %>%
  # write_csv(paste0(path2dir, '/edgar_2022_macroinvertebrates_ATRC-ParkVic-FRDC_Batemans', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level edgar_2022_macroinvertebrates_ATRC-ParkVic-FRDC_Kent Group
dat_rs %>% 
  filter(regional_level=='edgar_2022_macroinvertebrates_ATRC-ParkVic-FRDC_Kent Group') %>% 
  group_by(local) %>% 
  mutate(nyrs = n_distinct(year)) %>% 
  ungroup() %>% 
  filter(nyrs == max(nyrs)) %>% 
  # write_csv(paste0(path2dir, '/edgar_2022_macroinvertebrates_ATRC-ParkVic-FRDC_Kent Group', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level edgar_2022_macroinvertebrates_ATRC-ParkVic-FRDC_Port Davey
dat_rs %>% 
  filter(regional_level=='edgar_2022_macroinvertebrates_ATRC-ParkVic-FRDC_Port Davey') %>% 
  filter(year > 2004 & year != 2014 & year < 2020) %>% 
  group_by(local) %>%
  filter(n_distinct(year)==9) %>%
  # write_csv(paste0(path2dir, '/edgar_2022_macroinvertebrates_ATRC-ParkVic-FRDC_Port Davey', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level edgar_2022_macroinvertebrates_ATRC-ParkVic-FRDC_Port Phillip Heads
dat_rs %>% 
  filter(regional_level=='edgar_2022_macroinvertebrates_ATRC-ParkVic-FRDC_Port Phillip Heads') %>% 
  filter(year > 1998) %>% 
  group_by(local) %>% 
  mutate(nyrs = n_distinct(year)) %>% 
  ungroup() %>% 
  filter(nyrs == max(nyrs)) %>% 
  filter(!year %in% 2010:2011) %>% 
  # write_csv(paste0(path2dir, '/edgar_2022_macroinvertebrates_ATRC-ParkVic-FRDC_Port Phillip Heads', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level edgar_2022_macroinvertebrates_ATRC-ParkVic-FRDC_Tasmania - North
dat_rs %>% 
  filter(regional_level=='edgar_2022_macroinvertebrates_ATRC-ParkVic-FRDC_Tasmania - North') %>% 
  filter(year > 1998 & year != 2006) %>%
  group_by(local) %>%
  filter(n_distinct(year) ==3) %>%
  # write_csv(paste0(path2dir, '/edgar_2022_macroinvertebrates_ATRC-ParkVic-FRDC_Tasmania - North', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level edgar_2022_macroinvertebrates_ATRC-ParkVic-FRDC_Tasmania - North East
dat_rs %>% 
  filter(regional_level=='edgar_2022_macroinvertebrates_ATRC-ParkVic-FRDC_Tasmania - North East') %>% 
  filter(year > 1998 & year != 2000) %>%
  group_by(local) %>%
  filter(n_distinct(year) ==3) %>%
  # write_csv(paste0(path2dir, '/edgar_2022_macroinvertebrates_ATRC-ParkVic-FRDC_Tasmania - North East', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level edgar_2022_macroinvertebrates_RLS_Jervis Bay
dat_rs %>% 
  filter(regional_level=='edgar_2022_macroinvertebrates_RLS_Jervis Bay') %>% 
  filter(year %in% c(2010, 2011, 2013, 2022)) %>%
  group_by(local) %>%
  filter(n_distinct(year) == 4) %>%
  # write_csv(paste0(path2dir, '/edgar_2022_macroinvertebrates_RLS_Jervis Bay', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level gibb_2019_Main Camp area of the Ethabuka Reserve
dat_rs %>% 
  filter(regional_level=='gibb_2019_Main Camp area of the Ethabuka Reserve') %>% 
  filter(local %in% c('MC10_Swale', 'MC10_Crest', 'MC11_Swale', 'MC11_Crest')) %>%
  group_by(year) %>% 
  filter(n_distinct(local)==4) %>% 
  # write_csv(paste0(path2dir, '/gibb_2019_Main Camp area of the Ethabuka Reserve', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level gomez-gras_2021_Scandola Nature Reserve, France
dat_rs %>% 
  filter(regional_level=='gomez-gras_2021_Scandola Nature Reserve, France') %>% 
  filter(year %in% c(2003, 2011, 2018)) %>% 
  group_by(local) %>% 
  filter(n_distinct(year)==3) %>% 
  # write_csv(paste0(path2dir, '/gomez-gras_2021_Scandola Nature Reserve, France', '.csv'))
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
  # write_csv(paste0(path2dir, '/green_2021_Vancouver Island, Canada', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level jacinto_2022_Lower Putah Creek
dat_rs %>% 
  filter(regional_level=='jacinto_2022_Lower Putah Creek') %>% 
  group_by(year) %>% 
  filter(n_distinct(local) == 6) %>% 
  # write_csv(paste0(path2dir, '/jacinto_2022_Lower Putah Creek', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level klinkovska_2022_grain_100_Jeseniky, CZ
dat_rs %>% 
  filter(regional_level=='klinkovska_2022_grain_100_Jeseniky, CZ') %>% 
  filter(year %in% c(1974, 2010, 2021)) %>% 
  group_by(local) %>% 
  filter(n_distinct(year)==3) %>% 
  # write_csv(paste0(path2dir, '/klinkovska_2022_grain_100_Jeseniky, CZ', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level klinkovska_2022_grain_25_Jeseniky, CZ
dat_rs %>% 
  filter(regional_level=='klinkovska_2022_grain_25_Jeseniky, CZ') %>% 
  filter(year %in% c(1974, 2010, 2021)) %>%
  group_by(local) %>%
  filter(n_distinct(year)==3) %>%
  # write_csv(paste0(path2dir, '/klinkovska_2022_grain_25_Jeseniky, CZ', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level kolasa_2022_Discovery Bay, Jamaica
dat_rs %>% 
  filter(regional_level=='kolasa_2022_Discovery Bay, Jamaica') %>% 
  filter(!year %in% c(2004)) %>%
  group_by(local) %>%
  filter(n_distinct(year)==12) %>%
  # write_csv(paste0(path2dir, '/kolasa_2022_Discovery Bay, Jamaica', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level koleskinova_2021b_PP19
dat_rs %>% 
  filter(regional_level=='koleskinova_2021b_PP19') %>% 
  filter(!year %in% c(2007)) %>%
  group_by(local) %>%
  filter(n_distinct(year)==3) %>%
  # write_csv(paste0(path2dir, '/koleskinova_2021b_PP19', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level larson_2023_Bellevue, IA
dat_rs %>% 
  filter(regional_level=='larson_2023_Bellevue, IA') %>% 
  group_by(year) %>%
  mutate(nsites = n_distinct(local)) %>% 
  ungroup() %>% 
  filter(nsites > max(nsites) * 0.6) %>%
  filter(!year %in% c(2012, 2018)) %>% 
  group_by(local) %>% 
  filter(n_distinct(year)==7) %>% 
  # write_csv(paste0(path2dir, '/larson_2023_Bellevue, IA', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level larson_2023_Lake City, MN
dat_rs %>% 
  filter(regional_level=='larson_2023_Lake City, MN') %>% 
  group_by(year) %>%
  mutate(nsites = n_distinct(local)) %>%
  ungroup() %>%
  filter(nsites > max(nsites) * 0.6) %>%
  group_by(local) %>%
  filter(n_distinct(year) > 10) %>%
  group_by(year) %>%
  filter(n_distinct(local)==12) %>%
  # write_csv(paste0(path2dir, '/larson_2023_Lake City, MN', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level larson_2023_Onalaska, WI
dat_rs %>% 
  filter(regional_level=='larson_2023_Onalaska, WI') %>% 
  group_by(year) %>%
  mutate(nsites = n_distinct(local)) %>%
  ungroup() %>%
  filter(nsites > max(nsites) * 0.6) %>%
  group_by(local) %>%
  mutate(nyrs = n_distinct(year)) %>%
  filter(nyrs == 9) %>%
  filter(year %in% c(2005, 2010, 2018)) %>% 
  # write_csv(paste0(path2dir, '/larson_2023_Onalaska, WI', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level magnuson_2020_North Temperate Lakes
dat_rs %>% 
  filter(regional_level=='magnuson_2020_North Temperate Lakes') %>% 
  group_by(year) %>% 
  filter(n_distinct(local) == 7) %>% 
  # write_csv(paste0(path2dir, '/magnuson_2020_North Temperate Lakes', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level moore_2022_Big Fill
dat_rs %>% 
  filter(regional_level=='moore_2022_Big Fill') %>% 
  filter(local %in% c('30720', '30719', '30717', '30713',
                      '12000','11999')) %>%
  filter(!year %in% c(2005:2006, 2009, 2011, 2015, 2016, 2019:2020)) %>%
  # write_csv(paste0(path2dir, '/moore_2022_Big Fill', '.csv'))
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
  # write_csv(paste0(path2dir, '/mushet_2017_Cottonwood Lake Study Area', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level quimbayo_2022_deserta_island
dat_rs %>% 
  filter(regional_level=='quimbayo_2022_deserta_island') %>% 
  group_by(year) %>% 
  mutate(nsites = n_distinct(local)) %>% 
  ungroup() %>% 
  filter(nsites == max(nsites)) %>% 
  # write_csv(paste0(path2dir, '/quimbayo_2022_deserta_island', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level quimbayo_2022_xavier_island
dat_rs %>% 
  filter(regional_level=='quimbayo_2022_xavier_island') %>% 
  filter(year %in% c(2010, 2013, 2021)) %>% 
  group_by(local) %>% 
  filter(n_distinct(year)==3) %>% 
  # write_csv(paste0(path2dir, '/quimbayo_2022_xavier_island', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level rennie_2017_butterflies_T03
dat_rs %>% 
  filter(regional_level=='rennie_2017_butterflies_T03') %>% 
  filter(local !='1_2') %>%
  group_by(year) %>% 
  mutate(nsites = n_distinct(local)) %>% 
  ungroup() %>% 
  filter(nsites==max(nsites)) %>% 
  # write_csv(paste0(path2dir, '/rennie_2017_butterflies_T03', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level rennie_2017_carabids_T04
dat_rs %>% 
  filter(regional_level=='rennie_2017_carabids_T04') %>% 
  filter(year > 2000) %>% 
  group_by(local) %>% 
  mutate(nyrs = n_distinct(year)) %>% 
  ungroup() %>% 
  filter(nyrs > max(nyrs) * 0.5) %>% 
  filter(!local %in% c('4_T38', '4_T31')) %>% 
  group_by(year) %>% 
  mutate(nsites = n_distinct(local)) %>% 
  ungroup() %>% 
  filter(nsites == max(nsites)) %>% 
  # write_csv(paste0(path2dir, '/rennie_2017_carabids_T04', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level rennie_2017_carabids_T05
dat_rs %>% 
  filter(regional_level=='rennie_2017_carabids_T05') %>% 
  filter(!local %in% c('2_T19', '2_T13')) %>% 
  group_by(year) %>%
  mutate(nsites = n_distinct(local)) %>%
  ungroup() %>%
  filter(nsites == max(nsites)) %>%
  # write_csv(paste0(path2dir, '/rennie_2017_carabids_T05', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level rennie_2017_carabids_T10
dat_rs %>% 
  filter(regional_level=='rennie_2017_carabids_T10') %>% 
  group_by(local) %>% 
  mutate(nyrs = n_distinct(year)) %>% 
  ungroup() %>% 
  filter(nyrs > max(nyrs) * 0.9) %>% 
  group_by(year) %>% 
  mutate(nsites = n_distinct(local)) %>% 
  ungroup() %>% 
  filter(nsites == max(nsites)) %>% 
  # write_csv(paste0(path2dir, '/rennie_2017_carabids_T10', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level rennie_2017_carabids_T11
dat_rs %>% 
  filter(regional_level=='rennie_2017_carabids_T11') %>% 
  group_by(local) %>%
  mutate(nyrs = n_distinct(year)) %>%
  ungroup() %>%
  filter(nyrs > max(nyrs) * 0.9) %>%
  group_by(year) %>%
  mutate(nsites = n_distinct(local)) %>%
  ungroup() %>%
  filter(nsites == max(nsites)) %>%
  # write_csv(paste0(path2dir, '/rennie_2017_carabids_T11', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level rennie_2017_carabids_T12
dat_rs %>% 
  filter(regional_level=='rennie_2017_carabids_T12') %>% 
  filter(year < 2009 & !year %in% c(2002, 2004, 2005:2006) & 
           !local %in% c('4_T34', '4_T31')) %>% 
  group_by(local) %>%
  filter(n_distinct(year)==6) %>%
  # write_csv(paste0(path2dir, '/rennie_2017_carabids_T12', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level rennie_2017_coarsegrain_T01
dat_rs %>% 
  filter(regional_level=='rennie_2017_coarsegrain_T01') %>% 
  filter(year !=2003) %>%
  group_by(local) %>%
  filter(n_distinct(year)==3) %>%
  # write_csv(paste0(path2dir, '/rennie_2017_coarsegrain_T01', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level rennie_2017_coarsegrain_T04
dat_rs %>% 
  filter(regional_level=='rennie_2017_coarsegrain_T04') %>% 
  filter(year !=1994) %>%
  group_by(local) %>%
  filter(n_distinct(year)==3) %>%
  # write_csv(paste0(path2dir, '/rennie_2017_coarsegrain_T04', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level rennie_2017_coarsegrain_T05
dat_rs %>% 
  filter(regional_level=='rennie_2017_coarsegrain_T05') %>% 
  filter(year !=1994) %>%
  group_by(local) %>%
  filter(n_distinct(year)==3) %>%
  # write_csv(paste0(path2dir, '/rennie_2017_coarsegrain_T05', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level rennie_2017_coarsegrain_T08
dat_rs %>% 
  filter(regional_level=='rennie_2017_coarsegrain_T08') %>% 
  filter(year !=1994) %>%
  group_by(local) %>%
  filter(n_distinct(year)==3) %>%
  # write_csv(paste0(path2dir, '/rennie_2017_coarsegrain_T08', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level rennie_2017_coarsegrain_T09
dat_rs %>% 
  filter(regional_level=='rennie_2017_coarsegrain_T09') %>% 
  filter(year !=2003) %>%
  group_by(local) %>%
  filter(n_distinct(year)==3) %>%
  # write_csv(paste0(path2dir, '/rennie_2017_coarsegrain_T09', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level rennie_2017_coarsegrain_T10
dat_rs %>% 
  filter(regional_level=='rennie_2017_coarsegrain_T10') %>% 
  filter(year != 2002) %>% 
  group_by(local) %>%
  filter(n_distinct(year)==3) %>%
  # write_csv(paste0(path2dir, '/rennie_2017_coarsegrain_T10', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level rennie_2017_finegrain_T04
dat_rs %>% 
  filter(regional_level=='rennie_2017_finegrain_T04') %>% 
  group_by(year) %>%
  mutate(nsites = n_distinct(local)) %>% 
  ungroup() %>% 
  filter(nsites == max(nsites)) %>%
  # write_csv(paste0(path2dir, '/rennie_2017_finegrain_T04', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level rennie_2017_finegrain_T05
dat_rs %>% 
  filter(regional_level=='rennie_2017_finegrain_T05') %>% 
  group_by(year) %>%
  mutate(nsites = n_distinct(local)) %>% 
  ungroup() %>% 
  filter(nsites == max(nsites)) %>%
  # write_csv(paste0(path2dir, '/rennie_2017_finegrain_T05', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level rennie_2017_finegrain_T09
dat_rs %>% 
  filter(regional_level=='rennie_2017_finegrain_T09') %>% 
  group_by(year) %>%
  mutate(nsites = n_distinct(local)) %>%
  ungroup() %>%
  filter(nsites > max(nsites) * 0.8) %>%
  group_by(local) %>% 
  filter(n_distinct(year) == 6) %>% 
  # write_csv(paste0(path2dir, '/rennie_2017_finegrain_T09', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level rennie_2017_finegrain_T11
dat_rs %>% 
  filter(regional_level=='rennie_2017_finegrain_T11') %>% 
  group_by(year) %>%
  mutate(nsites = n_distinct(local)) %>%
  ungroup() %>%
  filter(nsites > max(nsites) * 0.8) %>%
  group_by(local) %>%
  filter(n_distinct(year) == 5) %>%
  # write_csv(paste0(path2dir, '/rennie_2017_finegrain_T11', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level rennie_2017_finegrain_T12
dat_rs %>% 
  filter(regional_level=='rennie_2017_finegrain_T12') %>% 
  group_by(local) %>%
  filter(n_distinct(year) == 7) %>%
  # write_csv(paste0(path2dir, '/rennie_2017_finegrain_T12', '.csv'))
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

# regional level santana_2017_SPA Castro Verde High-intensity
dat_rs %>% 
  filter(regional_level=='santana_2017_SPA Castro Verde High-intensity') %>% 
  filter(year!=1996) %>% 
  group_by(local) %>% 
  filter(n_distinct(year)==5) %>% 
  # write_csv(paste0(path2dir, '/santana_2017_SPA Castro Verde High-intensity', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level valtonen_2018_Hungary
dat_rs %>% 
  filter(regional_level=='valtonen_2018_Hungary') %>% 
  group_by(year) %>%
  mutate(nsites = n_distinct(local)) %>%
  ungroup() %>%
  filter(nsites == (max(nsites))) %>%
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
  # write_csv(paste0(path2dir, '/van-cleve_2021_Bonanza Creek LTER', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level wahren_2016_PV_OUT_G
dat_rs %>% 
  filter(regional_level=='wahren_2016_PV_OUT_G') %>%
  filter(!year %in% c(1951, 1966)) %>%
  group_by(local) %>%
  mutate(nyrs = n_distinct(year)) %>%
  ungroup() %>%
  filter(nyrs == (max(nyrs))) %>%
  # write_csv(paste0(path2dir, '/wahren_2016_PV_OUT_G', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level wardle_2014_mammals_Simpson Desert
dat_rs %>% 
  filter(regional_level=='wardle_2014_mammals_Simpson Desert') %>%
  filter(year > 2000) %>% 
  group_by(year) %>%
  mutate(nsites = n_distinct(local)) %>%
  ungroup() %>%
  filter(nsites > (max(nsites) * 0.7)) %>%
  group_by(local) %>% 
  filter(n_distinct(year) == 3) %>% 
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
  # write_csv(paste0(path2dir, '/wardle_2014_plants_Simpson Desert', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level wardle_2014_reptile_Simpson Desert
rs_filtered %>% 
  filter(regional_level=='wardle_2014_reptile_Simpson Desert') %>%
  filter(year > 1995) %>% 
  group_by(year) %>% 
  mutate(nsites = n_distinct(local)) %>% 
  ungroup() %>% 
  filter(nsites > max(nsites) * 0.6) %>% 
  group_by(local) %>% 
  filter(n_distinct(year)==4) %>% 
  # write_csv(paste0(path2dir, '/wardle_2014_reptile_Simpson Desert', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

# regional level warren_2022_riparian
rs_filtered %>% 
  filter(regional_level=='warren_2022_riparian') %>%
  group_by(year) %>%
  mutate(nsites = n_distinct(local)) %>%
  ungroup() %>%
  filter(nsites == max(nsites)) %>%
  # write_csv(paste0(path2dir, '/warren_2022_riparian', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

rs_filtered %>% 
  filter(regional_level=='wright_2021_desert grassland') %>%
  group_by(year) %>%
  mutate(nsites = n_distinct(local)) %>%
  ungroup() %>%
  filter(nsites == max(nsites)) %>%
  # write_csv(paste0(path2dir, '/wright_2021_desert grassland', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))

rs_filtered %>% 
  filter(regional_level=='wright_2021_plain grasslands') %>%
  group_by(year) %>%
  mutate(nsites = n_distinct(local)) %>%
  ungroup() %>%
  filter(nsites == max(nsites)) %>%
  # write_csv(paste0(path2dir, '/wright_2021_plain grasslands', '.csv'))
  ggplot() +
  geom_point(aes(x = year, y = local))
