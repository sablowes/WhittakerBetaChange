# manual processing of invert sampling sites and years
load('~/Dropbox/1current/spatial_composition_change/data/invert-location-plots-maxRect.Rdata')

##	Get the raw data (thesee are the data published in Van Klink et al 2020 Ecology)
inverts <- readRDS('~/Dropbox/1current/spatial_composition_change/data/Raw data insect metacommunities.Rdata')
inverts <- inverts %>% 
  as_tibble() %>% 
  # remove the zeroes 
  filter(Number > 0) 

# datasource 1102
invert_filtered %>% 
  filter(Datasource_ID==1102) %>% 
  group_by(Year) %>% 
  mutate(nplots = n_distinct(loc_plot)) %>% 
  ungroup() %>% 
  filter(nplots == max(nplots)) %>% 
  distinct(Year, loc_plot) %>% 
  # write_csv('~/Dropbox/1current/spatial_composition_change/WhittakerBetaChange/data-wrangle/01-Invertebrate-wrangle/balanced-site-years/invert_1102_loc_plot_multiyr.csv')
  ggplot() +
  geom_point(aes(x = Year, y = loc_plot))

# datasource 1340
invert_filtered %>% 
  filter(Datasource_ID==1340) %>% 
  group_by(Year) %>% 
  mutate(nplots = n_distinct(loc_plot)) %>% 
  ungroup() %>% 
  filter(nplots == max(nplots)) %>% 
  distinct(Year, loc_plot) %>% 
  # write_csv('~/Dropbox/1current/spatial_composition_change/WhittakerBetaChange/data-wrangle/01-Invertebrate-wrangle/balanced-site-years/invert_1340_loc_plot_multiyr.csv')
  ggplot() +
  geom_point(aes(x = Year, y = loc_plot))

# datasource 1347
inverts %>% 
  filter(Datasource_ID==1347) %>% 
  unite(loc_plot, c(Plot_ID), remove = F) %>% 
  group_by(Year) %>%
  mutate(nplots = n_distinct(loc_plot)) %>%
  ungroup() %>%
  filter(nplots > 8) %>%
  group_by(loc_plot) %>% 
  mutate(nyrs = n_distinct(Year)) %>% 
  ungroup() %>% 
  filter(nyrs > (max(nyrs) -3)) %>% 
  filter(Year %in% c(1985, 2011, 2015, 2017, 2018)) %>% 
  distinct(Year, loc_plot) %>% 
  # write_csv('~/Dropbox/1current/spatial_composition_change/WhittakerBetaChange/data-wrangle/01-Invertebrate-wrangle/balanced-site-years/invert_1347_loc_plot_multiyr.csv')
  ggplot() +
  geom_point(aes(x = Year, y = loc_plot))

# datasource 1361
inverts %>% 
  filter(Datasource_ID==1361) %>% 
  unite(loc_plot, c(Plot_ID), remove = F) %>% 
  group_by(Year) %>%
  mutate(nplots = n_distinct(loc_plot)) %>%
  ungroup() %>%
  filter(nplots == 6) %>%
  # group_by(loc_plot) %>% 
  # mutate(nyrs = n_distinct(Year)) %>% 
  # ungroup() %>% 
  # filter(nyrs > (max(nyrs) -3)) %>% 
  # filter(Year %in% c(1985, 2011, 2015, 2017, 2018)) %>% 
  distinct(Year, loc_plot) %>% 
  # write_csv('~/Dropbox/1current/spatial_composition_change/WhittakerBetaChange/data-wrangle/01-Invertebrate-wrangle/balanced-site-years/invert_1361_loc_plot_multiyr.csv')
  ggplot() +
  geom_point(aes(x = Year, y = loc_plot))

# datasource 1444
inverts %>% 
  filter(Datasource_ID==1444) %>% 
  unite(loc_plot, c(Plot_ID), remove = F) %>% 
  # group_by(Year) %>%
  # mutate(nplots = n_distinct(loc_plot)) %>%
  # ungroup() %>%
  # filter(nplots > (max(nplots)/2)) %>%
  group_by(loc_plot) %>%
  mutate(nyrs = n_distinct(Year)) %>%
  ungroup() %>%
  filter(nyrs > (max(nyrs)-2)) %>%
  filter(Year %in% c(1993:1994, 1998, 2002, 2008)) %>%
  distinct(Year, loc_plot) %>% 
  # write_csv('~/Dropbox/1current/spatial_composition_change/WhittakerBetaChange/data-wrangle/01-Invertebrate-wrangle/balanced-site-years/invert_1444_loc_plot_multiyr.csv')
  ggplot() +
  geom_point(aes(x = Year, y = loc_plot))

# datasource 1488
inverts %>% 
  filter(Datasource_ID==1488) %>% 
  unite(loc_plot, c(Plot_ID), remove = F) %>% 
  filter(Year > 2005 & Year!=2017) %>% 
  # group_by(Year) %>%
  # mutate(nplots = n_distinct(loc_plot)) %>%
  # ungroup() %>%
  # filter(nplots > (max(nplots)/2)) %>%
  group_by(loc_plot) %>%
  mutate(nyrs = n_distinct(Year)) %>%
  ungroup() %>%
  filter(nyrs > (max(nyrs)-3)) %>%
  filter(Year %in% c(2007:2010, 2015, 2019)) %>%
  distinct(Year, loc_plot) %>%
  # write_csv('~/Dropbox/1current/spatial_composition_change/WhittakerBetaChange/data-wrangle/01-Invertebrate-wrangle/balanced-site-years/invert_1488_loc_plot_multiyr.csv')
  ggplot() +
  geom_point(aes(x = Year, y = loc_plot))

# datasource 1542
inverts %>% 
  filter(Datasource_ID==1542) %>% 
  unite(loc_plot, c(Plot_ID), remove = F) %>% 
  filter(!Year %in% c(2001, 2019:2020)) %>%
  group_by(Year) %>%
  mutate(nplots = n_distinct(loc_plot)) %>%
  ungroup() %>%
  filter(nplots > (max(nplots)/2)) %>%
  group_by(loc_plot) %>%
  mutate(nyrs = n_distinct(Year)) %>%
  ungroup() %>%
  filter(nyrs > 7) %>%
  # filter(Year %in% c(2007:2010, 2015, 2019)) %>%
  distinct(Year, loc_plot) %>%
  # write_csv('~/Dropbox/1current/spatial_composition_change/WhittakerBetaChange/data-wrangle/01-Invertebrate-wrangle/balanced-site-years/invert_1542_loc_plot_multiyr.csv')
  ggplot() +
  geom_point(aes(x = Year, y = loc_plot))
