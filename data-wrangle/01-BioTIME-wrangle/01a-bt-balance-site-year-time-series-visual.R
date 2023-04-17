library(tidyverse)

##	Get the raw data 
bt <- read_csv('~/Dropbox/BioTIMELatest/BioTIMEQJune2021.csv')
btmeta <- read_csv('~/Dropbox/BioTIMELatest/bioTIMEMetadataJune2021.csv')

# only want count data for these analyses (need relative abundance)
count <- btmeta %>% 
  filter(ABUNDANCE_TYPE!='Presence/Absence')

bt <- bt %>% 
  filter(STUDY_ID %in% count$STUDY_ID)

# and the locations used when only two time points are in the analysis
load('~/Dropbox/1current/spatial_composition_change/data/bt-location-plot-approach-Sept22.Rdata')

site_2time <- bt_filtered %>% 
  filter(fYear=='start') %>% 
  distinct(loc_plot)

# study 10
bt %>% 
  filter(STUDY_ID==10) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  group_by(loc_plot) %>%
  filter(n_distinct(YEAR) == 3) %>%
  # # group_by(YEAR) %>% 
  # summarise(n_site = n_distinct(loc_plot)) 
  # select(YEAR, loc_plot) %>% write_csv('~/Desktop/bt_10_loc_plot-multiyr.csv')
  ggplot() +
  geom_point(aes(x = YEAR, y = loc_plot))

# study 44
sites <- bt %>% 
  filter(STUDY_ID==44) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  filter(YEAR==1977) 

bt %>% 
  filter(STUDY_ID==44) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  filter(loc_plot %in% sites$loc_plot) %>% 
  filter(!YEAR %in% 1966:1967) %>% 
  # group_by(YEAR) %>%
  # summarise(n_site = n_distinct(loc_plot)) 
  # select(YEAR, loc_plot) %>% write_csv('~/Desktop/bt_44_loc_plot-multiyr.csv')
  ggplot() +
  geom_point(aes(x = YEAR, y = loc_plot))

# study 67
bt %>% 
  filter(STUDY_ID==67) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  filter(loc_plot %in% site_2time$loc_plot) %>%
  filter(YEAR > 1992) %>%
  group_by(loc_plot) %>% 
  filter(n_distinct(YEAR)==14) %>% 
  # group_by(YEAR) %>%
  # summarise(n_site = n_distinct(loc_plot)) 
  # select(YEAR, loc_plot) %>% write_csv('~/Desktop/bt_67_loc_plot-multiyr.csv')
  ggplot() +
  geom_point(aes(x = YEAR, y = loc_plot))

# study 85
bt %>% 
  filter(STUDY_ID==85) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  filter(loc_plot %in% site_2time$loc_plot) %>%
  filter(YEAR != 1996) %>%
  # group_by(loc_plot) %>% 
  # filter(n_distinct(YEAR)==14) %>% 
  # group_by(YEAR) %>%
  # summarise(n_site = n_distinct(loc_plot)) 
  # select(YEAR, loc_plot) %>% write_csv('~/Desktop/bt_85_loc_plot-multiyr.csv')
  ggplot() +
  geom_point(aes(x = YEAR, y = loc_plot))

# study 112
bt %>% 
  filter(STUDY_ID==112) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  filter(loc_plot %in% site_2time$loc_plot) %>%
  filter(YEAR > 1991) %>%
  group_by(loc_plot) %>%
  filter(n_distinct(YEAR) > 12) %>%
  filter(!YEAR %in% c(1993, 1996, 2005)) %>% 
  # group_by(YEAR) %>%
  # summarise(n_site = n_distinct(loc_plot))
  # select(YEAR, loc_plot) %>% write_csv('~/Desktop/bt_112_loc_plot-multiyr.csv')
  ggplot() +
  geom_point(aes(x = YEAR, y = loc_plot))


# study 121
bt %>% 
  filter(STUDY_ID==121) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  filter(loc_plot %in% site_2time$loc_plot) %>%
  filter(!YEAR %in% c(2000, 2003)) %>%
  # group_by(loc_plot) %>%
  # filter(n_distinct(YEAR) > 12) %>%
  # filter(!YEAR %in% c(1993, 1996, 2005)) %>% 
  # group_by(YEAR) %>%
  # summarise(n_site = n_distinct(loc_plot))
  # select(YEAR, loc_plot) %>% write_csv('~/Desktop/bt_121_loc_plot-multiyr.csv')
  ggplot() +
  geom_point(aes(x = YEAR, y = loc_plot))

# study 125
bt %>% 
  filter(STUDY_ID==125) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  filter(loc_plot %in% site_2time$loc_plot) %>%
  group_by(loc_plot) %>%
  filter(n_distinct(YEAR) == 12) %>%
  # group_by(YEAR) %>%
  # summarise(n_site = n_distinct(loc_plot))
  # select(YEAR, loc_plot) %>% write_csv('~/Desktop/bt_125_loc_plot-multiyr.csv')
  ggplot() +
  geom_point(aes(x = YEAR, y = loc_plot))

# study 152
bt %>% 
  filter(STUDY_ID==152) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  filter(loc_plot %in% site_2time$loc_plot) %>%
  filter(YEAR %in% c(1979, 1982, 1985, 1988)) %>% 
  group_by(loc_plot) %>%
  filter(n_distinct(YEAR) ==4 ) %>%
  # group_by(YEAR) %>%
  # summarise(n_site = n_distinct(loc_plot))
  # select(YEAR, loc_plot) %>% write_csv('~/Desktop/bt_152_loc_plot-multiyr.csv')
  ggplot() +
  geom_point(aes(x = YEAR, y = loc_plot))

# study 176
bt %>% 
  filter(STUDY_ID==176) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  filter(loc_plot %in% site_2time$loc_plot) %>%
  filter(YEAR %in% c(2000, 2001, 2006:2009)) %>%
  group_by(loc_plot) %>%
  filter(n_distinct(YEAR) == 6) %>%
  # group_by(YEAR) %>%
  # summarise(n_site = n_distinct(loc_plot))
  # select(YEAR, loc_plot) %>% write_csv('~/Desktop/bt_176_loc_plot-multiyr.csv')
  ggplot() +
  geom_point(aes(x = YEAR, y = loc_plot))


# study 192
# how many sites per year
bt %>% 
  filter(STUDY_ID==192) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  filter(loc_plot %in% site_2time$loc_plot) %>%
  group_by(YEAR) %>% 
  summarise(n_sites = n_distinct(loc_plot)) %>% 
  ungroup() %>% 
  arrange(desc(n_sites))

bt %>% 
  filter(STUDY_ID==192) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  filter(loc_plot %in% site_2time$loc_plot) %>%
  group_by(YEAR) %>%
  filter(n_distinct(loc_plot) > 37) %>% 
  ungroup() %>% 
  filter(YEAR %in% c(1934, 1939, 1948, 1952, 1955, 1959)) %>% 
  group_by(loc_plot) %>%
  filter(n_distinct(YEAR) > 5) %>%
  # group_by(YEAR) %>%
  # summarise(n_site = n_distinct(loc_plot))
  # select(YEAR, loc_plot) %>% write_csv('~/Desktop/bt_192_loc_plot-multiyr.csv')
  ggplot() +
  geom_point(aes(x = YEAR, y = loc_plot))


# study id 194
# sites in last year of time series
sites_2004 <- bt %>% 
  filter(STUDY_ID==194) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  filter(YEAR==2004) %>% 
  distinct(loc_plot)

bt %>% 
  filter(STUDY_ID==194) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  filter(loc_plot %in% sites_2004$loc_plot) %>% 
  filter(YEAR %in% c(1994, 1996, 2004)) %>% 
  group_by(loc_plot) %>%
  filter(n_distinct(YEAR) == 3) %>%
  # select(YEAR, loc_plot) %>% write_csv('~/Desktop/bt_194_loc_plot-multiyr.csv')
  ggplot() +
  geom_point(aes(x = YEAR, y = loc_plot))

# study 198
bt %>% 
  filter(STUDY_ID==198) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  filter(loc_plot %in% site_2time$loc_plot) %>% 
  filter(YEAR %in% c(1991, 1994, 2001)) %>%
  group_by(loc_plot) %>%
  filter(n_distinct(YEAR) == 3) %>%
  # select(YEAR, loc_plot) %>% write_csv('~/Desktop/bt_198_loc_plot-multiyr.csv')
  ggplot() +
  geom_point(aes(x = YEAR, y = loc_plot))

# study 200
bt %>% 
  filter(STUDY_ID==200) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  filter(loc_plot %in% site_2time$loc_plot) %>% 
  filter(YEAR %in% c(1973, 1974, 1980:1983)) %>%
  group_by(loc_plot) %>%
  filter(n_distinct(YEAR) == 4) %>%
  # select(YEAR, loc_plot) %>% write_csv('~/Desktop/bt_200_loc_plot-multiyr.csv')
  ggplot() +
  geom_point(aes(x = YEAR, y = loc_plot))

# study 209
bt %>% 
  filter(STUDY_ID==209) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  filter(loc_plot %in% site_2time$loc_plot) %>% 
  # filter(YEAR %in% c(1973, 1974, 1980:1983)) %>%
  # group_by(loc_plot) %>%
  # filter(n_distinct(YEAR) == 4) %>%
  # select(YEAR, loc_plot) %>% write_csv('~/Desktop/bt_200_loc_plot-multiyr.csv')
  ggplot() +
  geom_point(aes(x = YEAR, y = loc_plot))

# study 210
bt %>% 
  filter(STUDY_ID==210) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  filter(loc_plot %in% site_2time$loc_plot) %>%
  filter(YEAR > 1990) %>% 
  group_by(loc_plot) %>%
  filter(n_distinct(YEAR) > 8) %>%
  filter(YEAR %in% c(1991, 1994,  2001)) %>%
  # select(YEAR, loc_plot) %>% write_csv('~/Desktop/bt_210_loc_plot-multiyr.csv')
  ggplot() +
  geom_point(aes(x = YEAR, y = loc_plot))

# study 214
bt %>% 
  filter(STUDY_ID==214) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  filter(loc_plot %in% site_2time$loc_plot) %>% 
  filter(YEAR > 1982 & YEAR < 1995) %>% 
  group_by(loc_plot) %>%
  filter(n_distinct(YEAR) > 8) %>%
  # ungroup() %>% 
  filter(!YEAR %in% c(1996, 1998:2000, 2004:2005)) %>%
  # group_by(loc_plot) %>%
  # filter(n_distinct(YEAR) == 6) %>% 
  # select(YEAR, loc_plot) %>% write_csv('~/Desktop/bt_214_loc_plot-multiyr.csv')
  ggplot() +
  geom_point(aes(x = YEAR, y = loc_plot))

# study 215
bt %>% 
  filter(STUDY_ID==215) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  filter(loc_plot %in% site_2time$loc_plot) %>% 
  filter(YEAR > 1991 & loc_plot!='-98.38056_26.16667_NA') %>%
  filter(!YEAR %in% c(1996, 1998:2000, 2004:2005)) %>%
  # group_by(loc_plot) %>%
  # filter(n_distinct(YEAR) == 6) %>% 
  # select(YEAR, loc_plot) %>% write_csv('~/Desktop/bt_215_loc_plot-multiyr.csv')
ggplot() +
  geom_point(aes(x = YEAR, y = loc_plot))

# study 217
bt %>% 
  filter(STUDY_ID==217) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  filter(loc_plot %in% site_2time$loc_plot) %>% 
  filter(YEAR %in% c(1994, 1995, 1996, 1998, 2000, 2004)) %>%
  group_by(loc_plot) %>% 
  filter(n_distinct(YEAR) == 6) %>% 
  # ungroup() %>% 
  # group_by(YEAR) %>% 
  # summarise(n_sites = n_distinct(loc_plot))
  # select(YEAR, loc_plot) %>% write_csv('~/Desktop/bt_217_loc_plot-multiyr.csv')
  ggplot() +
  geom_point(aes(x = YEAR, y = loc_plot))

# study 219
bt %>% 
  filter(STUDY_ID==219) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  filter(loc_plot %in% site_2time$loc_plot) %>% 
  filter(!YEAR %in% c(1995, 1996, 2003, 2007, 2011)) %>%
  group_by(loc_plot) %>%
  filter(n_distinct(YEAR) == 12) %>%
  ungroup() %>%
  # group_by(YEAR) %>%
  # summarise(n_sites = n_distinct(loc_plot))
  # select(YEAR, loc_plot) %>% write_csv('~/Desktop/bt_219_loc_plot-multiyr.csv')
  ggplot() +
  geom_point(aes(x = YEAR, y = loc_plot))

# study 220
bt %>% 
  filter(STUDY_ID==220) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  filter(loc_plot %in% site_2time$loc_plot) %>% 
  filter(!YEAR %in% c(1995, 1996, 2000, 2002, 2003, 2007, 2008, 2011)) %>%
  group_by(loc_plot) %>%
  filter(n_distinct(YEAR) == 9) %>%
  ungroup() %>%
  # group_by(YEAR) %>%
  # summarise(n_sites = n_distinct(loc_plot))
  # select(YEAR, loc_plot) %>% write_csv('~/Desktop/bt_220_loc_plot-multiyr.csv')
  ggplot() +
  geom_point(aes(x = YEAR, y = loc_plot))

# study 221
bt %>% 
  filter(STUDY_ID==221) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  filter(loc_plot %in% site_2time$loc_plot) %>% 
  filter(YEAR  %in% c(1988, 1991, 1993, 2003)) %>%
  group_by(loc_plot) %>%
  filter(n_distinct(YEAR) == 4) %>%
  ungroup() %>%
  # group_by(YEAR) %>%
  # summarise(n_sites = n_distinct(loc_plot))
  # select(YEAR, loc_plot) %>% write_csv('~/Desktop/bt_221_loc_plot-multiyr.csv')
  ggplot() +
  geom_point(aes(x = YEAR, y = loc_plot))

# study 252
bt %>% 
  filter(STUDY_ID==252) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  filter(loc_plot %in% site_2time$loc_plot) %>% 
  filter(YEAR  %in% c(1979, 1984, 1985, 1988)) %>%
  group_by(loc_plot) %>%
  filter(n_distinct(YEAR) == 4) %>%
  ungroup() %>%
  # group_by(YEAR) %>%
  # summarise(n_sites = n_distinct(loc_plot))
  # select(YEAR, loc_plot) %>% write_csv('~/Desktop/bt_252_loc_plot-multiyr.csv')
  ggplot() +
  geom_point(aes(x = YEAR, y = loc_plot))

# study 252
bt %>% 
  filter(STUDY_ID==252) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  filter(loc_plot %in% site_2time$loc_plot) %>% 
  filter(YEAR  %in% c(1979, 1984, 1985, 1988)) %>%
  group_by(loc_plot) %>%
  filter(n_distinct(YEAR) == 4) %>%
  ungroup() %>%
  # group_by(YEAR) %>%
  # summarise(n_sites = n_distinct(loc_plot))
  # select(YEAR, loc_plot) %>% write_csv('~/Desktop/bt_252_loc_plot-multiyr.csv')
  ggplot() +
  geom_point(aes(x = YEAR, y = loc_plot))

# study 255
bt %>% 
  filter(STUDY_ID==255) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  filter(loc_plot %in% site_2time$loc_plot) %>% 
  # group_by(YEAR) %>%
  # summarise(n_sites = n_distinct(loc_plot))
  # select(YEAR, loc_plot) %>% write_csv('~/Desktop/bt_255_loc_plot-multiyr.csv')
  ggplot() +
  geom_point(aes(x = YEAR, y = loc_plot))

# study 277
bt %>% 
  filter(STUDY_ID==277) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  filter(loc_plot %in% site_2time$loc_plot) %>% 
  filter(!YEAR %in% 1980:1981) %>% 
  group_by(loc_plot) %>% 
  filter(n_distinct(YEAR)==8) %>%
  ungroup() %>% 
  # group_by(YEAR) %>%
  # summarise(n_sites = n_distinct(loc_plot))
  # select(YEAR, loc_plot) %>% write_csv('~/Desktop/bt_277_loc_plot-multiyr.csv')
  ggplot() +
  geom_point(aes(x = YEAR, y = loc_plot))

# study 279
bt %>% 
  filter(STUDY_ID==279) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  filter(loc_plot %in% site_2time$loc_plot) %>% 
  filter(!YEAR %in% 1980:1981) %>% 
  group_by(loc_plot) %>% 
  filter(n_distinct(YEAR)==8) %>%
  ungroup() %>% 
  # group_by(YEAR) %>%
  # summarise(n_sites = n_distinct(loc_plot))
  # select(YEAR, loc_plot) %>% write_csv('~/Desktop/bt_279_loc_plot-multiyr.csv')
  ggplot() +
  geom_point(aes(x = YEAR, y = loc_plot))

# study 294
bt %>% 
  filter(STUDY_ID==294) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  filter(loc_plot %in% site_2time$loc_plot) %>%
  # filter(YEAR < 1994) %>%
  # group_by(loc_plot) %>%
  # filter(n_distinct(YEAR) > 2) %>%
  # ungroup() %>% 
  # group_by(YEAR) %>%
  # summarise(n_sites = n_distinct(loc_plot))
  # select(YEAR, loc_plot) %>% write_csv('~/Desktop/bt_294_loc_plot-multiyr.csv')
  ggplot() +
  geom_point(aes(x = YEAR, y = loc_plot))

# study 297
bt %>% 
  filter(STUDY_ID==297) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  filter(loc_plot %in% site_2time$loc_plot) %>%
  filter(!YEAR %in% c(2006:2008, 2010)) %>%
  # group_by(YEAR) %>%
  # summarise(n_sites = n_distinct(loc_plot))
  # select(YEAR, loc_plot) %>% write_csv('~/Desktop/bt_297_loc_plot-multiyr.csv')
  ggplot() +
  geom_point(aes(x = YEAR, y = loc_plot))

# study 321
bt %>% 
  filter(STUDY_ID==321) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  filter(loc_plot %in% site_2time$loc_plot) %>%
  filter(!YEAR %in% c(1995, 2006)) %>%
  group_by(loc_plot) %>%
  filter(n_distinct(YEAR)==11) %>% 
  # group_by(YEAR) %>% 
  # summarise(n_sites = n_distinct(loc_plot))
  # select(YEAR, loc_plot) %>% write_csv('~/Desktop/bt_321_loc_plot-multiyr.csv')
  ggplot() +
  geom_point(aes(x = YEAR, y = loc_plot))

# study 321
bt %>% 
  filter(STUDY_ID==321) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  filter(loc_plot %in% site_2time$loc_plot) %>%
  filter(!YEAR %in% c(1995, 2006)) %>%
  group_by(loc_plot) %>%
  filter(n_distinct(YEAR)==11) %>% 
  # group_by(YEAR) %>% 
  # summarise(n_sites = n_distinct(loc_plot))
  # select(YEAR, loc_plot) %>% write_csv('~/Desktop/bt_321_loc_plot-multiyr.csv')
  ggplot() +
  geom_point(aes(x = YEAR, y = loc_plot))

# study 331
bt %>% 
  filter(STUDY_ID==331) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  filter(loc_plot %in% site_2time$loc_plot) %>%
  # group_by(YEAR) %>% 
  # summarise(n_distinct(loc_plot))
  # select(YEAR, loc_plot) %>% write_csv('~/Desktop/bt_331_loc_plot-multiyr.csv')
  ggplot() +
  geom_point(aes(x = YEAR, y = loc_plot))

# study 356
bt %>% 
  filter(STUDY_ID==356) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  filter(loc_plot %in% site_2time$loc_plot) %>%
  group_by(YEAR) %>%
  filter(n_distinct(loc_plot)==9) %>%
  # summarise(n_distinct(loc_plot))
  # select(YEAR, loc_plot) %>% write_csv('~/Desktop/bt_356_loc_plot-multiyr.csv')
  ggplot() +
  geom_point(aes(x = YEAR, y = loc_plot))

# study 375
bt %>% 
  filter(STUDY_ID==375) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  filter(loc_plot %in% site_2time$loc_plot) %>%
  filter(!YEAR %in% c(2006, 2008:2010)) %>% 
  # group_by(YEAR) %>%
  # summarise(n_distinct(loc_plot))
  # select(YEAR, loc_plot) %>% write_csv('~/Desktop/bt_375_loc_plot-multiyr.csv')
  ggplot() +
  geom_point(aes(x = YEAR, y = loc_plot))

# study 415
bt %>% 
  filter(STUDY_ID==415) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  filter(loc_plot %in% site_2time$loc_plot) %>%
  # filter(!YEAR %in% c(2006, 2008:2010)) %>% 
  # group_by(YEAR) %>%
  # summarise(n_distinct(loc_plot))
  # select(YEAR, loc_plot) %>% write_csv('~/Desktop/bt_415_loc_plot-multiyr.csv')
  ggplot() +
  geom_point(aes(x = YEAR, y = loc_plot))

# study 433
bt %>% 
  filter(STUDY_ID==433) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  filter(loc_plot %in% site_2time$loc_plot) %>%
  filter(YEAR %in% c(1989, 1992:1995, 2001)) %>% 
  group_by(loc_plot) %>%
  filter(n_distinct(YEAR) == 6) %>%
  # summarise(n_distinct(loc_plot))
  # select(YEAR, loc_plot) %>% write_csv('~/Desktop/bt_433_loc_plot-multiyr.csv')
  ggplot() +
  geom_point(aes(x = YEAR, y = loc_plot))

# study 521
bt %>% 
  filter(STUDY_ID==521) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  filter(loc_plot %in% site_2time$loc_plot) %>%
  filter(YEAR > 1972) %>%
  group_by(loc_plot) %>%
  filter(n_distinct(YEAR) > 35) %>%
  # group_by(YEAR) %>% 
  # summarise(n_site = n_distinct(loc_plot)) %>% filter(n_site != 5)
  # select(YEAR, loc_plot) %>% write_csv('~/Desktop/bt_521_loc_plot-multiyr.csv')
  ggplot() +
  geom_point(aes(x = YEAR, y = loc_plot))

# study 527
bt %>% 
  filter(STUDY_ID==527) %>% 
  distinct(YEAR, LONGITUDE, LATITUDE, PLOT) %>%
  unite(loc_plot,  c(LONGITUDE, LATITUDE, PLOT)) %>%
  filter(loc_plot %in% site_2time$loc_plot) %>%
  filter(YEAR %in% c(2006, 2009, 2015)) %>% 
  group_by(loc_plot) %>%
  filter(n_distinct(YEAR) == 3) %>%
  # # group_by(YEAR) %>% 
  # summarise(n_site = n_distinct(loc_plot)) %>% filter(n_site != 5)
  # select(YEAR, loc_plot) %>% write_csv('~/Desktop/bt_527_loc_plot-multiyr.csv')
  ggplot() +
  geom_point(aes(x = YEAR, y = loc_plot))
