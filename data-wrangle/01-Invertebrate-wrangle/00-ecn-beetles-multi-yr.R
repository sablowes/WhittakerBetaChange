# ENC beetles for primary homogenisation analysis

library(tidyverse)

path2wd <- '~/Dropbox/1current/spatial_composition_change/code/invertebrate_data/code_from_Roel/metacommunity files for Shane/'

enc_beetle <- read_csv(paste0(path2wd, 'ECN 3 datasets for metacommunities/ECN ground beetles standardized.csv')) %>% 
  # remove zeroes now to prevent confusion later!
  filter(VALUE > 0)
enc_beetle$date <- as.Date(enc_beetle$SDATE, "%d-%b-%y")
enc_beetle$month <- lubridate::month(enc_beetle$date)
enc_beetle$year <- lubridate::year(enc_beetle$date)

# visual inspection of the spatial and temporal distribution of sampling effort
# only two sites have a second location: join site_loc as 'alpha' scale; regional scale is all site_locs 
# (drop site T12, only added in later years)
# ggplot() +
#   facet_grid(LCODE~SITECODE) + 
#   geom_point(data = enc_beetle %>% 
#                distinct(SITECODE, LCODE, year, month),
#              aes(x = year, y = month))

enc_beetle <- enc_beetle %>% 
  unite(site_loc, c(SITECODE, LCODE))

# retain 12 site_loc with sampling to start in 1996 and end in 2009
# 
enc_beetle %>% 
  group_by(site_loc) %>% 
  summarise(start = min(year),
            end = max(year),
            # duration of time series
            duration = end - start + 1) 

# duration (already in dataframe from Roel) is the number of days that the trap was open (i.e., sample effort)
# 14 days is the frequent duration (overall and within sites)

ggplot() +
  # facet_wrap(~site_loc) +
  geom_histogram(data = enc_beetle,
                 aes(x = duration))
ggplot() +
  facet_wrap(~site_loc) + 
  geom_point(data = enc_beetle %>% 
               filter(duration == 14) %>%
               distinct(site_loc, year, month) %>%
               filter(site_loc!='T02_1' & site_loc!='T02_2' & site_loc!='T02_3') %>%
               filter(site_loc!='T03_1' & site_loc!='T03_2') %>%
               filter(site_loc!='T04_1' & site_loc!='T04_2' & site_loc!='T04_3' & site_loc!='T04_4') %>%
               filter(site_loc!='T06_1' & site_loc!='T06_2' & site_loc!='T06_3' & site_loc!='T07_1' & site_loc!='T07_2' & site_loc!='T07_3') %>%
               filter(site_loc!='T11_1' & site_loc!='T11_2' & site_loc!='T11_3') %>%
               filter(site_loc!='T12_1' & site_loc!='T12_2' & site_loc!='T12_3' & site_loc!='T12_4' & site_loc!='T12_5' &
                        site_loc!='T12_6' & (month %in% c(5,6,9))) %>% #
               filter(year > 1995 & year < 2010) %>%
               mutate(limit = ifelse((year==1996 | year==2009), 'limit', 'middle')),
             aes(x = year, y = month, colour = limit))#


# count samples per site_loc / year / month: how many days per month?
# unequal effort: simplest to get one day per month (for months 6-9 inclusive)
effort_per_month <- enc_beetle %>% 
  # remove zeroes
  filter(VALUE > 0) %>% 
  # only want the samples where the trap was out for 14 days
  filter(duration==14) %>% 
  group_by(site_loc, year, month) %>% 
  summarise(samples_per_month  = n_distinct(SDATE)) %>% 
  ungroup() 

# find the minimum number of days per month
range(effort_per_month$samples_per_month)

# reduce site_loc's and year / months to standardise effort via resampling
# drop the two site_loc without required data
sites <- enc_beetle %>% 
  filter(site_loc!='T02_1' & site_loc!='T02_2' & site_loc!='T02_3') %>% 
  filter(site_loc!='T03_1' & site_loc!='T03_2') %>% 
  filter(site_loc!='T04_1' & site_loc!='T04_2' & site_loc!='T04_3' & site_loc!='T04_4') %>% 
  filter(site_loc!='T06_1' & site_loc!='T06_2' & site_loc!='T06_3' & site_loc!='T07_1' & site_loc!='T07_2' & site_loc!='T07_3') %>% 
  filter(site_loc!='T11_1' & site_loc!='T11_2' & site_loc!='T11_3') %>% 
  filter(site_loc!='T12_1' & site_loc!='T12_2' & site_loc!='T12_3' & site_loc!='T12_4' & site_loc!='T12_5' &
           site_loc!='T12_6') %>% 
  distinct(site_loc) %>% 
  pull()
  

enc_beetle_filtered <- enc_beetle %>% 
  # standardise duration
  filter(duration==14) %>% 
  # keep all years between 1996 & 2009
  filter(site_loc %in% sites & (year >= 1996 &  year <= 2009) & (month %in% c(5,6,9))) %>% 
  # some years are missing a month
  # date denotes a discrete sampling event
  group_by(site_loc, year, month, date) %>% 
  select(site_loc, year, month, date, FIELDNAME, VALUE) %>% 
  nest(data = c(FIELDNAME, VALUE)) %>% 
  ungroup()

# for multiyr average, only retain years with a sample in each of month 5, 6, 9
enc_beetle_filtered <- enc_beetle_filtered %>% 
  group_by(site_loc, year) %>% 
  filter(length(unique(month)) > 2) %>% 
  ungroup()

n_resamp = 200
all_resamps = tibble()

# suppress summarise statement (so counter is visible)
options(dplyr.summarise.inform = FALSE)

for(s in 1:length(sites)){
  print(paste('site ', s, 'in ', length(sites)))
  # get the focal site
  site = enc_beetle_filtered %>% 
    filter(site_loc==sites[s])
  
  for(r in 1:n_resamp){
    # print(paste(r, 'of ', n_resamp, ' resamples'))
    samp = site %>% 
      group_by(year, month) %>% 
      sample_n(1)
    
    sampS = samp %>% 
      unnest(data) %>% 
      # collate species within each year
      group_by(site_loc, year, FIELDNAME) %>% 
      summarise(N = sum(VALUE)) %>% 
      mutate(resamp = r)
    
    all_resamps = bind_rows(all_resamps, sampS)
  }
}

# calculate local and regional richness for the resamples
localS <- all_resamps %>% 
  group_by(site_loc, year, resamp) %>% 
  summarise(S = n_distinct(FIELDNAME),
            eH = exp(vegan::diversity(N, index = 'shannon')),
            S_PIE = vegan::diversity(N, index = 'invsimpson')) %>% 
  group_by(site_loc, year) %>% 
  summarise(Sbar = median(S),
            eHbar = median(eH),
            S_PIEbar = median(S_PIE)) %>% 
  ungroup()

regionalS_allresamps <- all_resamps %>% 
  # create regional SAD for each year and each resample
  group_by(year, resamp, FIELDNAME) %>% 
  summarise(N = sum(N)) %>% 
  # calculate regional richness for each resample; retain the resamples to use in conjunction with jack-knife resamps
  group_by(year, resamp) %>% 
  summarise(S = n_distinct(FIELDNAME),
            eH = exp(vegan::diversity(N, index = 'shannon')),
            S_PIE = vegan::diversity(N, index = 'invsimpson')) %>% 
  ungroup()

regionalS <- regionalS_allresamps %>% 
  group_by(year) %>% 
  summarise(S = median(S),
            eH = median(eH),
            S_PIE = median(S_PIE))

# rename all_resamps, we need these to calculate regional estimate for multiyr analysis
enc_beetle_resamps <- all_resamps %>% 
  mutate(region = 'enc_beetles')

save(localS, regionalS_allresamps, regionalS, enc_beetle_resamps,
     file = '~/Dropbox/1current/spatial_composition_change/data/enc_beetles-multiyr.Rdata')

# use these data for codes if needed
sites <- read_csv(paste0(path2wd, 'ECN 3 datasets for metacommunities/sites.csv')) %>% 
  rename(SITECODE = `Site code`)

spp_names <- read_csv(paste0(path2wd, 'ECN 3 datasets for metacommunities/species names.csv')) %>% 
  rename(FIELDCODE = `Species code`,
         latin = `Latin name`,
         common = `Common name`)