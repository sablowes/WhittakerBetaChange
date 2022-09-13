# ECN moth data
library(tidyverse)

path2wd <- '~/Dropbox/1current/spatial_composition_change/code/invertebrate_data/code_from_Roel/metacommunity files for Shane/'

enc_moth <- read_csv(paste0(path2wd, 'ECN 3 datasets for metacommunities/ECN_IM1(1).csv')) %>% 
  # remove zeroes now to prevent confusion later!
  filter(VALUE > 0)
enc_moth$date <- as.Date(enc_moth$SDATE, "%d-%b-%y")
enc_moth$month <- lubridate::month(enc_moth$date)
enc_moth$year <- lubridate::year(enc_moth$date)

# standardisation is more complicated for these data: effort varies due to the number of nights
# a trap was set
enc_moth_trap <- read_csv(paste0(path2wd, 'ECN 3 datasets for metacommunities/ECN_IM2.csv')) %>% 
  select(SITECODE, LCODE, SDATE, SPERIOD_D)
# the majority of samples had duration == 1 night 
ggplot() +
  facet_grid(LCODE~SITECODE) + 
  geom_histogram(data = enc_moth_trap,
                 aes(x = SPERIOD_D), binwidth = 1) +
  scale_x_continuous(trans = 'log2')

# put the sample duration into the data
enc_moth <- left_join(enc_moth, enc_moth_trap)

# visual inspection of the spatial and temporal distribution of sampling effort
# only two sites have a second location: join site_loc as 'alpha' scale; regional scale is all site_locs 
# (drop site T12, only added in later years)
ggplot() +
  facet_grid(LCODE~SITECODE) + 
  geom_point(data = enc_moth %>% 
               distinct(SITECODE, LCODE, year, month),
             aes(x = year, y = month))

enc_moth <- enc_moth %>% 
  unite(site_loc, c(SITECODE, LCODE))

# retain site_loc with sampling to start in 1996 and end in 2009
# this makes a temporal match to the butterfly data used 
enc_moth %>% 
  group_by(site_loc) %>% 
  summarise(start = min(year),
            end = max(year),
            duration = end - start + 1) 

ggplot() +
  facet_wrap(~site_loc, nrow = 3) + 
  geom_point(data = enc_moth %>% 
               # using only samples with the equal effort simplifies following steps
               filter(SPERIOD_D==1) %>% 
               distinct(site_loc, year, month) %>% 
               filter(site_loc!='T02_01' & site_loc!='T04_01' & site_loc!='T10_01' & site_loc!='T10_02' &
                        site_loc!='T04_03' & site_loc!='T10_03' & site_loc!='T12_01'  & site_loc!='T07_01' & 
                        (month %in% c(7,8,9,10))) %>%# 
               filter(year > 1995 & year < 2010) %>%
               mutate(limit = ifelse((year==1996 | year==2009), 'limit', 'middle')),
             aes(x = year, y = month, colour = limit))#


# count samples per site_loc / year / month: how many days per month?
# unequal effort: simplest to get one day per month (for months 6-9 inclusive)
effort_per_month <- enc_moth %>% 
  # same duration 
  filter(SPERIOD_D==1) %>% 
  group_by(site_loc, year, month) %>% 
  summarise(samples_per_month  = n_distinct(SDATE)) %>% 
  ungroup() 


# find the minimum number of days per month
range(effort_per_month$samples_per_month)

# reduce site_loc's and year / months to standardise effort via resampling
# drop the two site_loc without required data
sites <- enc_moth %>% 
  distinct(site_loc) %>% 
  filter(site_loc!='T02_01' & site_loc!='T04_01' & site_loc!='T10_01' & site_loc!='T10_02' &
           site_loc!='T04_03' & site_loc!='T10_03' & site_loc!='T12_01'  & site_loc!='T07_01') %>% 
  pull()

enc_moth_filtered <- enc_moth %>% 
  filter(site_loc %in% sites & (year >=1996 & year <= 2009) & (month %in% c(7,8,9,10))) %>% 
  filter(SPERIOD_D==1) %>% 
  # date denotes a discrete sampling event
  group_by(site_loc, year, month, date) %>% 
  select(-SDATE) %>% 
  nest(data = c(FIELDNAME, VALUE)) %>% 
  ungroup()

# reduce to years with samples in all months
enc_moth_filtered <- enc_moth_filtered %>% 
  group_by(site_loc, year) %>% 
  filter(length(unique(month)) > 3) %>% 
  ungroup()


n_resamp = 201
all_resamps = tibble()

# suppress summarise statement (so counter is visible)
options(dplyr.summarise.inform = FALSE)

for(s in 1:length(sites)){
  print(paste('site ', s, 'in ', length(sites)))
  # get the focal site
  site = enc_moth_filtered %>% 
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
enc_moths_resamps <- all_resamps %>% 
  mutate(region = 'enc_moths')

save(localS, regionalS_allresamps, regionalS, enc_moths_resamps,
     file = '~/Dropbox/1current/spatial_composition_change/data/enc_moths-multiyr.Rdata')

# use these data for codes if needed
sites <- read_csv(paste0(path2wd, 'ECN 3 datasets for metacommunities/sites.csv')) %>% 
  rename(SITECODE = `Site code`)

spp_names <- read_csv(paste0(path2wd, 'ECN 3 datasets for metacommunities/species names.csv')) %>% 
  rename(FIELDCODE = `Species code`,
         latin = `Latin name`,
         common = `Common name`)
