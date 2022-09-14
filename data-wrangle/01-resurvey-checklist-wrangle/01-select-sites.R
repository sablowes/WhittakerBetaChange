# code to select sites and years for analysis from the new 'homogenisation' database
# compiled by Alban xx and Jon Chase

# resurvey and checklist data are wrangled separately


library(tidyverse)

dat <- read_csv('~/Dropbox/BioTimeX/Local-Regional Homogenization/_data_extraction/communities.csv')
meta <- read_csv('~/Dropbox/BioTimeX/Local-Regional Homogenization/_data_extraction/metadata.csv')

# create new regional_level for resurvey data
dat_rs <- dat %>% 
  unite(regional_level, c(dataset_id, regional), remove = FALSE) %>% 
  left_join(meta %>% distinct(dataset_id, checklist)) %>% 
  filter(checklist==FALSE)

# identify unique years and locations within studies for the resurvey data 
rs_loc <- dat_rs %>% 
  distinct(dataset_id, regional, local, year) %>% 
  # count the number of sites per region per year
  group_by(dataset_id, regional, year) %>% 
  mutate(n_loc = n_distinct(local)) %>% 
  ungroup() %>% 
  # filter to years with at least 4 locations
  filter(n_loc >= 4) %>% 
  unite(regional_level, c(dataset_id, regional), remove = F)
  
# calculate number of locations, number of years and duration for each region
# remove regions with number of years < 2 and duration <10 years
# NB: this is meta data only (no species records)
meta_year <- rs_loc %>%
  group_by(regional_level, year) %>%
  summarise(n_loc = n_distinct(local)) %>%
  group_by(regional_level) %>% 
  mutate(total_loc = sum(n_loc), 
         mean_loc = round(mean(n_loc),1), 
         min_loc = min(n_loc), 
         max_loc = max(n_loc),
         n_years = n_distinct(year, na.rm = TRUE),
         duration = max(year) - min(year) +1) %>%
  ungroup() %>% 
  filter(n_years >=2 & duration >=10)

# clean meta data for all regional_levels
meta_new <- meta_year %>% 
  dplyr::select(-c(year, n_loc)) %>% 
  distinct()

# get the data corresponding to studies with number of years > 2 and duration > 10 years
rs_4loc_10yr <- rs_loc %>% 
  unite(col = rl_year, c(regional_level, year), remove = FALSE) %>%
  filter(rl_year %in% 
           (meta_year %>% 
              unite(col = rl_year, c(regional_level, year)) %>% 
              pull(rl_year))) %>%
  dplyr::select(-rl_year)

# loop de loop to optimise each region: want to maximise number of plots and duration 
rs_4loc_10yr_filtered <- NULL
rs_years_max_loc <- NULL

for(i in 1:nrow(meta_new)){
  print(paste('region', i, 'of', nrow(meta_new)))
  # perform loop for each study
  region <- rs_4loc_10yr %>% 
    filter(regional_level == meta_new$regional_level[i])
  
  # enter Dr Wu-Bing Ma...lesson learned: sometimes 'wide' beats 'long' data for wrangling!
  # calculate number of locations / plots in each year: rows are years, columns are cells, elements are number of locations
  year_loc <- as.matrix(xtabs( ~ year + local, data = region[,c("year","local")], sparse=TRUE)) 
  
  # calculations in preparation to keep locations /plots with samples in all years 
  loc_dat <- data.frame("loc_plot" = colnames(year_loc),
                        "p_years" = colMeans(year_loc>0),
                        "mean_loc" = colMeans(year_loc)) %>% 
    mutate(p_loc = round(mean_loc/sum(mean_loc),3)) #relative density of locations
  
  # logical test: which locations have samples in all years or > 1/2 
  id_location <- with(loc_dat, p_years==1 |  p_loc > 0.5*1/nrow(loc_dat))
  year_location <-  year_loc[, id_location, drop=FALSE]
  
  # number of samples that co-occur in the locations between years
  co_loc <- as.matrix(vegan::designdist(year_loc, method = "J", terms= "minimum"))
  
  # find the max duration for which two years (year-pair) have the at least 75% of the maximum number of locations
  max_co_loc <- reshape2::melt(co_loc) %>% 
    as_tibble() %>%
    set_names("year1","year2","n_loc") %>%
    mutate(year1 = as.numeric(year1),
           year2 = as.numeric(year2),
           duration = year2 - year1 + 1) %>%
    filter(duration > 9 & n_loc > 3)
  
  if(nrow(max_co_loc) == 0) {next}
  
  # set  proportion of sites to get: studies with many sites get higher threshold
  prop = meta_new %>% 
    filter(regional_level == meta_new$regional_level[i]) %>% 
    mutate(prop = case_when(mean_loc > 20 ~ 0.5, 
                            mean_loc < 20 ~ 0.9),
           prop = case_when(# lower threshold for NERC countryside survey plant data 
                            # average number of sites in different regions lies between ~ 60 - 300)
                            # lower threshold increase duration by > 10 years
                            regional_level=='countryside_survey_plants_2017_England' ~ 0.25,
                            regional_level=='countryside_survey_plants_2017_Scotland' ~ 0.25,
                            regional_level=='countryside_survey_plants_2017_Wales' ~ 0.25,
                            TRUE ~ as.numeric(prop))) %>% 
    pull(prop)
  
  # first, find pair of years with at prop% of the maximum # locations 
  max_co_loc <-  filter(max_co_loc, n_loc >= prop*max(n_loc))
  # break ties with the maximum duration
  max_co_loc <- filter(max_co_loc, duration == max(duration))
  max_co_loc <-  filter(max_co_loc, n_loc == max(n_loc))
  
  # if necessary, break tie (multiple start and end points with the same # sites / plots)
  if(nrow(max_co_loc) > 1){
    # get the latest time period
    n = nrow(max_co_loc)
    max_co_loc <- max_co_loc %>% 
      slice(n)
  }
  
  # keep the locations in both the selected years
  loc_year1 <- year_loc[rownames(year_loc) %in% unlist(max_co_loc[1,1]),]
  loc_year2 <- year_loc[rownames(year_loc) %in% unlist(max_co_loc[1,2]),]
  cell_shared <- loc_year1 > 0 & loc_year2 > 0
  year_loc <- year_loc[,cell_shared, drop=FALSE]
  
  # keep the desired locations & years
  rare_region <- region %>% 
    filter(local %in% colnames(year_loc) & year %in% rownames(year_loc))
  
  rs_years_max_loc <- bind_rows(rs_years_max_loc , bind_cols(regional_level = meta_new$regional_level[i], max_co_loc))
  rs_4loc_10yr_filtered  <- bind_rows(rs_4loc_10yr_filtered , rare_region)
}

# double check::
# remove years with < 4 locations and studies with number of years < 2 and duration < 10 years
rs_4loc_10yr_filtered <- rs_4loc_10yr_filtered %>% 
  group_by(regional_level, year) %>%
  mutate(n_loc = n_distinct(local)) %>% 
  filter(n_loc >= 4) %>%
  group_by(regional_level) %>%
  mutate(n_years = n_distinct(year, na.rm = TRUE),
         duration = max(year) - min(year) + 1) %>%
  filter(n_years >= 2 & duration >= 10) %>%
  ungroup() %>%
  dplyr::select(-c(n_loc, n_years, duration))

# get the raw data for these years and locations
rs_filtered <- rs_4loc_10yr_filtered %>% 
  inner_join(dat_rs, 
             by = c("dataset_id", "regional_level",  "regional", "year", "local"))



# check before saving
rs_years_max_loc <- rs_years_max_loc %>% 
  unite(region_yr1, c(regional_level, year1), remove = FALSE) %>% 
  unite(region_yr2, c(regional_level, year2), remove = FALSE) 

# filter to reduce to 2 years that max sites
rs_filtered_2timeOnly <- rs_filtered %>% 
  unite(region_yr, c(regional_level, year), remove = FALSE) %>% 
  filter(region_yr %in% rs_years_max_loc$region_yr1 | region_yr %in% rs_years_max_loc$region_yr2) %>% 
  group_by(regional_level, local) %>% 
  mutate(fyear = case_when(year==min(year) ~ 'start',
                           year==max(year) ~ 'end',
                           (year!=min(year) | year!=max(year)) ~ 'intermediate')) %>% 
  ungroup() 

# check we've got equal numbers of loc_plots at the start and end
left_join(rs_filtered_2timeOnly %>% 
            filter(fyear=='start') %>% 
            group_by(regional_level) %>% 
            summarise(n_loc_plots_start = n_distinct(local)),
          rs_filtered_2timeOnly %>%
            filter(fyear=='end') %>% 
            group_by(regional_level) %>% 
            summarise(n_loc_plots_end = n_distinct(local))) %>% 
  filter(n_loc_plots_start!=n_loc_plots_end)


# visual inspection
r <- rs_filtered_2timeOnly %>% distinct(regional_level) %>% pull()

pdf('~/Dropbox/1current/spatial_composition_change/figures/data-visualisation/homog-db-sampling-check-140922.pdf', width = 12, height = 9)

for(i in 1:length(unique(rs_filtered_2timeOnly$regional_level))){
  print(paste('STUDY', i, 'of', length(unique(rs_filtered_2timeOnly$regional_level)), 'regions'))
  
  p = ggplot() +
    geom_point(data = rs_filtered_2timeOnly %>% 
                 # filter(fyear!='intermediate') %>% 
                 distinct(regional_level, year, fyear) %>% 
                 right_join(rs_filtered) %>%  
                 filter(regional_level == r[i]) %>% 
                 distinct(local, year, fyear),
               aes(x = year, y = local, colour = fyear)) +
    labs(subtitle = r[i])
  
  
  print(p)
}
dev.off()

# now wrangle the checklist data
cl_dat <- dat %>% 
  unite(regional_level, c(dataset_id, regional), remove = F) %>% 
  left_join(meta %>% distinct(dataset_id, checklist)) %>% 
  filter(checklist==TRUE)

cl_dat <- cl_dat %>% 
  group_by(regional_level) %>% 
  mutate(fyear = case_when(year == min(year) ~ 'first',
                           year == max(year) ~ 'last', 
                           (year!=min(year) & year!=max(year)) ~ 'intermediate')) %>%
  ungroup()
  
# count how many start and end years there are in each region (we want to 
# ensure there is only one of each)
cl_dat %>% 
  group_by(regional_level) %>% 
  mutate(fyear = case_when(year == min(year) ~ 'first',
                           year == max(year) ~ 'last', 
                           (year!=min(year) & year!=max(year)) ~ 'intermediate')) %>% 
  group_by(regional_level, fyear) %>% 
  summarise(n_yrs = n_distinct(year)) %>% 
  filter(n_yrs != 1)

# # remove sites with different start and end points to others in their respective regions
cl_dat <- cl_dat %>%
  unite(reg_site, c(regional_level, local), remove = FALSE) %>%
  filter(reg_site!='fitzgerald_1997_Canagagigue Creek_Site  E') %>%
  filter(reg_site!='fitzgerald_1997_Laurel Creek_Site  C') %>%
  select(-reg_site)

# reducet to regions with at least four sites 
cl_dat <- cl_dat %>% 
  group_by(regional_level) %>% 
  mutate(n_site = n_distinct(local)) %>% 
  filter(n_site >= 4) %>% 
  ungroup()

# check if there are locations sampled in 2nd time point not in 1st, or vice versa
row2remove1 <-
anti_join(cl_dat %>% 
            filter(fyear=='first') %>% 
            distinct(regional_level, local),
          cl_dat %>% 
            filter(fyear=='last') %>% 
            distinct(regional_level, local)) %>% 
  unite(reg_loc, c(regional_level, local))

row2remove2 <-
  anti_join(cl_dat %>% 
              filter(fyear=='last') %>% 
              distinct(regional_level, local),
            cl_dat %>% 
              filter(fyear=='first') %>% 
              distinct(regional_level, local)) %>% 
  unite(reg_loc, c(regional_level, local))


cl_dat <-
cl_dat %>%
  unite(reg_loc, c(regional_level, local), remove = FALSE) %>%
  filter(!reg_loc %in% row2remove1$reg_loc) %>% 
  filter(!reg_loc %in% row2remove2$reg_loc) %>% 
  select(-reg_loc) 

homog_dat <- bind_rows(cl_dat, 
                       rs_filtered)

# check sorte_2018a and sorte_2018b
save(homog_dat,
     rs_years_max_loc,
     file = '~/Dropbox/1current/spatial_composition_change/data/homog-site-year-selected.Rdata')
