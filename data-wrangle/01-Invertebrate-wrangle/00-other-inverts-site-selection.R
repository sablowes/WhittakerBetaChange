##======================================================================
## identify studies with the same location sampled twice, at least 10 years apart
## want studies (i.e., regions) with at least four locations
##======================================================================

rm(list=ls())
##	
library(tidyverse)
library(vegan)
library(reshape2)


##	Get the raw data (thesee are the data published in Van Klink et al 2020 Ecology)
inverts <- readRDS('~/Dropbox/1current/spatial_composition_change/data/Raw data insect metacommunities.Rdata')
inverts <- inverts %>% 
  as_tibble() %>% 
  # remove the zeroes 
  filter(Number > 0) 

invert_meta <- read_csv('~/Dropbox/1current/spatial_composition_change/data/Insect Metacommunities Metadata.csv')

invert_loc <- inverts %>% 
  dplyr::select(Datasource_ID, Year, Plot_ID) %>% 
  distinct()

# remove years with number of locations / plots < 4.
invert_loc <-  invert_loc %>% 
  group_by(Datasource_ID, Year) %>%
  mutate(n_loc = n_distinct(Plot_ID)) %>%
  ungroup() %>%
  filter(n_loc >= 4) %>%
  dplyr::select(-n_loc)

# calculate number of locations, number of years and duration，
# and remove studies with number of years < 2 and duration <10 years
meta_year <- invert_loc %>%
  group_by(Datasource_ID, Year) %>%
  summarise(n_loc = n_distinct(Plot_ID)) %>%
  group_by(Datasource_ID) %>%
  mutate(total_loc = sum(n_loc), 
         mean_loc = round(mean(n_loc),1), 
         min_loc = min(n_loc), 
         max_loc = max(n_loc),
         n_years = n_distinct(Year, na.rm = TRUE),
         duration = max(Year) - min(Year) +1) %>%
  ungroup() %>% 
  filter(n_years >=2 & duration >=10)

# meta data of search criteria
meta <- meta_year %>% 
  dplyr::select(-c(Year, n_loc)) %>% 
  distinct()

# remove studies with number of years < 2 and duration <10 years
invert_4loc_10yr <- invert_loc %>% 
  unite(col = study_year, Datasource_ID, Year, remove = FALSE) %>%
  filter(study_year %in% (meta_year %>% unite(col = study_year, Datasource_ID, Year) %>% pull(study_year))) %>%
  dplyr::select(-study_year)

# loop de loop to optimise each study
# find pair of years ≥ 10 years apart, with maximum number of locations (plots)
invert_4loc_10yr_filtered <- NULL
invert_years_max_loc <- NULL

for(i in 1:nrow(meta)){
  print(paste('study', i, 'of', nrow(meta)))
  # perform loop for each study
  study <- invert_4loc_10yr %>% 
    filter(Datasource_ID == meta$Datasource_ID[i])  %>% 
    unite(loc_plot, c(Plot_ID), remove = F)
  
  # enter da Dr Wubing Ma...lesson learned: sometimes wide data are useful!
  # calculate number of locations / plots in each year: rows are years, columns are cells, elements are number of locations
  year_loc <- as.matrix(xtabs( ~ Year + loc_plot, data = study[,c("Year","loc_plot")], sparse=TRUE)) 
  
  # calculations in preparation to keep locations /plots with samples in all years 
  # or locations with density of samples grater than 50% of the mean value
  loc_dat <- data.frame("loc_plot" = colnames(year_loc),
                        "p_years" = colMeans(year_loc>0),
                        "mean_loc" = colMeans(year_loc)) %>% 
    mutate(p_loc = round(mean_loc/sum(mean_loc),3)) #relative density of locations
  
  # logical test
  id_location <- with(loc_dat, p_years==1 |  p_loc > 0.5*1/nrow(loc_dat))
  year_location <-  year_loc[, id_location, drop=FALSE]
  
  # number of samples that co-occur in the locations between years
  co_loc <- as.matrix(designdist(year_loc, method = "J", terms= "minimum"))
  
  # find which two years (year-pair) have the maximum number of co-occurred locations and duration >=10 years
  # these two years have priority to be kept, and other years will be compared to the two years
  max_co_loc <- reshape2::melt(co_loc) %>% 
    as_tibble() %>%
    set_names("year1","year2","n_loc") %>%
    mutate(year1 = as.numeric(year1),
           year2 = as.numeric(year2),
           duration = year2 - year1 + 1) %>%
    filter(duration > 9 & n_loc > 3)
  if(nrow(max_co_loc) == 0) {next}
  max_co_loc <-  filter(max_co_loc, n_loc >= 0.9*max(n_loc))
  max_co_loc <- filter(max_co_loc, duration == max(duration))
  max_co_loc <-  filter(max_co_loc, n_loc == max(n_loc))
  
  # if necessary, break tie (multiple start and end points with the same # sites / plots)
  if(nrow(max_co_loc) > 1){
    # get the latest time period
    n = nrow(max_co_loc)
    max_co_loc <- max_co_loc %>% 
      slice(n)
  }
  
  # keep the locations in all years
  loc_year1 <- year_loc[rownames(year_loc) %in% unlist(max_co_loc[1,1]),]
  loc_year2 <- year_loc[rownames(year_loc) %in% unlist(max_co_loc[1,2]),]
  cell_shared <- loc_year1 > 0 & loc_year2 > 0
  year_loc <- year_loc[,cell_shared, drop=FALSE]
  
  # keep the desired locations & years
  rare_study <- study %>% 
    filter(loc_plot %in% colnames(year_loc) & Year %in% rownames(year_loc))
  
  invert_years_max_loc <- bind_rows(invert_years_max_loc , bind_cols(Datasource_ID = meta$Datasource_ID[i], max_co_loc))
  invert_4loc_10yr_filtered  <- bind_rows(invert_4loc_10yr_filtered , rare_study)
}

# double check::
# remove years with < 4 locations and studies with number of years < 2 and duration < 10 years
invert_4loc_10yr_filtered <- invert_4loc_10yr_filtered %>% 
  group_by(Datasource_ID, Year) %>%
  mutate(n_loc = n_distinct(Plot_ID)) %>% 
  filter(n_loc >= 4) %>%
  group_by(Datasource_ID) %>%
  mutate(n_years = n_distinct(Year, na.rm = TRUE),
         duration = max(Year) - min(Year) + 1) %>%
  filter(n_years >= 2 & duration >= 10) %>%
  ungroup() %>%
  dplyr::select(-c(n_loc, n_years, duration))

# get the raw data for these years and locations
invert_filtered <- invert_4loc_10yr_filtered %>% 
  inner_join(inverts, 
             by = c("Datasource_ID",  "Year", "Plot_ID"))

save(invert_filtered,
     invert_years_max_loc,
     file = '~/Dropbox/1current/spatial_composition_change/data/invert-location-plots.Rdata')

