# Need to standardise spatial extent to use RivFishTime for analysis of homogenisation
# Two gamma-scale are of potential interest: 1. SourceID, 2. BioRealm (see Su et al. 2021 Science for recent example of the latter)

# To standardise extent for SourceID, this script:
# 1. counts number of unique locations within SourceID's (want at least 4)
# 2. finds the Protocol (i.e., sampling methodology) with the most samples
# 3. Identifies the Quarter (of the year) with the most years sampled
# 4. identifies the two years within each region (with at least 10 years duration expired between samples) for which we get the greatest number of sites

library(tidyverse)

ft <- read_csv('~/Dropbox/1current/data/RivFishTime/1873_10_1873_2_RivFishTIME_SurveyTable.csv')
meta <- read_csv('~/Dropbox/1current/data/RivFishTime/1873_10_1873_2_RivFishTIME_TimeseriesTable.csv') %>% 
  select(-X13)

# studies with at least 4 time series
study_4loc <- meta %>%
  group_by(SourceID) %>%
  summarise(n_loc = n_distinct(TimeSeriesID),
            n_protocol = n_distinct(Protocol)) %>% 
  ungroup() %>% 
  filter(n_loc > 3) 

# 10 studies with multiple protocols
study_mp <- study_4loc  %>% 
  filter(n_protocol > 1)

# Choose the protocol with the maximum number of time series
study_mp_selected <- meta %>% 
  filter(SourceID %in% pull(study_mp, SourceID)) %>%
  group_by(SourceID, Protocol) %>%
  summarise(n_loc = n_distinct(TimeSeriesID)) %>%
  filter(n_loc > 3) %>%
  group_by(SourceID) %>%
  filter(n_loc == max(n_loc)) %>%
  ungroup() %>% 
  distinct(SourceID, .keep_all = TRUE) #one study have 2 protocol with same number of time series. Keep one of them.

# the time series in the studies with at least 4 time series in the same protocol 
meta_4loc <- bind_rows(meta %>% filter(SourceID %in% 
                                         (study_4loc  %>% filter(n_protocol == 1) %>% pull(SourceID))),
                       meta %>% inner_join(study_mp_selected %>% dplyr::select(-n_loc)))

# check number of sites and time series for each study, and whether they are equal
meta_4loc %>% 
  group_by(SourceID) %>% 
  summarise(n_site = n_distinct(SiteID),
            n_ts = n_distinct(TimeSeriesID)) %>% 
  filter(n_site != n_ts) # each time series has unique site

# communities for the selected time series
ft_4loc <- inner_join(ft, meta_4loc, by = "TimeSeriesID") %>% 
  relocate(SourceID) 

# Check the number of years with surveys in each Quarter for each time series
table(ft_4loc$Quarter, useNA = "always")
ft_4loc_nyear_quarter <- ft_4loc %>% 
  mutate(Quarter = ifelse(Quarter == "3-Feb", 1, Quarter)) %>%
  group_by(TimeSeriesID) %>% 
  summarise(n_years = n_distinct(Year),
            n_years_Q1 = n_distinct(Year[Quarter == 1]),
            n_years_Q2 = n_distinct(Year[Quarter == 2]),
            n_years_Q3 = n_distinct(Year[Quarter == 3]),
            n_years_Q4 = n_distinct(Year[Quarter == 4]))
ft_4loc_nyear_quarter$max_years_quater <- apply(ft_4loc_nyear_quarter[,3:6], 1, max)
apply(ft_4loc_nyear_quarter[,-1], 2, mean)

# the quarter with the most years sampled 
ft_4loc_nyear_quarter$best_quarter <- apply(ft_4loc_nyear_quarter[,3:6], 1, function(x) { which(x == max(x))[1]})
table(ft_4loc_nyear_quarter$best_quarter)

# keep the years in the quarter with most surveys to control similar survey time through years for each time series,    
ft_4loc <- ft_4loc %>% 
  mutate(Quarter = ifelse(Quarter == "3-Feb", 1, Quarter)) %>% 
  inner_join(ft_4loc_nyear_quarter %>%
               select(TimeSeriesID, Quarter = best_quarter) %>%
               mutate(Quarter = as.character(Quarter)))

# filter the dataset to maximum the number of shared  time series for at least two years within a region
# ft_4loc <- ft_4loc %>% dplyr::select(-keep)
ft_4loc_filtered <- NULL
for(i in 1:length(unique(ft_4loc$SourceID))){
  # perform loop for each study
  study <- ft_4loc %>% 
    filter(SourceID == unique(ft_4loc$SourceID)[i])
  
  # get a matrix with rows are years and columns are sites (TimeSeriesID),and elements are presence/absence of sites in each year
  year_site <- as.matrix(xtabs(~ Year + TimeSeriesID, data = unique(study[,c("Year","TimeSeriesID")]), sparse=TRUE))
  
  # number of co-occurred sites between years
  co_site <- as.matrix(designdist(year_site, method = "J", terms= "binary"))
  
  # find which two years (year-pair) have the maximum number of co-occurred sites and duration >=10 years
  # these two years have priority to be kept, and other years will be compared to the two years
  max_co_site <- melt(co_site) %>% 
    as_tibble() %>%
    set_names("year1","year2","n_site") %>%
    mutate(year1 = as.numeric(year1),
           year2 = as.numeric(year2),
           duration = year2 - year1 + 1) %>%
    filter(duration > 9 & n_site > 3)
  if(nrow(max_co_site) == 0) {next}
  max_co_site <-  filter(max_co_site, n_site >= 0.9*max(n_site))
  max_co_site <- filter(max_co_site, duration == max(duration))
  max_co_site <-  filter(max_co_site, n_site == max(n_site))
  
  # keep sites that are shared in the two determined years
  sites_year1 <- year_site[rownames(year_site) %in% unlist(max_co_site[1,1]),]
  sites_year2 <- year_site[rownames(year_site) %in% unlist(max_co_site[1,2]),]
  sites_shared <- sites_year1 > 0 & sites_year2 > 0
  year_site <- year_site[,sites_shared, drop=FALSE]
  
  # Other years except the two priority years will be compared to the two years. Keep only the years have the same sites with the priority years
  id <- rowSums(year_site) == ncol(year_site)
  year_site <- year_site[id, ]
  
  #  only keep the selected TimeSeriesIDs and years 
  study_filtered <- study %>% 
    filter(TimeSeriesID %in% colnames(year_site) & Year %in% rownames(year_site))
  
  ft_4loc_filtered <- bind_rows(ft_4loc_filtered, study_filtered)
}

ggplot(data = study_filtered, aes(Longitude, Latitude)) + 
  facet_wrap(~Year) +
  geom_point(size = 1, alpha = 0.7) + 
  theme(legend.position = "top", legend.text = element_text(size=12)) + 
  coord_fixed()


# keep only years with at least 4 sites,
# and keep studies with at least 2 time points and duration >10 years
ft_4loc_filtered <- ft_4loc_filtered %>%
  group_by(SourceID, Year) %>%
  mutate(n_site = n_distinct(TimeSeriesID)) %>%
  filter(n_site > 3) %>%
  group_by(SourceID) %>%
  mutate(all_site = n_distinct(TimeSeriesID ),
         min_site = min(n_site),
         n_years = n_distinct(Year),
         duration = max(Year) - min(Year) +1) %>%
  ungroup() %>%
  filter(n_years >= 2 & duration >= 10)


# check how many studies and their attributes
ft_studies <- ft_4loc_filtered %>% 
  distinct(SourceID, all_site, min_site, n_years, duration) #36 studies
table(ft_studies$all_site == ft_studies$min_site) #all is TRUE, meaning all years within the same study have same sites   
table(ft_studies$all_site)
table(ft_studies$min_site) # 18 studies >= 10
table(ft_studies$n_years)  # 8 studies >= 10
table(ft_studies$duration)


# add the column "keep" to distinguish the records that should be kept or removed
ft_4loc <- ft_4loc %>% 
  left_join(ft_4loc_filtered %>% 
              dplyr::select(- (n_site:duration)) %>%
              mutate(keep = "yes")) %>%
  mutate(keep = ifelse(is.na(keep), "no", keep))


# plot distributions of sites and indicate which records will be removed
pdf('RivFishTIME_4locations_filtered_sameQuarter.pdf', width = 12, height = 10)
id_study <- unique(ft_4loc$SourceID)
for(i in 1:length(id_study)){
  study <- ft_4loc %>% 
    filter(SourceID %in% id_study[i]) %>% 
    distinct(SourceID, TimeSeriesID, Year, Latitude, Longitude, keep)
  
  p <- ggplot(data = study, aes(Longitude, Latitude)) + 
    facet_wrap(~Year) +
    geom_point( aes(colour = keep), size = 1, alpha = 0.7) + 
    labs(title = id_study[i]) +
    theme(legend.position = "top", legend.text = element_text(size=12)) + 
    coord_fixed() +
    scale_color_manual(values=c("yes" = "deepskyblue", "no" = "coral"))
  
  print(p)
}
dev.off()


# check how many time series have different number of surveys across years
n_survey <- ft_4loc_filtered %>% 
  dplyr::select(-(n_site:duration)) %>%
  group_by(TimeSeriesID, Year) %>%
  summarise(n_survey = n_distinct(SurveyID)) %>%
  group_by(TimeSeriesID) %>% 
  summarise(min_survey = min(n_survey),
            max_survey = max(n_survey))
# ~10% of time series have different number of surveys between years
table(n_survey$min_survey == n_survey$max_survey)
# only 3 time series have multiple surveys at all year
table(n_survey$min_survey > 1) 

# keep one survey for each year of each time series
one_survey_perYear <- ft_4loc_filtered %>% 
  dplyr::select(SourceID, TimeSeriesID, Year, SurveyID) %>% 
  distinct(SourceID, TimeSeriesID, Year, .keep_all=TRUE)

ft_4loc_filtered <-  ft_4loc_filtered %>% 
  inner_join(one_survey_perYear)

# save the filtered data
ft_filtered <- ft_4loc_filtered %>% 
  dplyr::select(-(n_site:duration)) 

save(ft_filtered, 
     file = '~/Dropbox/1current/spatial_composition_change/data/rft_filtered_4s_10y_sameQ.Rdata')


