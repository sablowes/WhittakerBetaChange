# script to reduce checklists to balanced site-year combinations
path2dir <- '~/Dropbox/1current/spatial_composition_change/WhittakerBetaChange/data-wrangle/01-resurvey-checklist-wrangle/balanced-site-years-checklist/'

# manual processing of metacommunity resurvey sampling sites and years
load('~/Dropbox/1current/spatial_composition_change/data/homog-site-year-selected-two-time-points.Rdata')

# separate checklists from resurvey data
resurvey_dat <- homog_dat %>% 
  filter(checklist==FALSE)

checklist_dat <- homog_dat %>% 
  filter(checklist==TRUE)

rm(homog_dat)

##	Get the raw data (needed because some alternate year-site combinations
## have been identified in the manual check)
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

# manual inspection results
man_inspection <- read_csv(paste0(path2dir, '/../checklist-visual-inspection-new.csv'),
                           show_col_types = FALSE)

# studies to clean
to_clean <- man_inspection %>% 
  filter(check=='see file') %>% 
  distinct(regional_level)

# studies ready to go
ready <- man_inspection %>% 
  filter(check=='ok') %>% 
  distinct(regional_level)

# create filelist to loop through
setwd(paste0(path2dir))
filelist <- dir(pattern="*.csv")

study_site_year <- tibble()

for(file in 1:length(filelist)){
  id = as.character(str_split(filelist[file], pattern = '.csv')[[1]][1])
  temp <- read_csv(filelist[file], show_col_types = FALSE) %>% 
    mutate(regional_level = id,
           local = as.character(local))
  
  study_site_year <- bind_rows(study_site_year,
                               temp)
}

study_site_year <- study_site_year %>% 
  select(-c(nsites)) %>% 
  unite(styrsite, c(regional_level, year, local), remove = FALSE)

# reduce studies to site-year combinations manually selected
study_site_yr_selected <- cl_dat %>% 
  unite(styrsite, c(regional_level, year, local), remove = FALSE) %>% 
  filter(styrsite %in% study_site_year$styrsite) %>% 
  select(-styrsite)

rm(cl_dat)

# retain studies that are ok
ok_checklists <- checklist_dat %>% 
  filter(regional_level %in% ready$regional_level)

# put reduced study-site-year combinations back in,
# and reduce to sites in max_rect
ok_checklists <- bind_rows(ok_checklists,
                   study_site_yr_selected %>% 
                     mutate(checklist = TRUE))

# check we have the right number of studies
target <- bind_rows(to_clean, ready) %>% distinct(regional_level) %>% arrange(-desc(regional_level))
check <- checklist_dat %>% distinct(regional_level) %>% arrange(-desc(regional_level))
filter(target, !(regional_level %in% check$regional_level))


# visual inspection
r <- ok_checklists %>% distinct(regional_level) %>% pull()

pdf('~/Dropbox/1current/spatial_composition_change/figures/data-visualisation/checklist-two-time-points-only-new-clean.pdf', width = 12, height = 9)

for(i in 1:length(r)){
  print(paste('STUDY', i, 'of', length(r), 'regions'))
  
  p = ggplot() +
    # facet_wrap(~YEAR) +
    geom_point(data = ok_checklists %>% 
                 filter(regional_level == r[i]) %>% 
                 distinct(local, year, .keep_all = TRUE),
               aes(x = year, y = local)) +
    labs(subtitle = paste0('regional_level = ', r[i])) +
    theme(legend.position = 'none')
  
  
  print(p)
}
dev.off()

# check duration
ok_checklists <- ok_checklists %>% 
  group_by(regional_level) %>% 
  mutate(duration = max(year) - min(year) + 1) %>% 
  ungroup() %>% 
  filter(duration > 9)

# put the checklists back with the resurvey data
homog_dat <- bind_rows(resurvey_dat,
                       ok_checklists)


# save data (ready for analysis)
save(homog_dat,
     rs_years_max_loc,
     file = '~/Dropbox/1current/spatial_composition_change/data/homog-site-year-selected-two-time-points-clean.Rdata')
