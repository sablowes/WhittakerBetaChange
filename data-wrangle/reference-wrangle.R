# code by Dr Alban Sagouis to build reference list for compiled databases

library(data.table)


# Building the .bib reference file for data compiled alban 
dois <- unique(data.table::fread(file = '~/Dropbox/1current/spatial_composition_change/ms/refs/idiv-dat.csv',
                                 select = c('dataset_id','doi'), na.strings = ''))
dois[, c('doi','doi2', 'doi3') := data.table::tstrsplit(dois$doi, ' *\\| *')]
dois <- data.table::melt(data = dois, id.vars = 'dataset_id', value.name = 'doi', na.rm = TRUE)[, variable := NULL]

# build bibliography
bib <- rcrossref::cr_cn(dois = dois$doi, locale = "en-US")
# saveRDS(object = bib, file = '~/Desktop/raw_checklistreferences.rds')
bib <- unlist(stringi::stri_split_lines(bib))
bib <- base::enc2utf8(bib)
base::writeLines(text = bib, "~/Dropbox/1current/spatial_composition_change/ms/refs/checklist-and-metacommunity-references.bib")

# Preparing a reference list for all data sets in metacommunity resurveys----
dois <- unique(data.table::fread(file = '~/Dropbox/BioTimeX/Local-Regional Homogenization/_data_extraction/checklist_change_metadata.csv',
                                 select = c('dataset_id','doi'), na.strings = ''))
dois[, c('doi','doi2') := data.table::tstrsplit(dois$doi, ' *\\| *')]
dois <- data.table::melt(data = dois, id.vars = 'dataset_id', value.name = 'doi', na.rm = TRUE)[, variable := NULL]

# Building the .bib reference file
bib <- rcrossref::cr_cn(dois = dois$doi, locale = "en-US")
# saveRDS(object = bib, file = '~/Desktop/raw_checklistreferences.rds')
bib <- unlist(stringi::stri_split_lines(bib))
bib <- base::enc2utf8(bib)
base::writeLines(text = bib, "~/Desktop/checklist-references.bib")
