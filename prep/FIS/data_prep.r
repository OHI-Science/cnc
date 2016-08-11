# load packages
library(dplyr)
library(tidyr) # install.packages('tidyr')
library(readr) # install.packages('readr')

# set directories
dir_fis <- '~/github/cnc/prep/FIS'
setwd(dir_fis)

# read in raw data files

catch_per_species <- read_csv(file.path(dir_fis, 'Fisheries_CSV_Layers', 'Catch_Tonnes_per_Species.csv'))

reef_catch_per_species <- read_csv(file.path(dir_fis, 'Fisheries_CSV_Layers', 'Reef_Catch_Tonne_per_Species.csv'))
reef_catch_per_species <- reef_catch_per_species[, colSums(is.na(reef_catch_per_species)) < nrow(reef_catch_per_species)] # remove NA columns

## combine two data frames

fis_meancatch_french <- rbind(catch_per_species, reef_catch_per_species)
## other is from open_catch_per_species

# replace french names with scientific names

species_name_match <- read_csv(file.path(dir_fis,'Fisheries_CSV_Layers','catch_species_names.csv'))
fis_meancatch <- full_join(fis_meancatch_french, species_name_match,
                           by = 'species') %>%
  select(-species) %>%

rename(species = scientific_name)

# add stock resilience to each fishery from fishbase.org, following Martell & Froese 2011
resilience <- data.frame( species = c("Thunnus alalunga", "Thunnus albacares", "Thunnus obesus",
                                      "Xiphias gladius", "Kajikia audax", "Makaira mazara",
                                      "Istiompax indica", "Acanthocybium solandri", "Lampris guttatus",
                                      "Coryphaena hippurus", "Istiophorus platypterus", "Tetrapturus angustirostris",
                                      "other", "Holothuroidea (class)", "Panulirus (genus)",
                                      "Pagrus auratus"
                                        ),
                          resilience = c('medium', 'medium', 'low',
                                         'low', 'low', 'low',
                                         'medium', 'medium', 'low',
                                         'high', 'low', 'medium',
                                         'low', 'low', 'low',
                                         'low') )

## gave low resilience to 'other', 'holothuroidea' (sea cucumber), and 'panulirus' (lobster) assuming the precautionary principle

fis_meancatch_resilience <- full_join(fis_meancatch, resilience,
                           by = 'species') %>%

select(rgn_id, species, year, tonnes, resilience)

# save file as csv
write_csv(fis_meancatch_resilience, file.path(dir_fis, 'fis_meancatch_resilience.csv'))

