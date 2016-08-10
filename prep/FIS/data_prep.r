# load packages
library(dplyr)
library(tidyr) # install.packages('tidyr')
library(readr) # install.packages('readr')

# set directories
dir_fis <- '~/github/cnc/prep/FIS'
setwd(dir_fis)

# read in raw data files

catch_per_species <- read_csv(file.path(dir_fis, 'Fisheries_CSV_Layers', 'Catch_Tonnes_per_Species.csv'))

reef_catch_per_species <- read.csv(file.path(dir_fis, 'Fisheries_CSV_Layers', 'Reef_Catch_Tonne_per_Species.csv'), na.rm = TRUE)
reef_catch_per_species <- reef_catch_per_species[, colSums(is.na(reef_catch_per_species)) < nrow(reef_catch_per_species)] # remove NA columns

## combine two data frames

fis_meancatch <- rbind(catch_per_species, reef_catch_per_species)
## other is from open_catch_per_species

# save file as csv
write_csv(fis_meancatch, file.path(dir_fis, 'fis_meancatch.csv'))
