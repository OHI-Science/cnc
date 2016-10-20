# load packages
library(dplyr)
library(tidyr) # install.packages('tidyr')
library(readr) # install.packages('readr')
library(stringr) # install.packages('stringr')

# set directory
dir_hab <- '~/github/cnc/prep/HAB'
setwd(dir_hab)

dir_layers <- '~/github/cnc/eez2016/layers'

# read in raw data file
hab <- read_csv(file.path(dir_hab, 'hab_data.csv')) %>%
  select(kingdom = Kingdom, species = Gen_Species, iucn_status = IUCN_Status, year = `Year Assessed`) %>%
  mutate(iucn_status = ifelse(is.na(iucn_status), 'NA', iucn_status),
         year = ifelse(is.na(year), 'NA', year))

# lookup for weights iucn_status
w.risk_category <-  c('LC' = 0,
                      'NT' = 0.2,
                      'VU' = 0.4,
                      'NA' = 0.5, #Not Assessed (NA)
                      'EN' = 0.6,
                      'CR' = 0.8,
                      'EX' = 1)

#remove records with IUCN weight DD
final_hab_data <- filter(hab, !iucn_status == 'DD') %>%
  #Merge w.risk_category with iucn_status
  mutate(risk.wt = w.risk_category[iucn_status]) %>%
  group_by(species) %>%
  ungroup %>%
  mutate(rgn_id = 1) %>%
  select(rgn_id, kingdom, species, iucn_status, year, risk.wt)

#calculate status for coral habitats
coral_status <- filter(final_hab_data, kingdom == 'ANIMALIA') %>%
  summarize(status = (1 - mean(risk.wt)) * 100)

#calculate status for plant habitats
plant_status <- filter(final_hab_data, kingdom == 'PLANTAE') %>%
  summarize(status = (1 - mean(risk.wt)) * 100)

hab_status <- rbind(coral_status, plant_status) %>%
  summarize(score = mean(status))

write_csv(final_hab_data, file.path(dir_hab, "final_hab_data.csv"))
write_csv(final_hab_data, file.path(dir_layers, "hab_iucn_status_cnc2016_EJP.csv"))
