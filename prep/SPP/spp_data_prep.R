# load packages
library(dplyr)
library(tidyr) # install.packages('tidyr')
library(readr) # install.packages('readr')

# set directory
dir_spp <- '~/github/cnc/prep/SPP'
setwd(dir_spp)

# read in raw data file
iucn_doc <- read_csv(file.path(dir_spp, 'nc_iucn_spp.csv')) %>%
  select(species = `Species`, iucn_status = `Red List status`, year = `Year assessed`)

#adding missing species from iconic species list
add_ico_spp <- read_csv(file.path(dir_spp, 'additional_ico_spp.csv')) %>%
  select(species = `Species`, iucn_status = `Red List status`, year = `Year assessed`)

# merge raw data frames
## replace LR/cd = NT, LR/nt = NT, LR/lc = LC based on http://www.iucnredlist.org/static/categories_criteria_2_3
full_spp_data <- rbind(iucn_doc, add_ico_spp) %>%
  mutate(iucn_status = stringr::str_replace_all(iucn_status, "LR/cd", "NT"),
                        iucn_status = stringr::str_replace_all(iucn_status, "LR/nt", "NT"),
                        iucn_status = stringr::str_replace_all(iucn_status, "LR/lc", "LC"))

write_csv(full_spp_data, file.path(dir_spp, "full_spp_data.csv"))

# lookup for weights iucn_status
w.risk_category <-  c('LC' = 0,
                      'NT' = 0.2,
                      'VU' = 0.4,
                      'EN' = 0.6,
                      'CR' = 0.8,
                      'EX' = 1)

#remove records with IUCN weight DD
final_spp_data <- filter(full_spp_data, !iucn_status == 'DD') %>%
#Merge w.risk_category with iucn_status
  mutate(risk.wt = w.risk_category[iucn_status]) %>%
  filter(year %in% 2006:2016) %>% ##only selected the past 10 years for biodiversity status -- this could be changed
  group_by(species) %>%
  filter(year == max(year)) %>%
  ungroup %>%
  mutate(rgn_id = 1) %>%
  select(rgn_id, species, iucn_status, year, risk.wt)

write_csv(final_spp_data, file.path(dir_spp, "final_spp_data.csv"))

spp_status <- final_spp_data %>%
  summarize(status = (1 - mean(risk.wt)) * 100)
