# load packages
library(dplyr)
library(tidyr) # install.packages('tidyr')
library(readr) # install.packages('readr')
library(stringr) # install.packages('stringr')

# set directory
dir_ico <- '~/github/cnc/prep/ICO'
setwd(dir_ico)

dir_layers <- '~/github/cnc/eez2016/layers'

# read in raw data file
ico <- read_csv(file.path(dir_ico, 'ico.csv')) %>%
  select(species = Species)

full_spp_data <- read_csv(file.path('~/github/cnc/prep/SPP/full_spp_data.csv'))

#merge data frames
ico_data <- left_join(ico, full_spp_data)

# lookup for weights iucn_status
w.risk_category <-  c('LC' = 0,
                      'NT' = 0.2,
                      'VU' = 0.4,
                      'EN' = 0.6,
                      'CR' = 0.8,
                      'EX' = 1)

#remove records with IUCN weight DD
final_ico_data <- filter(ico_data, !iucn_status == 'DD') %>%
  #Merge w.risk_category with iucn_status
  mutate(risk.wt = w.risk_category[iucn_status]) %>%
  filter(year %in% 2006:2016) %>% ##only selected the past 10 years for biodiversity status -- this could be changed
  group_by(species) %>%
  filter(year == max(year)) %>%
  ungroup %>%
  mutate(rgn_id = 1) %>%
  select(rgn_id, species, iucn_status, year, risk.wt)

write_csv(final_ico_data, file.path(dir_ico, "final_ico_data.csv"))
write_csv(final_ico_data, file.path(dir_layers, "ico_data_cnc2016_EJP.csv"))


ico_status <- final_ico_data %>%
  summarize(status = (1 - mean(risk.wt)) * 100)
