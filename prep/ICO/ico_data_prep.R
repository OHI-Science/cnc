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
  select(species, iucn_status, year, risk.wt) %>%
  unique(.)

write_csv(final_ico_data, file.path(dir_ico, "final_ico_data.csv"))
write_csv(final_ico_data, file.path(dir_layers, "ico_iucn_status_cnc2016_EJP.csv"))

ico_status <- final_ico_data %>%
  summarize(status = (1 - mean(risk.wt)) * 100)

# Trend calculations
## find species with multiple years of assessment

dup_species = final_ico_data$species[duplicated(final_ico_data[,1:2])]

ico_mult <- final_ico_data %>%
  filter(species %in% dup_species) %>%
  select(-iucn_status) %>%
  group_by(species) %>%
  filter(year == max(year) | year == min(year),
         !(species == "minor" & year == 2014 & risk.wt == "0")) %>%
  arrange(year) %>%
  mutate(year = ifelse(year == max(year), "max_year", "min_year")) %>%
  spread(year, risk.wt) %>%
  summarize(trend_species = ifelse(max_year > min_year, -0.5,
                    ifelse(max_year < min_year, 0.5,
                           0))) %>%
  summarize(trend = mean(trend_species))
