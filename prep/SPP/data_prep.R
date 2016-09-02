# load packages
library(dplyr)
library(tidyr) # install.packages('tidyr')
library(readr) # install.packages('readr')

# set directory
dir_lsp <- '~/github/cnc/prep/ICO'
setwd(dir_lsp)

# read in raw data file
iucn_doc <- read_csv(file.path(dir_lsp, 'nc_iucn_spp.csv')) %>%
  mutate(rgn_id = 1) %>%
  select(rgn_id, species = `Species`, iucn_status = `Red List status`, year = `Year assessed`)

# lookup for weights status
w.risk_category <-  c('LC' = 0,
                    'NT' = 0.2,
                    'VU' = 0.4,
                    'EN' = 0.6,
                    'CR' = 0.8,
                    'EX' = 1)

#remove duplicated rows
iucn_spp <- distinct(iucn_doc)

#remove records with IUCN weight DD
iucn_values <- filter(iucn_spp, !iucn_status == 'DD') %>%
# replace LR/cd = NT, LR/nt = NT, LR/lc = LC based on http://www.iucnredlist.org/static/categories_criteria_2_3
  mutate(iucn_status = stringr::str_replace_all(iucn_status, "LR/cd", "NT"),
         iucn_status = stringr::str_replace_all(iucn_status, "LR/nt", "NT"),
         iucn_status = stringr::str_replace_all(iucn_status, "LR/lc", "LC")) %>%
#Merge w.risk_category with iucn_status
  mutate(risk.wt = w.risk_category[iucn_status])
