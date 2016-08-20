# load packages
library(dplyr)
library(tidyr) # install.packages('tidyr')
library(readr) # install.packages('readr')
library(plyr)

# set directory
dir_lsp <- '~/github/cnc/prep/LSP'
setwd(dir_lsp)

# read in raw data file
file <- read_csv(file.path(dir_lsp, 'LSP_2016_updated.csv'))

#remove non-data columns
file_data <- select(file, -Note, -Type, -Designation)

file_data_mngmt <- file_data %>%
  filter(!is.na(rgn_id)) %>%
  mutate(Management = ifelse(is.na(Management), 'No plan', Management))

#rename variables with no spaces
lsp_data <- rename(file_data_mngmt, c("Surface Km2" = "km2", "IUCN Cat" = "iucn_cat")) %>%
  filter(!Year== 2017) #remove row 7 (2017) since it denotes a potential future protection area -- not the present

# add protection status weight
## weight were created by E. Pacheco. Could be potential area for manipulation
lsp_protection_cat <- data.frame(iucn_cat = c("Cat Ia", "Cat II", "Cat III",
                                         "Cat IV", "Cat V", "Cat VI"
                                         ),
                             prot_values = c('1', '0.8', '0.6',
                                             '0.4', '0.2', '0.1') )

lsp_with_wt <- left_join(lsp_data, lsp_protection_cat )

#add management plan status weight
## weights were created by E. Pacheco. Could be potential area for manipulation
management_wt <- data.frame(Management = c("Management Plan Implemented", "Management Plan",
                                              "Management Plan in Development", "No plan"
                                              ),
                         mngmt_values = c('1', '0.5',
                                         '0.25', '0.1') )

lsp_w_mngmt <- left_join(lsp_with_wt, management_wt, by = "Management") %>%
  select(rgn_id, Year, km2, iucn_cat, prot_values, Management, mngmt_values) %>%
  mutate(prot_values = as.numeric(prot_values), mngmt_values = as.numeric(mngmt_values) ) %>%
  mutate(rel_protection = km2 * prot_values * mngmt_values)
