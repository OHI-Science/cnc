# load packages
library(dplyr)
library(tidyr) # install.packages('tidyr')
library(readr) # install.packages('readr')

# set directory
dir_lsp <- '~/github/cnc/prep/LSP'
setwd(dir_lsp)

dir_layers <- '~/github/cnc/eez2016/layers'

# read in raw data file
file <- read_csv(file.path(dir_lsp, 'LSP_2016_updated.csv'))

#remove non-data columns
file_data <- select(file, -Note, -Type, -Designation)

file_data_mngmt <- file_data %>%
  filter(!is.na(rgn_id)) %>%
  mutate(Management = ifelse(is.na(Management), 'No plan', Management))

#rename variables with no spaces
lsp_data <- plyr::rename(file_data_mngmt, c("Surface Km2" = "km2", "IUCN Cat" = "iucn_cat")) %>%
  filter(!Year== 2017) #remove row 7 (2017) since it denotes a potential future protection area -- not the present

# add protection status weight
## weight were created by E. Pacheco. Could be potential area for manipulation
lsp_protection_cat <- data.frame(iucn_cat = c("Cat Ia", "Cat II", "Cat III",
                                         "Cat IV", "Cat V", "Cat VI"
                                         ),
                             prot_values = c(1, 0.8, 0.6,
                                             0.4, 0.2, 0.1) )

lsp_with_wt <- left_join(lsp_data, lsp_protection_cat )

#add management plan status weight
## weights were created by E. Pacheco/M. Imirizaldu. Could be potential area for manipulation
management_wt <- data.frame(Management = c("Management Plan Implemented", "Management Plan",
                                              "Management Plan in Development", "No plan"
                                              ),
                         mngmt_values = c(1, 0.75,
                                         0.5, 0.1) )

lsp_w_mngmt <- left_join(lsp_with_wt, management_wt, by = "Management") %>%
  select(rgn_id, Year, km2, iucn_cat, prot_values, Management, mngmt_values) %>%
  mutate(rel_protection = km2 * prot_values * mngmt_values) %>%
  arrange(Year) %>%
  mutate(km2_cum = round(cumsum(km2), 2))

write_csv(lsp_w_mngmt, file.path(dir_layers, "lsp_iucn_mngmt_cnc2016_EJP.csv"))

#calculations of status with temporal gapfilling and cummulative protection areas

lsp_final_data <- lsp_w_mngmt %>%
  ##protected areas 0.216, 0.226, 0.191, and 0.270 are within 1067.097
  group_by(Year, prot_values, mngmt_values) %>%
  summarize(tot_area = sum(km2)) %>%
  ungroup() %>%
  mutate(tot_area = ifelse(tot_area == 1067.097, 1067.097-0.442-0.461, tot_area)) %>%
  # all these are within 1288864.900
  mutate(tot_area = ifelse(tot_area == 1288864.900, 1288864.900-1066.194, tot_area)) %>%
  mutate(rel_protection = tot_area * prot_values * mngmt_values)

lsp_status <- lsp_final_data %>%
  group_by(Year) %>% # calculate rel_prot_total by year
  summarise(rel_protection_total = sum(rel_protection)) %>%
  ungroup() %>%
  arrange(Year) %>%
  mutate(cum_rel_prot = cumsum(rel_protection_total)) %>%
  complete(Year = 2008:2016) %>%
  fill(cum_rel_prot) %>%

  #proposing different reference points

  ##MANAGEMENT: all versions are at Management Plan Implemented = 1

  ## v1: Reference point is 1288864.900 minus the remaining protected areas on the list (total cumulative protection in 2016 = 1287798.706) at IUNC category VI (score = 0.1)
  mutate(ref_point_1 = 1287798.706 * 0.1 * 1,
         status_1 = round((cum_rel_prot / ref_point_1) * 100, 2)) %>%
  ## v2: Reaching 60% of EEZ Protected as Cat II (KBA) = 0.8 & 40% of EEZ Protected as Category VI
  mutate(ref_point_2 = (((1287798.706 * .6) * 0.8 * 1) + (1287798.706 * .4) * 0.1 * 1),
         status_2 = round((cum_rel_prot / ref_point_2) * 100, 2)) %>%
  ## v3: Reaching 70% of EZZ as Cat IV, 30% of entire EEZ as Cat I (working group)
  ### use this model for the final version
  mutate(ref_point_3 = (((1287798.706 * .7) * 0.4 * 1) + ((1287798.706 * .3) * 1 * 1)),
         status_3 = round((cum_rel_prot / ref_point_3) * 100, 2)) %>%
  mutate(rgn_id = 1) %>%
  select(rgn_id, Year, cum_rel_prot, ref_point_1, status_1, ref_point_2, status_2, ref_point_3, status_3)
  # save file as csv
  write_csv(lsp_status, file.path(dir_lsp, 'lsp_status.csv'))


# calculate trend
## Trend status_1
lsp_trend_1 <- lsp_status %>%
  filter(Year %in% 2012:2016) %>%
  do(mod = lm(status_1 ~ Year, data = .)) %>%
  do(
    dimension = 'trend',
    score     =  max(min(coef(.$mod)[['Year']] * 0.05, 1), -1))

## Trend status_2
lsp_trend_2 <- lsp_status %>%
  filter(Year %in% 2012:2016) %>%
  do(mod = lm(status_2 ~ Year, data = .)) %>%
  do(
    dimension = 'trend',
    score     =  max(min(coef(.$mod)[['Year']] * 0.05, 1), -1))

## Trend status_3
lsp_trend_3 <- lsp_status %>%
  filter(Year %in% 2012:2016) %>%
  do(mod = lm(status_3 ~ Year, data = .)) %>%
  do(
    dimension = 'trend',
    score     =  max(min(coef(.$mod)[['Year']] * 0.05, 1), -1))
