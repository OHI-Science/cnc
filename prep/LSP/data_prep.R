# load packages
library(dplyr)
library(tidyr) # install.packages('tidyr')
library(readr) # install.packages('readr')

# set directory
dir_lsp <- '~/github/cnc/prep/LSP'
setwd(dir_lsp)

# read in raw data file
lsp <- read_csv(file.path(dir_lsp, 'cnc_LSP_2016.csv'))

lsp <- lsp[, colSums(is.na(lsp)) < nrow(lsp)] # remove NA columns

colnames(lsp)[9] <- "comments"

# remove N/A empty columns and rows (ie. comments) and Note
lsp <- select(lsp, -comments, -Note, -Designation)

# add protection status weight
protection <- data.frame(IUCN_eq = sample(1:6, 14, replace = TRUE))

values <-  c('1', '0.8', '0.6', '0.4', '0.2', '0.1')
lsp$protection_weight <- values[lsp$IUCN_eq]

#remove non-data columns
lsp <- select(lsp, -Type, -Category, -IUCN_eq)

lsp <- rename(lsp, c("Surface Km2" = "km2"))

#Create proportional protected area
lsp_with_prot_wt <- lsp %>%
  mutate(prot_wt = km2 * as.numeric(protection_weight))

