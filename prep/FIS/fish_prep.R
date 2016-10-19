##################################
## fisheries layer prep
## EMS Oct 2016
##################################
library(dplyr)
library(tidyr)

## Directories
dir_cnc = '~/github/cnc/eez2016'
dir_layers = file.path(dir_cnc, 'layers')
dir_prep   = file.path(dir_cnc, 'prep')
dir_fis = file.path(dir_prep, 'FIS/Fisheries_CSV_Layers')


###########################################################################
#data prep
################################
#sb_sbmsy is only current until 2012 so we used the most current 10 yrs of data on sb_sbmsy to run a regression model to gap fill sb-sbmsy estimates for 2013-2015.
###############################
ALB<-read.csv(file.path(dir_fis,
                        'ALB.csv'))## stock assessment data
BET<-read.csv(file.path(dir_fis,
                        'BET.csv'))## stock assessment data
YFT<-read.csv(file.path(dir_fis,
                        'YFT.csv'))## stock assessment data

Xip<-read.csv(file.path(dir_fis,
                        'Xip.csv'))## stock assessment data


ALB<- ALB %>%
  select(year, SBSBmsy)#select only year and sbsbmsy
ALB<- ALB %>%
  filter(year %in% (max(ALB$year)-9):max(ALB$year))## use only the most 10 recent years of stock assessments to predict


BET<- BET %>%
  select(year, SBSBmsy)
BET<- BET %>%
  filter(year %in% (max(BET$year)-9):max(BET$year))## use only the most 10 recent years of stock assessments to predict

YFT<- YFT %>%
  select(year, SBSBmsy)
YFT<- YFT %>%
  filter(year %in% (max(YFT$year)-9):max(YFT$year))## use only the most 10 recent years of stock assessments to predict


Xip<- Xip %>%
  select(year, SBSBmsy)#select only year and sbsbmsy
Xip<- Xip %>%
  filter(year %in% (max(Xip$year)-9):max(Xip$year))## use only the most 10 recent years of stock assessments to predict


ALB_1<-lm(ALB$SBSBmsy~ALB$year)
summary(ALB_1)

BET_1<-lm(BET$SBSBmsy~BET$year)
summary(BET_1)

YFT_1<-lm(YFT$SBSBmsy~YFT$year)
summary(YFT_1)
summary(YFT_1)$coefficients[2, 1]



Xip_1<-lm(Xip$SBSBmsy~Xip$year)
summary(Xip_1)

###used regression coefficents from the model to gap fil in excel (fish_sb_sbmsy_allsp.csv" and then read in the data below##

####### Read in data layers###########
scores <- read.csv(file.path(dir_fis,
                             'fish_sb_sbmsy_allsp.csv'))
colnames(scores) # "region_id" "stock"     "year"      "metric"    "score" "ref"


## select objects for sbmsy

scores.sbmsy = scores %>%
  filter(metric == "sbmsy") %>%
  select(-metric)

landings <- read.csv(file.path(dir_fis,'/fis_catch.csv'))
colnames(landings) # "rgn_id" "stock" "scientific_name"     "year"      "tonnes"

###only use the 5 most recent years of data

scores<- scores %>%
  filter(year %in% (max(scores$year)-4):max(scores$year))

landings<- landings %>%
  filter(year %in% (max(landings$year)-4):max(landings$year))

## save to layers folder #layer data for fis represents data from 2011-2015
write.csv(scores, file.path(dir_layers ,'fis_sbmsy_2016.csv'), row.names=FALSE)
write.csv(landings, file.path(dir_layers ,'fis_landings_2016.csv'), row.names=FALSE)


