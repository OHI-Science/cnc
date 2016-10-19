##################################
## fisheries
## MRF Feb 17 2016
##################################
library(dplyr)
library(tidyr)

## Directories
dir_cnc = '~/github/cnc/'
dir_cnc_eez = '~/github/cnc/eez2016'
dir_layers = file.path(dir_cnc_eez, 'layers')
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





##########################################################################
## STATUS AND TREND CALCULATIONS
##########################################################################
scores = read.csv(file.path(dir_layers, 'fis_sbmsy_2016.csv'))
landings = read.csv(file.path(dir_layers, 'fis_landings_2016.csv'))


###########################################################################
##converting SB/SBmsy to B-scores
###########################################################################

#####Global model, model penalized for underexploitation of fish stocks##### use this to compare to proof of concept model for fisheries for nc
B_scores <- scores %>%
  mutate(score = ifelse(sbmsy < 0.8 , sbmsy/0.8, NA),
         score = ifelse(sbmsy >= 0.8 & sbmsy < 1.5, 1, score),
         score = ifelse(sbmsy >= 1.5, (3.35 - sbmsy)/1.8, score)) %>%
  mutate(score = ifelse(score <= 0.1, 0.1, score)) %>%
  mutate(score = ifelse(score > 1, 1, score))%>%
  mutate(score_type = "B_score")

# to see what relationship between B/Bmsy and B_score looks like:
plot(score ~ sbmsy, data=B_scores, type="p") # need to not penalize underfishing as this is not the fishery intent in New Caldonia


#####Local model, scores NOT penalized for underexploitation of fish stocks#####
B_scores_cnc <- scores %>%
  mutate(score = ifelse(sbmsy < 0.8 , sbmsy/0.8, NA),
         score = ifelse(sbmsy >= 0.8 & sbmsy < 1.5, 1, score),
         score = ifelse(sbmsy >= 1.5, 1, score)) %>%
  mutate(score = ifelse(score <= 0.1, 0.1, score)) %>%
  mutate(score = ifelse(score > 1, 1, score))%>%
  mutate(score_type = "B_score")

# to see what relationship between B/Bmsy and B_score looks like:
plot(score ~ sbmsy, data=B_scores_ncn, type="p") # need to not penalize underfishing as this is not the fishery intent in New Caldonia



###many of the marlin stocks, opah, and wahoo stocks do not have formal stock assessments
#to include them in the model we used the median score for all the stocks in the region
#and added a penalty of .25 due to unknown status of the stock

B_scores_gf <- B_scores %>% #gap fill data for stocks without formal stock assessment
  group_by(year) %>%
  mutate(Median_score = quantile(score, probs=c(0.5), na.rm=TRUE)) %>%
  ungroup()

B_scores_cnc_gf <- B_scores_cnc %>% #gap fill data for stocks without formal stock assessment
  group_by(year) %>%
  mutate(Median_score = quantile(score, probs=c(0.5), na.rm=TRUE)) %>%
  ungroup()

#can remove this if only using catch by species not species groups # can include this for new caledonia in the future if wanting to include species that do not have species level catch data (ie lobster, other)

#penaltyTable <- data.frame(TaxonPenaltyCode=1:6,
                           #penalty=c(0.1, 0.25, 0.5, 0.8, 0.9, 1))

B_scores_gf <- B_scores_gf %>%
  mutate(score_gf = Median_score*.75) %>%
  mutate(score_gapfilled = ifelse(is.na(score), "Median gapfilled", "none")) %>%
  mutate(score = ifelse(is.na(score), score_gf, score))

B_scores_cnc_gf <- B_scores_cnc_gf %>%
  mutate(score_gf = Median_score*.75) %>%
  mutate(score_gapfilled = ifelse(is.na(score), "Median gapfilled", "none")) %>%
  mutate(score = ifelse(is.na(score), score_gf, score))





write.csv(B_scores_gf, file.path(dir_layers ,'fis_sbmsy_2016gl.csv'), row.names=FALSE)
write.csv(B_scores_cnc_gf, file.path(dir_layers ,'fis_sbmsy_2016cnc.csv'), row.names=FALSE)
write.csv(landings, file.path(dir_layers ,'fis_landings_2016.csv'), row.names=FALSE)


##laod in the layer data for the scores
scores_gl<- read.csv(file.path(dir_layers,
                                        'fis_sbmsy_2016gl.csv'))
scores_cnc<- read.csv(file.path(dir_layers,
                                         'fis_sbmsy_2016cnc.csv'))
landings<- read.csv(file.path(dir_layers,
                                'fis_landings_2016.csv'))
#############################################
## calculating the catch weights.
#############################################


## we use the average catch for each stock accross all years
## to obtain weights

str(landings)
str(scores_gl)

weights <- landings %>%
  group_by(rgn_id,stock) %>%
  mutate(avgCatch = mean(tonnes)) %>%
  ungroup() %>%
  data.frame()



## determine the total proportion of catch each stock accounts for:
weights <- weights %>%
  group_by(rgn_id, year) %>%
  mutate(totCatch = sum(avgCatch)) %>%
  ungroup() %>%
  mutate(propCatch = avgCatch/totCatch)

#### The total proportion of landings for each region/year will sum to one:
filter(weights, rgn_id ==1, year==2011)



##to make an output table with stock, year, catch, prop_catch, and score##
#cnc global model table
status_gl_table <- weights %>%
  left_join(scores_gl, by=c('rgn_id', 'year', 'stock')) %>%
  # remove missing data
  select(rgn_id, year, stock, tonnes, propCatch, score)

status_gl_table<-as.data.frame(status_gl_table)

#cnc regional model table
status_cnc_table <- weights %>%
  left_join(scores_cnc, by=c('rgn_id', 'year', 'stock')) %>%
  # remove missing data
  select(rgn_id, year, stock, tonnes, propCatch, metric, sbmsy,score)

status_cnc_table<-as.data.frame(status_cnc_table)


write.csv(status_gl_table, file.path(dir_prep ,'status_gl_table.csv'), row.names=FALSE)##not sure why it isnt able to write to directory
############################################################
#####   Join scores and weights to calculate status
############################################################


#for global model
status_gl <- weights %>%
  left_join(scores_gl, by=c('rgn_id', 'year', 'stock')) %>%

  select(rgn_id, year, stock, propCatch, score)

status_gl <- status_gl %>%
  group_by(rgn_id, year) %>%
  summarize(status_gl = prod(score^propCatch)) %>%
  ungroup() %>%
  data.frame()

#final formatting for status from global model
status_gl <- status_gl %>%
  filter(year == max(year)) %>%
  mutate(status_cnc = round(status_gl * 100, 1)) %>%
  select(rgn_id, status_cnc)
#42.9 for global model using nc data from 2011-2015 but with penaly for underfishing


trend_years <- (max(scores_gl$year)-4):max(scores_gl$year)#dont really need since we are only using the recent 5 yrs of data

trend_gl <- status_gl %>%
  group_by(rgn_id) %>%
  filter(year %in% trend_years) %>%
  do(mdl = lm(status_gl ~ year, data=.)) %>%
  summarize(rgn_id = rgn_id,
            trend = coef(mdl)['year'] * 5) %>%  ## trend multiplied by 5 to get prediction 5 years out
  ungroup() %>%
  mutate(trend = round(trend, 2))



#######for local cnc model###########3
status_cnc <- weights %>%
  left_join(scores_cnc, by=c('rgn_id', 'year', 'stock')) %>%
  # remove missing data
  select(rgn_id, year, stock, propCatch, score)        # cleaning data


### Geometric mean weighted by proportion of catch in each region
status_cnc <- status_cnc %>%
  group_by(rgn_id, year) %>%
  summarize(status_cnc = prod(score^propCatch)) %>%
  ungroup() %>%
  data.frame()

##cnc fisheries score is 95.2

### To get trend, get slope of regression model based on most recent 5 years
### of data

trend_years <- (max(status_cnc$year)-4):max(status_cnc$year)#dont really need since we are only using the recent 5 yrs of data

trend_cnc <- status_cnc %>%
  group_by(rgn_id) %>%
  filter(year %in% trend_years) %>%
  do(mdl = lm(status_cnc ~ year, data=.)) %>%
  summarize(rgn_id = rgn_id,
            trend = coef(mdl)['year'] * 5) %>%  ## trend multiplied by 5 to get prediction 5 years out
  ungroup() %>%
  mutate(trend = round(trend, 2))

##0 slope so no trend for cnc fis status

### final formatting of status data for new caledonia model
status_cnc <- status_cnc %>%
  filter(year == max(year)) %>%
  mutate(status_cnc = round(status_cnc * 100, 1)) %>%
  select(rgn_id, status_cnc)



##### save the data
#New Caledonia Model
write.csv(status_cnc, file.path(dir_layers ,'FIS_status_cnc.csv'), row.names=FALSE)
write.csv(trend_cnc,  file.path(dir_layers ,'FIS_trend_cnc.csv'), row.names = FALSE)

#Global model for senario testing
write.csv(status_gl, file.path(dir_layers ,'fis_status_gl.csv'), row.names=FALSE)
write.csv(trend_gl,  file.path(dir_layers ,'fis_trend_gl.csv'), row.names = FALSE)
##############################################
## PLOT RESULTS
library(ggplot2)



## SBMSY
ggplot(scores_gl) + geom_point(aes(year,sbmsy), color="black") +
  facet_wrap(~stock)


ggplot(scores_cnc) + geom_point(aes(year,sbmsy), color="black") +
  facet_wrap(~stock)






















