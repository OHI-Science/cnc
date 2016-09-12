##################################
## fisheries
## MRF Feb 17 2016
##################################
library(dplyr)
library(tidyr)

## Directories
dir_cnc = '~/github/cnc'
dir_layers = file.path(dir_cnc, 'layers')
dir_prep   = file.path(dir_cnc, 'prep')
dir_fis = file.path(dir_prep, 'FIS/Fisheries_CSV_Layers')



###########################################################################
## Layers for layers.csv

## Read in data layers
scores <- read.csv(file.path(dir_fis,
                             'fish_sb_sbmsy_no_global.csv'))
colnames(scores) # "region_id" "stock"     "year"      "metric"    "score" "ref"


## select objects for sbmsy

scores.sbmsy = scores %>%
  filter(metric == "sbmsy") %>%
  select(-metric)

landings <- read.csv(file.path(dir_fis,'/fis_catch_reduced_years.csv'))
colnames(landings) # "rgn_id" "stock" "scientific_name"     "year"      "tonnes"



## save to layers folder
write.csv(scores.sbmsy, file.path(dir_layers ,'fis_sbmsy_2016.csv'), row.names=FALSE)
write.csv(landings, file.path(dir_layers ,'fis_landings_2016.csv'), row.names=FALSE)




##########################################################################
## STATUS AND TREND CALCULATIONS
##########################################################################



scores <- read.csv(file.path(dir_fis,
                             'fish_sb_sbmsy_no_global.csv')) %>%
  spread(metric, score)


###########################################################################
## STEP 1: converting SB/SBmsy to B-scores
###########################################################################

#####Global model, model penalized for underexploitation of fish stocks#####
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
plot(score ~ sbmsy, data=B_scores_cnc, type="p") # need to not penalize underfishing as this is not the fishery intent in New Caldonia

#############################################
## calculating the catch weights.
#############################################

landings <- read.csv(file.path(dir_fis,'fis_catch_reduced_years.csv'))

##### Subset the data to include only the most recent 10 years # we are looking at only the years where there are stock assessments and catch data -
#landings <- landings %>%
#  filter(year %in% (max(landings$year)-9):max(landings$year))


## we use the average catch for each stock accross all years
## to obtain weights
str(landings)

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
filter(weights, rgn_id ==1, year==2012)

############################################################
#####   Join scores and weights to calculate status
############################################################
## need to have sbmsy and landings data for each year, must overlap

str(weights)
str(B_scores)

#for global model
status_global <- weights %>%
  left_join(B_scores, by=c('rgn_id', 'year', 'stock')) %>%
  select(rgn_id, year, stock, propCatch, score)        # cleaning data


##for local ncn model
status_cnc <- weights %>%
  left_join(B_scores_cnc, by=c('rgn_id', 'year', 'stock')) %>%
  select(rgn_id, year, stock, propCatch, score)        # cleaning data


### Geometric mean weighted by proportion of catch in each region
status_global <- status_global %>%
  group_by(rgn_id, year) %>%
  summarize(status = prod(score^propCatch)) %>%
  ungroup() %>%
  data.frame()



status_cnc <- status_cnc %>%
  group_by(rgn_id, year) %>%
  summarize(status = prod(score^propCatch)) %>%
  ungroup() %>%
  data.frame()
### To get trend, get slope of regression model based on most recent 5 years
### of data

trend_years <- (max(status_global$year)-4):max(status_global$year)
trend_years <- (max(status_cnc$year)-4):max(status_cnc$year)


trend_global <- status_global %>%
  group_by(rgn_id) %>%
  filter(year %in% trend_years) %>%
  do(mdl = lm(status ~ year, data=.)) %>%
  summarize(rgn_id = rgn_id,
            trend = coef(mdl)['year'] * 5) %>%  ## trend multiplied by 5 to get prediction 5 years out
  ungroup() %>%
  mutate(trend = round(trend, 2))


trend_cnc <- status_cnc %>%
  group_by(rgn_id) %>%
  filter(year %in% trend_years) %>%
  do(mdl = lm(status ~ year, data=.)) %>%
  summarize(rgn_id = rgn_id,
            trend = coef(mdl)['year'] * 5) %>%  ## trend multiplied by 5 to get prediction 5 years out
  ungroup() %>%
  mutate(trend = round(trend, 2))



### final formatting of status data:
status_global <- status_global %>%
  filter(year == max(year)) %>%
  mutate(status = round(status * 100, 1)) %>%
  select(rgn_id, status)

status_cnc <- status_cnc %>%
  filter(year == max(year)) %>%
  mutate(status = round(status * 100, 1)) %>%
  select(rgn_id, status)



##### save the data (eventually these steps will be incorporated into the OHI toolbox)
write.csv(status_global, file.path(dir_fis ,'FIS_status_global.csv'), row.names=FALSE)
write.csv(trend_global,  file.path(dir_fis ,'FIS_trend_global.csv'), row.names = FALSE)

write.csv(status_cnc, file.path(dir_fis ,'FIS_status_cnc.csv'), row.names=FALSE)
write.csv(trend_cnc,  file.path(dir_fis ,'FIS_trend_cnc.csv'), row.names = FALSE)


##############################################
## PLOT RESULTS
library(ggplot2)

##-----------------------------------------------##
## FINAL STATUS AND TREND
## Read in the status and trend csv for plotting
status = read.csv(file.path(dir_fis,
                            'FIS_status.csv'))
trend = read.csv(file.path(dir_fis,
                           'FIS_trend.csv'))


##-----------------------------------------------##
##-----------------------------------------------##
## INITIAL SCORES
## Read in initial scores (code line 14-16) and plot

scores <- read.csv(file.path(dir_fis,
                             'FIS_scores.csv')) %>%
  spread(metric, score)


##proportion of catch
# Pie Chart with Percentages
par(mfrow=c(1,1))
slices<-status %>%
  group_by(stock) %>%
  summarize(status = mean(propCatch)*100)
pct<-round(slices$status)
lbls <- c("Kajikia audax","Thunnus alalunga", "Thunnus albacares", "Thunnus obesus", "Xiphias gladius")
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(pct,labels = lbls, col=rainbow(length(lbls)),

    main="Percent of Catch")

## SBMSY
ggplot(scores) + geom_point(aes(year,sbmsy), color="black") +
  facet_wrap(~stock)

##-----------------------------------------------##
##-----------------------------------------------##
## SCORES based on BBMSY and FFMSY OVER TIME
## Run code up to lines 23-31 and plot rescaled scores
ggplot(B_scores_cnc)+ geom_point(aes(year,score), color="black") +
  facet_wrap(~stock)

##-----------------------------------------------##
##-----------------------------------------------##
## STATUS weighted by propCatch Over time
## Run up to line 83 in the code

#ggplot(status)+ geom_point(aes(year,status)) +
#  xlim(c(2005,2015)) +
#  theme(axis.text.x = element_text(colour="grey20", size=8, angle=90,
    #                               hjust=.5, vjust=.5))


#########################################
# adjusted model
#######################################3
#           still working on this section. looking at gap filling for recent years for
#           sb/sbMsY values to be able to use the latest local catch data
## data prep for Fisheries Goal

#get sbsbmsy predictions for most recent years to gap fill 2013, 2014, 2015
#load stock assessment data
ALB<-read.csv(file.path(dir_fis,
                        'ALB.csv'))## stock assessment data
BET<-read.csv(file.path(dir_fis,
                        'BET.csv'))## stock assessment data
YFT<-read.csv(file.path(dir_fis,
                        'YFT.csv'))## stock assessment data
ALB<- ALB %>%
  select(year, SBSBmsy)#select only year and sbsbmsy
ALB<- ALB %>%
  filter(year %in% (max(landings$year)-9):max(landings$year))## use only the most 10 recent years of stock assessments to predict


BET<- BET %>%
  select(year, SBSBmsy)
BET<- BET %>%
  filter(year %in% (max(landings$year)-9):max(landings$year))## use only the most 10 recent years of stock assessments to predict

YFT<- YFT %>%
  select(year, SBSBmsy)
YFT<- YFT %>%
  filter(year %in% (max(landings$year)-9):max(landings$year))## use only the most 10 recent years of stock assessments to predict

ALB_1<-lm(ALB$SBSBmsy~ALB$year)
summary(ALB_1)

BET_1<-lm(BET$SBSBmsy~BET$year)
summary(BET_1)

YFT_1<-lm(YFT$SBSBmsy~YFT$year)
summary(YFT_1)
summary(YFT_1)$coefficients[2, 1]
