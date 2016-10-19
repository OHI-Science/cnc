##################################
## fisheries
## EMS Oct 2016
##################################
library(dplyr)
library(tidyr)

## Directories

dir_cnc = '~/github/cnc/'
dir_cnc_eez = '~/github/cnc/eez2016'
dir_layers = file.path(dir_cnc_eez, 'layers')
dir_prep   = file.path(dir_cnc, 'prep')
dir_fis = file.path(dir_prep, 'FIS/Fisheries_CSV_Layers')





##########################################################################
## STATUS AND TREND CALCULATIONS
##########################################################################
scores = read.csv(file.path(dir_layers, 'fis_sbmsy_2016.csv'))
landings = read.csv(file.path(dir_layers, 'fis_landings_2016.csv'))
###########################################################################
##converting SB/SBmsy to B-scores
###########################################################################

str(scores)


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



###many of the marlin stocks, opah, and wahoo stocks do not have formal stock assessments
#to include them in the model we used the median score for all the stocks in the region
#and added a penalty of .25 due to unknown status of the stock



B_scores_cnc_gf <- B_scores_cnc %>% #gap fill data for stocks without formal stock assessment
  group_by(year) %>%
  mutate(Median_score = quantile(score, probs=c(0.5), na.rm=TRUE)) %>%
  ungroup()





B_scores_cnc_gf <- B_scores_cnc_gf %>%
  mutate(score_gf = Median_score*.75) %>%
  mutate(score_gapfilled = ifelse(is.na(score), "Median gapfilled", "none")) %>%
  mutate(score = ifelse(is.na(score), score_gf, score))




write.csv(B_scores_cnc_gf, file.path(dir_layers ,'fis_sbmsy_2016cnc.csv'), row.names=FALSE)
write.csv(landings, file.path(dir_layers ,'fis_landings_2016.csv'), row.names=FALSE)


##laod in the layer data for the scores
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
str(scores_cnc)

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

#cnc regional model table
status_cnc_table <- weights %>%
  left_join(scores_cnc, by=c('rgn_id', 'year', 'stock')) %>%
  # remove missing data
  select(rgn_id, year, stock, tonnes, propCatch, metric, sbmsy,score)

status_cnc_table<-as.data.frame(status_cnc_table)


############################################################
#####   Join scores and weights to calculate status
############################################################


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

### final formatting of status data:
status_cnc <- status_cnc %>%
  filter(year == max(year)) %>%
  mutate(status_cnc = round(status_cnc * 100, 1)) %>%
  select(rgn_id, status_cnc)


##### save the data
write.csv(status_cnc, file.path(dir_layers ,'FIS_status_cnc.csv'), row.names=FALSE)
write.csv(trend_cnc,  file.path(dir_layers ,'FIS_trend_cnc.csv'), row.names = FALSE)



##############################################
## PLOT RESULTS
library(ggplot2)



## SBMSY
ggplot(scores_gl) + geom_point(aes(year,sbmsy), color="black") +
  facet_wrap(~stock)


ggplot(scores_cnc) + geom_point(aes(year,sbmsy), color="black") +
  facet_wrap(~stock)






















