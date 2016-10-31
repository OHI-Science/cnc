FIS= function(layers){
  ## FIS - Modeled for New Caledonia using two senarios - global (does not incorperate local philosphy of consistent catches of pelagics and uses the global model to penalize underfishing of stocks
  # and New Caledonia model (files_cnc) and does not penalize for underfishing a stock but does penalize for over fishing)#
  #load data
  #layers used fis_sbmsy_2016.csv, fis_landings_2016.csv, fis_sbmsy_2016gl.csv, fis_sbmsy_2016cnc.csv, fis_landings_2016.csv, FIS_status_cnc.cvs, fis_status_gl.csv
  #FIS_trend_cnc.csv, fis_trend_gl.csv


  scores_gl <- SelectLayersData(layers, layers='fis_sbmsy_2016gl')
  scores_cnc <- SelectLayersData(layers, layers='fis_sbmsy_2016cnc')
  # landings <- SelectLayersData(layers, layers='fis_landings_2016')
  landings <- layers$data[['fis_landings_2016']] %>%
    select(-layer)

  str(scores_gl)
  scores_gl <- select(scores_gl, rgn_id = id_num, stock=category, year, score=val_num)
  str(scores_cnc)
  scores_cnc <- select(scores_cnc, rgn_id = id_num, stock=category, year, score=val_num)
  str(landings)

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


 ############################################################
  #####   Join scores and weights to calculate status
  ############################################################
  #Senario 2
#
#
#   #for global model/senario
#   status_gl <- weights %>%
#     left_join(scores_gl, by=c('rgn_id', 'year', 'stock')) %>%
#     select(rgn_id, year, stock, propCatch, score)
#
#   status_gl <- status_gl %>%
#     group_by(rgn_id, year) %>%
#     summarize(status_gl = prod(score^propCatch)) %>%
#     ungroup() %>%
#     data.frame()
#
#
#   trend_years <- (max(scores_gl$year)-4):max(scores_gl$year)#dont really need since we are only using the recent 5 yrs of data
#
#   trend_gl <- status_gl %>%
#     group_by(rgn_id) %>%
#     filter(year %in% trend_years) %>%
#     do(mdl = lm(status_gl ~ year, data=.)) %>%
#     summarize(rgn_id = rgn_id,
#               trend_gl = coef(mdl)['year'] * 5) %>%  ## trend multiplied by 5 to get prediction 5 years out
#     ungroup() %>%
#     mutate(trend = round(trend_gl, 2),
#            dimension = 'trend')%>%
#     select(rgn_id, score=trend_gl, dimension)
#
#    `trend_gl<-as.data.frame(trend_gl)
#
#
#
#   #final formatting for status from global model/senario
#   status_gl <- status_gl %>%
#     filter(year == max(year)) %>%
#     mutate(status_gl = round(status_gl * 100, 1),
#       dimension = 'status')%>%
#     select(rgn_id, score=status_gl, dimension)
#   #42.9 for global model using nc data from 2011-2015 but with penaly for underfishing
#
#  fis_status <- status_gl %>%
#    filter(year == max(year)) %>%
#    mutate(score =round(status_gl*100,1), #not sure why *100 is not working in this code added this code below
#           dimension = 'status')%>%
#    select(rgn_id, score=status_gl, dimension)

#  fis_status$score<-fis_status$score*100 #translates the proportion score into a percentage

  # # return scores for New Caledonia Model
  # scores <-  rbind(status_gl, trend_gl) %>%
  #   mutate('goal'='FIS') %>%
  #   select(goal, dimension, region_id = rgn_id, score) %>%
  #   data.frame()


   #######Senario 1 - for local cnc model###########3
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
    mutate(trend = round(trend, 2),
           dimension = 'trend')%>%
    select(rgn_id, score=trend, dimension)

  trend_cnc<-as.data.frame(trend_cnc)
  ##0 slope so no trend for cnc fis status

  ### final formatting of status data for new caledonia model
  fis_status <- status_cnc %>%
    filter(year == max(year)) %>%
    mutate(score =round(status_cnc*100,1), #not sure why *100 is not working in this code added this code below
      dimension = 'status')%>%
    select(rgn_id, score=status_cnc, dimension)

  fis_status$score<-fis_status$score*100 #translates the proportion score into a percentage

  ##cnc fisheries score is 95.2


  ##NEED TO CHOOSE ONE OF THE SENARIOS BELOW AND COMMENT OUT THE OTHER DEPENDING ON WHAT MODEL/SENARIO YOU WANT TO USE (GLOBAL OR NEW CALEDONIA)
  #global senario - penalizes underfishing
  # return scores for global model using regional data but global B model
  #scores <-  rbind(status_gl, trend_gl) %>%
  #  mutate('goal'='FIS') %>%
  #  select(goal, dimension, rgn_id, score) %>%
  #  data.frame()


  # return scores for New Caledonia Model
  scores <-  rbind(fis_status, trend_cnc) %>%
    mutate('goal'='FIS') %>%
    select(goal, dimension, region_id = rgn_id, score) %>%
    data.frame()

  return(scores)

}
######end of fis code

# Removing MAR from all calculations
# MAR = function(layers, status_year){
#
#    # layers used: mar_harvest_tonnes, mar_harvest_species, mar_sustainability_score, mar_coastalpopn_inland25mi, mar_trend_years
#   harvest_tonnes <- SelectLayersData(layers, layers='mar_harvest_tonnes', narrow = TRUE) %>%
#     select(rgn_id=id_num, species_code=category, year, tonnes=val_num)
#
#   sustainability_score <- SelectLayersData(layers, layers='mar_sustainability_score', narrow = TRUE) %>%
#     select(rgn_id=id_num, species_code=category, sust_coeff=val_num)
#
#   popn_inland25mi <- SelectLayersData(layers, layers='mar_coastalpopn_inland25mi', narrow = TRUE) %>%
#     select(rgn_id=id_num, year, popsum=val_num)
#
#   rky <-  harvest_tonnes %>%
#     left_join(sustainability_score, by = c('rgn_id', 'species_code'))
#
#   # fill in gaps with no data
#   rky <- spread(rky, year, tonnes)
#   rky <- gather(rky, "year", "tonnes", 4:dim(rky)[2])
#
#
#   # 4-year rolling mean of data
#   m <- rky %>%
#     mutate(year = as.numeric(as.character(year))) %>%
#     group_by(rgn_id, species_code, sust_coeff) %>%
#     arrange(rgn_id, species_code, year) %>%
#     mutate(sm_tonnes = zoo::rollapply(tonnes, 4, mean, na.rm=TRUE, partial=TRUE)) %>%
#     ungroup()
#
#   # smoothed mariculture harvest * sustainability coefficient
#   m <- m %>%
#     mutate(sust_tonnes = sust_coeff * sm_tonnes)
#
#
#   # aggregate all weighted timeseries per region, and divide by coastal human population
#   ry = m %>%
#     group_by(rgn_id, year) %>%
#     summarize(sust_tonnes_sum = sum(sust_tonnes, na.rm=TRUE)) %>%  #na.rm = TRUE assumes that NA values are 0
#     left_join(popn_inland25mi, by = c('rgn_id','year')) %>%
#     mutate(mar_pop = sust_tonnes_sum / popsum) %>%
#     ungroup()
#
#
#   # get reference quantile based on argument years
#   ref_95pct_data <- ry %>%
#     filter(year <= status_year)
#
#   ref_95pct <- quantile(ref_95pct_data$mar_pop, 0.95, na.rm=TRUE)
#
#   # identify reference rgn_id
#   ry_ref = ref_95pct_data %>%
#     arrange(mar_pop) %>%
#     filter(mar_pop >= ref_95pct)
#   message(sprintf('95th percentile for MAR ref pt is: %s\n', ref_95pct)) # rgn_id 25 = Thailand
#   message(sprintf('95th percentile rgn_id for MAR ref pt is: %s\n', ry_ref$rgn_id[1])) # rgn_id 25 = Thailand
#
#   rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
#     rbind(data.frame(goal = "MAR", method = "spatial 95th quantile",
#                      reference_point = paste0("region id: ", ry_ref$rgn_id[1], ' value: ', ref_95pct)))
#   write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)
#
#
#   ry = ry %>%
#     mutate(status = ifelse(mar_pop / ref_95pct > 1,
#                            1,
#                            mar_pop / ref_95pct))
#   status <- ry %>%
#     filter(year == status_year) %>%
#     select(rgn_id, status) %>%
#     mutate(status = round(status*100, 2))
#
#
#   # get MAR trend
#   trend = ry %>%
#     group_by(rgn_id) %>%
#     do(mdl = lm(status ~ year, data=., subset=year %in% (status_year-4):(status_year))) %>%
#     summarize(rgn_id, trend = coef(mdl)['year'] * 5) %>%
#     ungroup()
#
#   trend <- trend %>%
#     mutate(trend = ifelse(trend>1, 1, trend)) %>%
#     mutate(trend = ifelse(trend<(-1), (-1), trend)) %>%
#     mutate(trend = round(trend, 2))
#
#   # return scores
#   scores = status %>%
#     select(region_id = rgn_id,
#            score     = status) %>%
#     mutate(dimension='status') %>%
#     rbind(
#       trend %>%
#         select(region_id = rgn_id,
#                score     = trend) %>%
#         mutate(dimension = 'trend')) %>%
#     mutate(goal='MAR')
#
#   return(scores)
# }


# FP = function(layers, scores){
#
#   # weights
#   w <-  SelectLayersData(layers, layers='fp_wildcaught_weight', narrow = TRUE) %>%
#     select(region_id = id_num, w_FIS = val_num); head(w)
#
#   # scores
#   s <- scores %>%
#     filter(goal %in% c('FIS', 'MAR')) %>%
#     filter(!(dimension %in% c('pressures', 'resilience'))) %>%
#     left_join(w, by="region_id")  %>%
#     mutate(w_MAR = 1 - w_FIS) %>%
#     mutate(weight = ifelse(goal == "FIS", w_FIS, w_MAR))
#
#
#   ## Some warning messages due to potential mismatches in data:
#   # NA score but there is a weight
#   tmp <- filter(s, goal=='FIS' & is.na(score) & (!is.na(w_FIS) & w_FIS!=0) & dimension == "score")
#   if(dim(tmp)[1]>0){
#     warning(paste0("Check: these regions have a FIS weight but no score: ",
#                    paste(as.character(tmp$region_id), collapse = ", ")))}
#
#   tmp <- filter(s, goal=='MAR' & is.na(score) & (!is.na(w_MAR) & w_MAR!=0) & dimension == "score")
#   if(dim(tmp)[1]>0){
#     warning(paste0("Check: these regions have a MAR weight but no score: ",
#                    paste(as.character(tmp$region_id), collapse = ", ")))}
#
#   # score, but the weight is NA or 0
#   tmp <- filter(s, goal=='FIS' & (!is.na(score) & score > 0) & (is.na(w_FIS) | w_FIS==0) & dimension == "score" & region_id !=0)
#   if(dim(tmp)[1]>0){
#     warning(paste0("Check: these regions have a FIS score but no weight: ",
#                    paste(as.character(tmp$region_id), collapse = ", ")))}
#
#   tmp <- filter(s, goal=='MAR' & (!is.na(score) & score > 0) & (is.na(w_MAR) | w_MAR==0) & dimension == "score" & region_id !=0)
#   if(dim(tmp)[1]>0){
#     warning(paste0("Check: these regions have a MAR score but no weight: ",
#                    paste(as.character(tmp$region_id), collapse = ", ")))}
#
#   s <- s  %>%
#     group_by(region_id, dimension) %>%
#     summarize(score = weighted.mean(score, weight, na.rm=TRUE)) %>%
#     mutate(goal = "FP") %>%
#     ungroup() %>%
#     select(region_id, goal, dimension, score) %>%
#     data.frame()
#
#   # return all scores
#   return(rbind(scores, s))
# }


AO = function(layers,
              status_year,
              Sustainability=1.0){

  # cast data
  layers_data = SelectLayersData(layers, targets='AO')

  year_min=max(min(layers_data$year, na.rm = TRUE), status_year - 10)

  r <- layers_data %>%
    filter(layer == 'ao_access') %>%
    select(region_id=id_num, access=val_num)
  r <- na.omit(r)

  ry <- layers_data %>%
    filter(layer == 'ao_need') %>%
    select(region_id = id_num, year, need=val_num) %>%
    left_join(r, by="region_id")


  # model

  ry <- ry %>%
    mutate(Du = (1 - need) * (1 - access)) %>%
    mutate(statusData = (1 - Du) * Sustainability)

  # status
  r.status <- ry %>%
    filter(year==status_year) %>%
    select(region_id, statusData) %>%
    mutate(status=statusData*100)
  summary(r.status); dim(r.status)

  # trend
  r.trend <- ry %>%
    filter(year >= year_min) %>%
    filter(!is.na(statusData)) %>%
    group_by(region_id) %>%
    arrange(year) %>%
    top_n(5, year) %>%
    ungroup()


  r.trend <- r.trend %>%
    group_by(region_id) %>%
    do(mdl = lm(statusData ~ year, data=.)) %>%
    summarize( region_id = region_id,
               trend = coef(mdl)['year']*5) %>%
    ungroup()

  ## reference points
  rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "AO", method = "??",
                     reference_point = NA))
  write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)


  # return scores
  scores = r.status %>%
    select(region_id, score=status) %>%
    mutate(dimension='status') %>%
    rbind(
      r.trend %>%
        select(region_id, score=trend) %>%
        mutate(dimension='trend')) %>%
    mutate(goal='AO') # dlply(scores, .(dimension), summary)
  return(scores)
}

NP <- function(scores, layers, status_year, debug = FALSE){

  #replacing NP data with global scores since the global data gaps artificially lower the score
  np_status <- data.frame(rgn_id = 1,
                          score = 100,
                          dimension = 'status')

  np_trend <- data.frame(rgn_id = 1,
                          score = 1,
                          dimension = 'trend')

  scores_NP <- rbind(np_status, np_trend) %>%
    mutate(goal = "NP",
           rgn_id = as.integer(rgn_id)) %>%
    select(region_id = rgn_id, goal, dimension, score)

  # return scores
  return(scores_NP)
}


  # ### new code version - load combined harvest variables
  # r_cyanide    = layers$data[['np_cyanide']] # cyanide & blast used to calculate risk variable
  # r_blast      = layers$data[['np_blast']]
  # hab_extent   = layers$data[['hab_extent']] # used to calculate exposure variable
  #
  # ### FIS status for fish oil sustainability
  # #FIS_status <- read.csv('scores.csv')%>%
  # FIS_status   <-  scores %>%
  #   filter(goal == 'FIS' & dimension == 'status') %>%
  #   select(rgn_id = region_id, score)
  #
  #
  # ###########################################################.
  # ### Here I define five main sub-functions.  The main script that
  # ### actually calls these functions is at the very end of the NP section.
  # ###   np_rebuild_harvest
  # ###   np_calc_exposure
  # ###   np_calc_risk
  # ###   np_calc_sustainability
  # ###   np_calc_scores
  #
  # np_rebuild_harvest <- function(layers) {
  #   ### Reassembles NP harvest information from separate data layers:
  #   ### [rgn_name  rgn_id  product  year  tonnes  tonnes_rel  prod_weight]
  #   #########################################.
  #
  #   ## load data from layers dataframe
  #   rgns         <- layers$data[['rgn_labels']]
  #   h_tonnes     <- layers$data[['np_harvest_tonnes']]
  #   h_tonnes_rel <- layers$data[['np_harvest_tonnes_relative']]
  #   h_w          <- layers$data[['np_harvest_product_weight']]
  #
  #   # merge harvest in tonnes and usd
  #   np_harvest <- h_tonnes %>%
  #     full_join(
  #       h_tonnes_rel,
  #       by=c('rgn_id', 'product', 'year')) %>%
  #     left_join(
  #       h_w %>%
  #         select(rgn_id, product, prod_weight = weight),
  #       by=c('rgn_id', 'product')) %>%
  #     left_join(
  #       rgns %>%
  #         select(rgn_id, rgn_name=label),
  #       by='rgn_id') %>%
  #     select(
  #       rgn_name, rgn_id, product, year,
  #       tonnes, tonnes_rel, prod_weight) %>%
  #     group_by(rgn_id, product)
  #
  #   return(np_harvest)
  # }
  #
  #
  # np_calc_exposure <- function(np_harvest, hab_extent, FIS_status) {
  #   ### calculates NP exposure based on habitats (for corals, seaweeds,
  #   ### ornamentals, shells, sponges).
  #   ### Returns the first input data frame with a new column for exposure:
  #   ### [rgn_id rgn_name product year tonnes tonnes_rel prod_weight exposure]
  #   #########################################.
  #
  #   ### Determine Habitat Areas for Exposure
  #   ### extract habitats used
  #   hab_coral <- hab_extent %>%
  #     filter(habitat == 'coral') %>%
  #     select(rgn_id, km2)
  #   hab_rocky   <- hab_extent %>%
  #     filter(habitat == 'rocky_reef') %>%
  #     select(rgn_id, km2)
  #
  #   ### area for products having single habitats for exposure
  #   area_single_hab <- bind_rows(
  #     # corals in coral reef
  #     np_harvest %>%
  #       filter(product == 'corals') %>%
  #       left_join(
  #         hab_coral %>%
  #           filter(km2 > 0) %>%
  #           select(rgn_id, km2), by = 'rgn_id'),
  #     ### seaweeds in rocky reef
  #     np_harvest %>%
  #       filter(product == 'seaweeds') %>%
  #       left_join(
  #         hab_rocky %>%
  #           filter(km2 > 0) %>%
  #           select(rgn_id, km2), by = 'rgn_id'))
  #
  #   ### area for products in both coral and rocky reef habitats: shells, ornamentals, sponges
  #   area_dual_hab <- np_harvest %>%
  #     filter(product %in% c('shells', 'ornamentals','sponges')) %>%
  #     left_join(
  #       hab_coral %>%
  #         filter(km2 > 0) %>%
  #         select(rgn_id, coral_km2 = km2),
  #       by = 'rgn_id') %>%
  #     left_join(
  #       hab_rocky %>%
  #         filter(km2 > 0) %>%
  #         select(rgn_id, rocky_km2 = km2),
  #       by = 'rgn_id') %>%
  #     rowwise() %>%
  #     mutate(
  #       km2 = sum(c(rocky_km2, coral_km2), na.rm = TRUE)) %>%
  #     filter(km2 > 0)
  #
  #   ### Determine Exposure
  #   ### exposure: combine areas, get tonnes / area, and rescale with log transform
  #   np_exp <-
  #     bind_rows(
  #       area_single_hab,
  #       area_dual_hab %>%
  #         select(-rocky_km2, -coral_km2)) %>%
  #     mutate(
  #       expos_raw = ifelse(tonnes > 0 & km2 > 0, (tonnes / km2), 0)) %>%
  #     group_by(product) %>%
  #     mutate(
  #       expos_prod_max = (1 - .35)*max(expos_raw, na.rm = TRUE)) %>%
  #     ungroup() %>%
  #     mutate(
  #       exposure = (log(expos_raw + 1) / log(expos_prod_max + 1)),
  #       exposure = ifelse(exposure > 1, 1, exposure)) %>%
  #     select(-km2, -expos_raw, -expos_prod_max)
  #   ### clean up columns
  #
  #   gap_fill <- np_exp %>%
  #     mutate(gap_fill = ifelse(is.na(exposure), "prod_average", 0)) %>%
  #     select(rgn_id, product, year, gap_fill)
  #   write.csv(gap_fill, 'temp/NP_exposure_gapfill.csv', row.names=FALSE)
  #
  #   ### add exposure for countries with (habitat extent == NA)
  #   np_exp <- np_exp %>%
  #     group_by(product) %>%
  #     mutate(mean_exp = mean(exposure, na.rm = TRUE)) %>%
  #     mutate(exposure = ifelse(is.na(exposure), mean_exp, exposure)) %>%
  #     select(-mean_exp) %>%
  #     ungroup() %>%
  #     mutate(product = as.character(product))
  #
  #
  #   return(np_exp)
  # }
  #
  # np_calc_risk <- function(np_exp, r_cyanide, r_blast) {
  #   ### calculates NP risk based on:
  #   ###   ornamentals:      risk = 1 if blast or cyanide fishing
  #   ###   corals:           risk = 1 for all cases
  #   ###   shells, sponges:  risk = 0 for all cases
  #   ###   others:           risk = NA?
  #   ### Returns a data frame of risk, by product, by region:
  #   ###
  #   #########################################.
  #
  #   ### Determine Risk
  #
  #   ### risk for ornamentals set to 1 if blast or cyanide fishing present, based on Nature 2012 code
  #   ###  despite Nature 2012 Suppl saying Risk for ornamental fish is set to the "relative intensity of cyanide fishing"
  #   risk_orn <- r_cyanide %>%
  #     filter(!is.na(score) & score > 0) %>%
  #     select(rgn_id, cyanide = score) %>%
  #     merge(
  #       r_blast %>%
  #         filter(!is.na(score) & score > 0) %>%
  #         select(rgn_id, blast = score),
  #       all = TRUE) %>%
  #     mutate(ornamentals = 1)
  #
  #   ### risk as binary
  #   np_risk <-
  #     ### fixed risk: corals (1), sponges (0) and shells (0)
  #     data.frame(
  #       rgn_id  = unique(np_harvest$rgn_id),
  #       corals  = 1,
  #       sponges = 0,
  #       shells  = 0) %>%
  #     ### ornamentals
  #     left_join(
  #       risk_orn %>%
  #         select(rgn_id, ornamentals),
  #       by = 'rgn_id')  %>%
  #     mutate(
  #       ornamentals = ifelse(is.na(ornamentals), 0, ornamentals)) %>%
  #     gather(product, risk, -rgn_id) %>%
  #     mutate(product = as.character(product))
  #   return(np_risk)
  # }
  #
  # np_calc_sustainability <- function(np_exp, np_risk) {
  #   ### calculates NP sustainability coefficient for each natural product, based
  #   ### on (1 - mean(c(exposure, risk))).  Returns first input dataframe with
  #   ### new columns for sustainability coefficient, and sustainability-adjusted
  #   ### NP product_status:
  #   ### [rgn_id  rgn_name  product  year  prod_weight  sustainability  product_status]
  #   #########################################.
  #
  #   ### join Exposure (with harvest) and Risk
  #   np_sust <- np_exp %>%
  #     left_join(
  #       np_risk,
  #       by = c('rgn_id', 'product')) %>%
  #     rowwise() %>%
  #     mutate(sustainability = 1 - mean(c(exposure, risk), na.rm = TRUE))
  #
  #   ### add in fish_oil sustainability based on FIS scores calculated above:
  #   ### add fish_oil (no exposure calculated, sustainability is based on FIS score only, and not exposure/risk components)
  #   fish_oil_sust <-   FIS_status %>%
  #     mutate(sustainability = score / 100) %>%
  #     mutate(sustainability = ifelse(is.na(sustainability), 0, sustainability)) %>%
  #     select(rgn_id, sustainability)
  #
  #   np_sus_fis_oil <- np_harvest %>%
  #     filter(product=='fish_oil') %>%
  #     mutate(exposure = NA) %>%
  #     mutate(risk = NA) %>%
  #     left_join(fish_oil_sust, by='rgn_id')
  #
  #   np_exp <- np_sust %>%
  #     bind_rows(np_sus_fis_oil)
  #
  #
  #   ### calculate rgn-product-year status
  #   np_sust <- np_sust %>%
  #     mutate(product_status = tonnes_rel * sustainability) %>%
  #     filter(rgn_name != 'DISPUTED') %>%
  #     select(-tonnes, -tonnes_rel, -risk, -exposure) %>%
  #     ungroup()
  #
  #   return(np_sust)
  # }
  #
  # np_calc_scores <- function(np_sust, status_year) {
  #   ### Calculates NP status for all production years for each region, based
  #   ### upon weighted mean of all products produced.
  #   ### From this, reports the most recent year as the NP status.
  #   ### Calculates NP trend for each region, based upon slope of a linear
  #   ### model over the past six years inclusive (five one-year intervals).
  #   ### Returns data frame with status and trend by region:
  #   ### [goal   dimension   region_id   score]
  #   #########################################.
  #
  #   ### Calculate status, trends
  #   ### aggregate across products to rgn-year status, weighting by usd_rel
  #   np_status_all <- np_sust %>%
  #     filter(!is.na(product_status) & !is.na(prod_weight)) %>%
  #     ### ??? CCO: guadeloupe & martinique have NA for ornamental prod_weight.  Is this a gap-filling error?
  #     select(rgn_name, rgn_id, year, product, product_status, prod_weight) %>%
  #     group_by(rgn_id, year) %>%
  #     summarize(status = weighted.mean(product_status, prod_weight)) %>%
  #     filter(!is.na(status)) %>% # 1/0 produces NaN
  #     ungroup()
  #
  #   ### get current status
  #   np_status_current <- np_status_all %>%
  #     filter(year == status_year & !is.na(status)) %>%
  #     mutate(
  #       dimension = 'status',
  #       score     = round(status,4) * 100) %>%
  #     select(rgn_id, dimension, score)
  #   stopifnot(
  #     min(np_status_current$score, na.rm = TRUE) >= 0,
  #     max(np_status_current$score, na.rm = TRUE) <= 100)
  #
  #   ### trend
  #   np_trend <- np_status_all %>%
  #     filter(year <= status_year & year > (status_year - 5) & !is.na(status)) %>%
  #     group_by(rgn_id) %>%
  #     do(mdl = lm(status ~ year, data=.)) %>%
  #     summarize(
  #       rgn_id    = rgn_id,
  #       dimension = 'trend',
  #       score     = max(-1, min(1, coef(mdl)[['year']] * 5)))
  #   stopifnot(min(np_trend$score) >= -1, max(np_trend$score) <= 1)


  #   ### return scores
  #   np_scores <- np_status_current %>%
  #     full_join(np_trend, by=c('rgn_id', 'dimension', 'score')) %>%
  #     mutate(goal = 'NP') %>%
  #     select(goal, dimension, region_id=rgn_id, score) %>%
  #     arrange(goal, dimension, region_id)
  #
  #   return(np_scores)
  # }

  # ##########################################.
  # ### Natural Products main starts here:
  #
  # np_harvest <- np_rebuild_harvest(layers)
  # np_exp     <- np_calc_exposure(np_harvest, hab_extent, FIS_status)
  # np_risk    <- np_calc_risk(np_exp, r_cyanide, r_blast)
  # np_sust    <- np_calc_sustainability(np_exp, np_risk)
  # np_scores  <- np_calc_scores(np_sust, status_year)
  #
  # ## reference points
  # rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
  #   rbind(data.frame(goal = "NP", method = "Harvest peak within region times 0.65 buffer",
#   #                    reference_point = "varies for each region"))
#   # write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)
#
#
#   return(np_scores)
# }


CS <- function(layers){

  ## read in layers
  extent <- layers$data[['hab_extent']] %>%
    select(rgn_id, habitat, km2) %>%
    mutate(habitat = as.character(habitat))

  health <-  layers$data[['hab_health']] %>%
    select(rgn_id, habitat, health) %>%
    mutate(habitat = as.character(habitat))

  trend <-layers$data[['hab_trend']] %>%
    select(rgn_id, habitat, trend) %>%
    mutate(habitat = as.character(habitat))

  ## join layer data
  d <-  extent %>%
    full_join(health, by=c("rgn_id", "habitat")) %>%
    full_join(trend, by=c("rgn_id", "habitat"))

  ## set ranks for each habitat
  habitat.rank <- c('mangrove'         = 139,
                    'saltmarsh'        = 210,
                    'seagrass'         = 83)

  ## limit to CS habitats and add rank
  d <- d %>%
    filter(habitat %in% names(habitat.rank)) %>%
    mutate(
      rank = habitat.rank[habitat],
      extent = ifelse(km2==0, NA, km2))

  ## output file to temp folder that describes how much each habitat
  ## contributes to the score based on rank and extent
  ## this output is for the dataplayground website
  dp <- d %>%
    mutate(weighted_cont = rank*extent) %>%
    filter(!is.na(weighted_cont)) %>%
    group_by(rgn_id) %>%
    mutate(prop_score = weighted_cont/sum(weighted_cont)) %>%
    mutate(prop_score = round(prop_score, 3)) %>%
    select(rgn_id, habitat, prop_score)
  write.csv(dp, 'temp/cs_hab_contributions.csv', row.names=FALSE)

  ## status and trend models; ensure at least one habitat-region has extent (km2) > 0, otherwise set NA.
  if (sum(d$km2, na.rm=TRUE) > 0){
    # status
    scores_CS <- d %>%
      filter(!is.na(rank) & !is.na(health) & !is.na(extent)) %>%
      group_by(rgn_id) %>%
      summarize(
        score = pmin(1, sum(rank * health * extent, na.rm=TRUE) / (sum(extent * rank, na.rm=TRUE)) ) * 100,
        dimension = 'status') %>%
      ungroup()

    # trend
    d_trend <- d %>%
      filter(!is.na(rank) & !is.na(trend) & !is.na(extent))
    if (nrow(d_trend) > 0 ){
      scores_CS <- dplyr::bind_rows(
        scores_CS,
        d_trend %>%
          group_by(rgn_id) %>%
          summarize(
            score = sum(rank * trend * extent, na.rm=TRUE) / (sum(extent*rank, na.rm=TRUE)),
            dimension = 'trend')) %>%
        ungroup()
    } else { # if no trend score, assign NA
      scores_CS <- dplyr::bind_rows(
        scores_CS,
        d %>%
          group_by(rgn_id) %>%
          summarize(
            score = NA,
            dimension = 'trend'))
    }

    ### output data file for checking and data review
    scores_check <- spread(scores_CS, dimension, score) %>%
      select(rgn_id, status, trend_score=trend)

    d_check <- d %>%
      select(rgn_id, habitat, extent, health, trend, rank) %>%
      arrange(rgn_id, habitat) %>%
      left_join(scores_check, by="rgn_id")

    ### end: output...

    scores_CS <- scores_CS %>%
      mutate(
        goal = 'CS') %>%
      select(region_id=rgn_id, goal, dimension, score)

  } else { ## else -- if sum(d$km2) is not greater than 0

     ## set status and trend to NA for all regions
      message('CS status and trend are NA, consider removing goal if no CS habitats in assessment area')

      rgns <-layers$data[['rgn_labels']]
      scores_CS <- bind_rows(
        rgns %>%
        mutate(goal      = 'CS',
               dimension = 'status',
               score     = NA),
      rgns %>%
        mutate(goal      = 'CS',
               dimension = 'trend',
               score     = NA)) %>%
        select(goal, dimension, region_id = rgn_id, score)

  } ## end -- if (sum(d$km2) > 0)

  ## reference points
  rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "CS", method = "Health/condition variable based on current vs. historic extent",
                     reference_point = "varies for each region/habitat"))
  write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)


  # return scores
  return(scores_CS)
}



CP <- function(layers){

  ## read in layers
  extent <- layers$data[['hab_extent']] %>%
    select(rgn_id, habitat, km2) %>%
    mutate(habitat = as.character(habitat))

  health <-  layers$data[['hab_health']] %>%
    select(rgn_id, habitat, health) %>%
    mutate(habitat = as.character(habitat))

  trend <-layers$data[['hab_trend']] %>%
    select(rgn_id, habitat, trend) %>%
    mutate(habitat = as.character(habitat))


  ## sum mangrove_offshore + mangrove_inland1km = mangrove to match with extent and trend
  mangrove_extent <- extent %>%
    filter(habitat %in% c('mangrove_inland1km','mangrove_offshore'))

  if (nrow(mangrove_extent) > 0){
    mangrove_extent <- mangrove_extent %>%
      group_by(rgn_id) %>%
      summarize(km2 = sum(km2, na.rm = TRUE)) %>%
      mutate(habitat='mangrove') %>%
      ungroup()
  }

  extent <- extent %>%
    filter(!habitat %in% c('mangrove','mangrove_inland1km','mangrove_offshore')) %>%  #do not use all mangrove
    rbind(mangrove_extent)  #just the inland 1km and offshore

  ## join layer data
  d <-  extent %>%
    full_join(health, by=c("rgn_id", "habitat")) %>%
    full_join(trend, by=c("rgn_id", "habitat"))

  ## set ranks for each habitat
  habitat.rank <- c('coral'            = 4,
                    'mangrove'         = 4,
                    'saltmarsh'        = 3,
                    'seagrass'         = 1,
                    'seaice_shoreline' = 4)

  ## limit to CP habitats and add rank
  d <- d %>%
    filter(habitat %in% names(habitat.rank)) %>%
    mutate(
      rank = habitat.rank[habitat],
      extent = ifelse(km2==0, NA, km2))

  ## output file to temp folder that describes how much each habitat
  ## contributes to the score based on rank and extent
  ## this output is for the dataplayground website
  dp <- d %>%
    mutate(weighted_cont = rank*extent) %>%
    filter(!is.na(weighted_cont)) %>%
    group_by(rgn_id) %>%
    mutate(prop_score = weighted_cont/sum(weighted_cont)) %>%
    mutate(prop_score = round(prop_score, 3)) %>%
    select(rgn_id, habitat, prop_score)
  write.csv(dp, 'temp/cp_hab_contributions.csv', row.names=FALSE)

  ## status and trend models; ensure at least one habitat-region has extent (km2) > 0, otherwise set NA.
  if (sum(d$km2, na.rm=TRUE) > 0){
    # status
    scores_CP <- d %>%
      filter(!is.na(rank) & !is.na(health) & !is.na(extent)) %>%
      group_by(rgn_id) %>%
      summarize(score = pmin(1, sum(rank * health * extent, na.rm=TRUE) /
                               (sum(extent * rank, na.rm=TRUE)) ) * 100) %>%
      mutate(dimension = 'status') %>%
      ungroup()

    # trend
    d_trend <- d %>%
      filter(!is.na(rank) & !is.na(trend) & !is.na(extent))

    if (nrow(d_trend) > 0 ){
      scores_CP <- dplyr::bind_rows(
        scores_CP,
        d_trend %>%
          group_by(rgn_id) %>%
          summarize(
            score = sum(rank * trend * extent, na.rm=TRUE) / (sum(extent*rank, na.rm=TRUE)),
            dimension = 'trend'))
    } else { # if no trend score, assign NA
      scores_CP <- dplyr::bind_rows(
        scores_CP,
        d %>%
          group_by(rgn_id) %>%
          summarize(
            score = NA,
            dimension = 'trend'))
    }

    ### output data file for checking and data review
    scores_check <- spread(scores_CP, dimension, score) %>%
      select(rgn_id, status, trend_score=trend)

    d_check <- d %>%
      select(rgn_id, habitat, extent, health, trend, rank) %>%
      arrange(rgn_id, habitat) %>%
      left_join(scores_check, by="rgn_id")


    ## finalize scores_CP
    scores_CP <- scores_CP %>%
      mutate(
        goal = 'CP') %>%
      select(region_id=rgn_id, goal, dimension, score)

  } else { ## else -- if sum(d$km2) is not greater than 0

    ## set status and trend to NA for all regions
    message('CP status and trend are NA, consider removing goal if no CP habitats in assessment area')

    rgns <-layers$data[['rgn_labels']]
    scores_CP <- bind_rows(
      rgns %>%
        mutate(goal      = 'CP',
               dimension = 'status',
               score     = NA),
      rgns %>%
        mutate(goal      = 'CP',
               dimension = 'trend',
               score     = NA)) %>%
      select(goal, dimension, region_id = rgn_id, score)

  } ## end -- if (sum(d$km2) > 0)

  ## reference points
  rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "CP", method = "Health/condition variable based on current vs. historic extent",
                     reference_point = "varies for each region/habitat"))
  write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)


  # return scores
  return(scores_CP)

}


TR = function(layers, status_year, pct_ref = 90) {

   ## formula:
  ##  E   = Ep                         # Ep: % of direct tourism jobs. tr_jobs_pct_tourism.csv
  ##  S   = (S_score - 1) / (7 - 1)    # S_score: raw TTCI score, not normalized (1-7). tr_sustainability.csv
  ##  Xtr = E * S

  ## read in layers
  tr_data  <- full_join(
    layers$data[['tr_jobs_pct_tourism']] %>%
      select(-layer),
    layers$data[['tr_sustainability']] %>%
      select(-layer),
    by = c('rgn_id'))%>%
    filter(year <= status_year)

  tr_model <- tr_data %>%
    mutate(
      E   = Ep,
      S   = (S_score - 1) / (7 - 1), # scale score from 1 to 7.
      Xtr = E * S ) %>%
    filter(year <= status_year & year > status_year - 5)
  # five data years for trend calcs

  # regions with Travel Warnings
  ### adjust the travel warning years...these always reflect the current year
  ### but the other datasets will lag
  if (exists('scenarios')) { ## if global scenarios
    scenario_year <- as.numeric(substring(scenario, 4,7))
    offset_years <- scenario_year - status_year

    ## read in layers for regions with Travel Warnings
    rgn_travel_warnings <- layers$data[['tr_travelwarnings']] %>%
      select(rgn_id, year, multiplier) %>%
      mutate(year = year - offset_years)

    ## incorporate Travel Warnings
    tr_model <- tr_model %>%
      left_join(rgn_travel_warnings, by = c('rgn_id', 'year')) %>%
      mutate(Xtr = ifelse(!is.na(multiplier), multiplier * Xtr, Xtr)) %>%
      select(-multiplier)

  } ## end if (exists('scenarios'))

  ### Calculate status based on quantile reference (see function call for pct_ref)
  tr_model <- tr_model %>%
    select(rgn_id, year, Xtr) %>%
    left_join(tr_model %>%
                group_by(year) %>%
                summarize(Xtr_q = quantile(Xtr, probs = pct_ref/100, na.rm = TRUE)),
              by = 'year') %>%
    mutate(
      Xtr_rq  = ifelse(Xtr / Xtr_q > 1, 1, Xtr / Xtr_q)) # rescale to qth percentile, cap at 1

  ## reference points
  ref_point <- tr_model %>%
    filter(year == status_year) %>%
    select(Xtr_q) %>%
    unique()
  rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "TR", method = paste0('spatial: ', pct_ref, "th quantile"),
                     reference_point = ref_point$Xtr_q))
  write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)



  # calculate trend
  tr_trend <- tr_model %>%
    filter(!is.na(Xtr_rq)) %>%
    arrange(year, rgn_id) %>%
    group_by(rgn_id) %>%
    do(mod = lm(Xtr_rq ~ year, data = .)) %>%
    do(data.frame(
      rgn_id    = .$rgn_id,
      dimension = 'trend',
      score     =  max(min(coef(.$mod)[['year']] * 5, 1), -1)))

  # get status (as last year's value)
  tr_status <- tr_model %>%
    arrange(year, rgn_id) %>%
    group_by(rgn_id) %>%
    summarize(
      dimension = 'status',
      score     = last(Xtr_rq) * 100)

  # bind status and trend by rows
  tr_score <- bind_rows(tr_status, tr_trend) %>%
    mutate(goal = 'TR')

  if (conf$config$layer_region_labels=='rgn_global'){
    # assign NA for uninhabitated islands
    unpopulated = layers$data[['le_popn']] %>%
      group_by(rgn_id) %>%
      filter(count==0) %>%
      select(rgn_id)
    tr_score$score = ifelse(tr_score$rgn_id %in% unpopulated$rgn_id, NA, tr_score$score)
  }

  # return final scores
  scores = tr_score %>%
    select(region_id=rgn_id, goal, dimension, score)

  return(scores)
}

LIV_ECO = function(layers, subgoal){

  ## read in all data: gdp, wages, jobs and workforce_size data
  le_gdp   = SelectLayersData(layers, layers='le_gdp')  %>%
    dplyr::select(rgn_id = id_num, year, gdp_usd = val_num)

  le_wages = SelectLayersData(layers, layers='le_wage_sector_year') %>%
    dplyr::select(rgn_id = id_num, year, sector = category, wage_usd = val_num)

  le_jobs  = SelectLayersData(layers, layers='le_jobs_sector_year') %>%
    dplyr::select(rgn_id = id_num, year, sector = category, jobs = val_num)

  le_workforce_size = SelectLayersData(layers, layers='le_workforcesize_adj') %>%
    dplyr::select(rgn_id = id_num, year, jobs_all = val_num)

  le_unemployment = SelectLayersData(layers, layers='le_unemployment') %>%
    dplyr::select(rgn_id = id_num, year, pct_unemployed = val_num)


  # multipliers from Table S10 (Halpern et al 2012 SOM)
  multipliers_jobs = data.frame('sector' = c('tour','cf', 'mmw', 'wte','mar'),
                                'multiplier' = c(1, 1.582, 1.915, 1.88, 2.7)) # no multiplers for tour (=1)
  # multipliers_rev  = data.frame('sector' = c('mar', 'tour'), 'multiplier' = c(1.59, 1)) # not used because GDP data is not by sector


  # calculate employment counts
  le_employed = le_workforce_size %>%
    left_join(le_unemployment, by = c('rgn_id', 'year')) %>%
    mutate(proportion_employed = (100 - pct_unemployed) / 100,
           employed            = jobs_all * proportion_employed)

  # reworded from SOM p.26-27
  #reference point for wages is the reference region (r) with the highest average wages across all sectors.
  #Reference points for jobs (j) and revenue (e) employ a moving baseline. The two metrics (j, e) are calculated
  #as relative values: the value in the current year (or most recent year), c, relative to the value in a recent
  #moving reference period, r, defined as 5 years prior to c. This reflects an implicit goal of maintaining coastal
  #livelihoods and economies (L&E) on short time scales, allowing for decadal or generational shifts in what people
  #want and expect for coastal L&E. The most recent year c must be 2000 or later in order for the data to be included.

  liv =
    # adjust jobs
    le_jobs %>%
    left_join(multipliers_jobs, by = 'sector') %>%
    mutate(jobs_mult = jobs * multiplier) %>%  # adjust jobs by multipliers
    left_join(le_employed, by= c('rgn_id', 'year')) %>%
    mutate(jobs_adj = jobs_mult * proportion_employed) %>% # adjust jobs by proportion employed
    left_join(le_wages, by=c('rgn_id','year','sector')) %>%
    arrange(year, sector, rgn_id)

  # LIV calculations ----

  # LIV status
  liv_status = liv %>%
    filter(!is.na(jobs_adj) & !is.na(wage_usd))
  # aia/subcountry2014 crashing b/c no concurrent wage data, so adding this check
  if (nrow(liv_status)==0){
    liv_status = liv %>%
      dplyr::select(region_id=rgn_id) %>%
      group_by(region_id) %>%
      summarize(
        goal      = 'LIV',
        dimension = 'status',
        score     = NA)
    liv_trend = liv %>%
      dplyr::select(region_id=rgn_id) %>%
      group_by(region_id) %>%
      summarize(
        goal      = 'LIV',
        dimension = 'trend',
        score     = NA)
  } else {
    liv_status = liv_status %>%
      filter(year >= max(year, na.rm=T) - 4) %>% # reference point is 5 years ago
      arrange(rgn_id, year, sector) %>%
      # summarize across sectors
      group_by(rgn_id, year) %>%
      summarize(
        # across sectors, jobs are summed
        jobs_sum  = sum(jobs_adj, na.rm=T),
        # across sectors, wages are averaged
        wages_avg = mean(wage_usd, na.rm=T)) %>%
      group_by(rgn_id) %>%
      arrange(rgn_id, year) %>%
      mutate(
        # reference for jobs [j]: value in the current year (or most recent year) [c], relative to the value in a recent moving reference period [r] defined as 5 years prior to [c]
        jobs_sum_first  = first(jobs_sum),                     # note:  `first(jobs_sum, order_by=year)` caused segfault crash on Linux with dplyr 0.3.0.2, so using arrange above instead
        # original reference for wages [w]: target value for average annual wages is the highest value observed across all reporting units
        # new reference for wages [w]: value in the current year (or most recent year) [c], relative to the value in a recent moving reference period [r] defined as 5 years prior to [c]
        wages_avg_first = first(wages_avg)) %>% # note:  `first(jobs_sum, order_by=year)` caused segfault crash on Linux with dplyr 0.3.0.2, so using arrange above instead
      # calculate final scores
      ungroup() %>%
      mutate(
        x_jobs  = pmax(-1, pmin(1,  jobs_sum / jobs_sum_first)),
        x_wages = pmax(-1, pmin(1, wages_avg / wages_avg_first)),
        score   = mean(c(x_jobs, x_wages), na.rm=T) * 100) %>%
      # filter for most recent year
      filter(year == max(year, na.rm=T)) %>%
      # format
      dplyr::select(
        region_id = rgn_id,
        score) %>%
      mutate(
        goal      = 'LIV',
        dimension = 'status')

    # LIV trend
    # From SOM p. 29: trend was calculated as the slope in the individual sector values (not summed sectors)
    # over the most recent five years...
    # with the average weighted by the number of jobs in each sector
    # ... averaging slopes across sectors weighted by the revenue in each sector

    # get trend across years as slope of individual sectors for jobs and wages
    liv_trend = liv %>%
      filter(!is.na(jobs_adj) & !is.na(wage_usd)) %>%
      # TODO: consider "5 year time spans" as having 5 [(max(year)-4):max(year)] or 6 [(max(year)-5):max(year)] member years
      filter(year >= max(year, na.rm=T) - 4) %>% # reference point is 5 years ago
      # get sector weight as total jobs across years for given region
      arrange(rgn_id, year, sector) %>%
      group_by(rgn_id, sector) %>%
      mutate(
        weight = sum(jobs_adj, na.rm=T)) %>%
      # reshape into jobs and wages columns into single metric to get slope of both with one do() call
      reshape2::melt(id=c('rgn_id','year','sector','weight'), variable='metric', value.name='value') %>%
      mutate(
        sector = as.character(sector),
        metric = as.character(metric)) %>%
      # get linear model coefficient per metric
      group_by(metric, rgn_id, sector, weight) %>%
      do(mdl = lm(value ~ year, data=.)) %>%
      summarize(
        metric = metric,
        weight = weight,
        rgn_id = rgn_id,
        sector = sector,
        # TODO: consider how the units affect trend; should these be normalized? cap per sector or later?
        sector_trend = pmax(-1, pmin(1, coef(mdl)[['year']] * 5))) %>%
      arrange(rgn_id, metric, sector) %>%
      # get weighted mean across sectors per region-metric
      group_by(metric, rgn_id) %>%
      summarize(
        metric_trend = weighted.mean(sector_trend, weight, na.rm=T)) %>%
      # get mean trend across metrics (jobs, wages) per region
      group_by(rgn_id) %>%
      summarize(
        score = mean(metric_trend, na.rm=T)) %>%
      # format
      mutate(
        goal      = 'LIV',
        dimension = 'trend') %>%
      dplyr::select(
        goal, dimension,
        region_id = rgn_id,
        score)
  }


  # ECO calculations ----
  eco = le_gdp %>%
    mutate(
      rev_adj = gdp_usd,
      sector = 'gdp') %>%
    # adjust rev with national GDP rates if available. Example: (rev_adj = gdp_usd / ntl_gdp)
    dplyr::select(rgn_id, year, sector, rev_adj)

  # ECO status
  eco_status = eco %>%
    filter(!is.na(rev_adj)) %>%
    filter(year >= max(year, na.rm=T) - 4) %>% # reference point is 5 years ago
    # across sectors, revenue is summed
    group_by(rgn_id, year) %>%
    summarize(
      rev_sum  = sum(rev_adj, na.rm=T)) %>%
    # reference for revenue [e]: value in the current year (or most recent year) [c], relative to the value in a recent moving reference period [r] defined as 5 years prior to [c]
    arrange(rgn_id, year) %>%
    group_by(rgn_id) %>%
    mutate(
      rev_sum_first  = first(rev_sum)) %>%
    # calculate final scores
    ungroup() %>%
    mutate(
      score  = pmin(rev_sum / rev_sum_first, 1) * 100) %>%
    # get most recent year
    filter(year == max(year, na.rm=T)) %>%
    # format
    mutate(
      goal      = 'ECO',
      dimension = 'status') %>%
    dplyr::select(
      goal, dimension,
      region_id = rgn_id,
      score)

  # ECO trend
  eco_trend = eco %>%
    filter(!is.na(rev_adj)) %>%
    filter(year >= max(year, na.rm=T) - 4 ) %>% # 5 year trend
    # get sector weight as total revenue across years for given region
    arrange(rgn_id, year, sector) %>%
    group_by(rgn_id, sector) %>%
    mutate(
      weight = sum(rev_adj, na.rm=T)) %>%
    # get linear model coefficient per region-sector
    group_by(rgn_id, sector, weight) %>%
    do(mdl = lm(rev_adj ~ year, data=.)) %>%
    summarize(
      weight = weight,
      rgn_id = rgn_id,
      sector = sector,
      # TODO: consider how the units affect trend; should these be normalized? cap per sector or later?
      sector_trend = pmax(-1, pmin(1, coef(mdl)[['year']] * 5))) %>%
    # get weighted mean across sectors per region
    group_by(rgn_id) %>%
    summarize(
      score = weighted.mean(sector_trend, weight, na.rm=T)) %>%
    # format
    mutate(
      goal      = 'ECO',
      dimension = 'trend') %>%
    dplyr::select(
      goal, dimension,
      region_id = rgn_id,
      score)

  # report LIV and ECO scores separately
  if (subgoal=='LIV'){
    d = rbind(liv_status, liv_trend)
  } else if (subgoal=='ECO'){
    d = rbind(eco_status, eco_trend)
  } else {
    stop('LIV_ECO function only handles subgoal of "LIV" or "ECO"')
  }
  return(d)

}


LE = function(scores, layers){

  # calculate LE scores
  scores.LE = scores %>%
    dplyr::filter(goal %in% c('LIV','ECO') & dimension %in% c('status','trend','score','future')) %>%
    tidyr::spread(key = goal, value = score) %>%
    dplyr::mutate(score = rowMeans(cbind(ECO, LIV), na.rm=T)) %>%
    dplyr::select(region_id, dimension, score) %>%
    dplyr::mutate(goal  = 'LE')

  # rbind to all scores
  scores = scores %>%
    rbind(scores.LE)

  # return scores
  return(scores)
}


ICO = function(layers){

  ico_data <- SelectLayersData(layers, layers = 'ico_iucn_status'); head(ico_data)
  ico_data <- select(ico_data, species = category, year, risk.wt = val_num)

  ico_status <- ico_data %>%
    group_by(species) %>%
    filter(year == max(year)) %>%
    ungroup %>%
    summarize(score = (1 - mean(risk.wt)) * 100) %>%
    mutate(dimension = 'status',
           rgn_id = 1) %>%
    select(rgn_id, score, dimension)

  # Trend calculations
  ## find species with multiple assessments

  dup_species <- ico_data$species[duplicated(ico_data[,1])]

  ico_trend <- ico_data %>%
    filter(species %in% dup_species) %>%
    group_by(species) %>%
    filter(year == max(year) | year == min(year),
           !(species == "minor" & year == 2014 & risk.wt == "0"),
           !(species == "cylindrica")) %>%
    arrange(year) %>%
    mutate(year = ifelse(year == max(year), "max_year", "min_year")) %>%
    ungroup() %>%
    group_by(species, year) %>%
    summarise(risk.wt.new = mean(risk.wt)) %>%
    spread(year, risk.wt.new) %>%
    summarize(trend_species = ifelse(max_year > min_year, -0.5,
                                     ifelse(max_year < min_year, 0.5,
                                            0))) %>%
    summarize(score = mean(trend_species)) %>%
    mutate(rgn_id = 1,
           dimension = 'trend') %>%
    select(rgn_id, score, dimension) %>%
    ungroup()

  # # lookup for weights status
  # #  LC <- "LOWER RISK/LEAST CONCERN (LR/LC)"
  # #  NT <- "LOWER RISK/NEAR THREATENED (LR/NT)"
  # #  T  <- "THREATENED (T)" treat as "EN"
  # #  VU <- "VULNERABLE (V)"
  # #  EN <- "ENDANGERED (E)"
  # #  LR/CD <- "LOWER RISK/CONSERVATION DEPENDENT (LR/CD)" treat as between VU and NT
  # #  CR <- "VERY RARE AND BELIEVED TO BE DECREASING IN NUMBERS"
  # #  DD <- "INSUFFICIENTLY KNOWN (K)"
  # #  DD <- "INDETERMINATE (I)"
  # #  DD <- "STATUS INADEQUATELY KNOWN-SURVEY REQUIRED OR DATA SOUGHT"

  # return scores
  scores_ICO <- rbind(ico_status, ico_trend) %>%
    mutate(goal = "ICO",
           rgn_id = as.integer(rgn_id)) %>%
    select(region_id = rgn_id, goal, dimension, score)

  # return scores
  return(scores_ICO)

}

LSP = function(layers){

  #select data ----
    lsp_data <- SelectLayersData(layers, layers='lsp_iucn_mngmt'); head(lsp_data)  #total offshore/EEZ areas
    lsp_data <- select(lsp_data, rgn_id = id_num, year, km2 = category, iucn_cat, prot_values, management, mngmt_values, rel_protection = val_num, km2_cum)

    #calculations of status with temporal gapfilling and cummulative protection areas
    lsp_final_data <- lsp_data %>%
      ##protected areas 0.216, 0.226, 0.191, and 0.270 are within 1067.097
      group_by(year, prot_values, mngmt_values) %>%
      summarize(tot_area = sum(km2)) %>%
      ungroup() %>%
      mutate(tot_area = ifelse(tot_area == 1067.097, 1067.097-0.442-0.461, tot_area)) %>%
      # all these are within 1288864.900
      mutate(tot_area = ifelse(tot_area == 1288864.900, 1288864.900-1066.194, tot_area)) %>%
      mutate(rel_protection = tot_area * prot_values * mngmt_values)

    lsp_status_allyear <- lsp_final_data %>%
      group_by(year) %>% # calculate rel_prot_total by year
      summarise(rel_protection_total = sum(rel_protection)) %>%
      ungroup() %>%
      arrange(year) %>%
      mutate(cum_rel_prot = cumsum(rel_protection_total)) %>%
      complete(year = 2008:2016) %>%
      fill(cum_rel_prot) %>%
      ## Reference point: Reaching 70% of EZZ as Cat IV, 30% of entire EEZ as Cat I (working group)
      ### use this model for the final version
      mutate(ref_point = (((1287798.706 * .7) * 0.4 * 1) + ((1287798.706 * .3) * 1 * 1)),
             status = round((cum_rel_prot / ref_point) * 100, 2)) %>%
      select(year, score = status)

    #proposing different reference points
    ##MANAGEMENT: all versions are at Management Plan Implemented = 1

    # ## v1: Reference point is 1288864.900 minus the remaining protected areas on the list (total cumulative protection in 2016 = 1287798.706) at IUNC category VI (score = 0.1)
    # mutate(ref_point_1 = 1287798.706 * 0.1 * 1,
    #        status_1 = round((cum_rel_prot / ref_point_1) * 100, 2)) %>%
    # ## v2: Reaching 60% of EEZ Protected as Cat II (KBA) = 0.8 & 40% of EEZ Protected as Category VI
    # mutate(ref_point_2 = (((1287798.706 * .6) * 0.8 * 1) + (1287798.706 * .4) * 0.1 * 1),
    #        status_2 = round((cum_rel_prot / ref_point_2) * 100, 2))

    lsp_status <- lsp_status_allyear %>%
      filter(year == max(year)) %>%
      mutate(rgn_id = 1,
             dimension = 'status') %>%
      select(rgn_id, score, dimension)

    # calculate trend
    lsp_trend <- lsp_status_allyear %>%
      filter(year %in% 2012:2016) %>%
      do(mod = lm(score ~ year, data =.)) %>%
      mutate(rgn_id = "1",
             score     =  max(min(coef(mod)[['year']] * 0.05, 1), -1),
             dimension = 'trend') %>%
      select(-mod)

    #alternative trends for reference point v1 and v2
    # ## v1: Trend status_1
    # lsp_trend_1 <- lsp_status_allyear %>%
    #   filter(year %in% 2012:2016) %>%
    #   do(mod = lm(status_1 ~ year, data = .)) %>%
    # mutate(rgn_id = "1",
    #        score     =  max(min(coef(.$mod)[['year']] * 0.05, 1), -1),
    #        dimension = 'trend')

    # ## v2: Trend status_2
    # lsp_trend_2 <- lsp_status_allyear %>%
    #   filter(year %in% 2012:2016) %>%
    #   do(mod = lm(status_2 ~ Year, data = .)) %>%
    # mutate(rgn_id = "1",
    #        score     =  max(min(coef(.$mod)[['year']] * 0.05, 1), -1),
    #        dimension = 'trend')

    scores_LSP <- rbind(lsp_status, lsp_trend) %>%
      mutate(goal = "LSP",
             rgn_id = as.integer(rgn_id)) %>%
      select(region_id = rgn_id, goal, dimension, score)

    # return scores
    return(scores_LSP)
}

SP = function(scores){

  ## to calculate the four SP dimesions, average those dimensions for ICO and LSP
  s <- scores %>%
    filter(goal %in% c('ICO','LSP'),
           dimension %in% c('status', 'trend', 'future', 'score')) %>%
    group_by(region_id, dimension) %>%
    summarize(score = mean(score, na.rm=TRUE)) %>%
    ungroup() %>%
    arrange(region_id) %>%
    mutate(goal = "SP") %>%
    select(region_id, goal, dimension, score) %>%
    data.frame()

  # return all scores
  return(rbind(scores, s))
}


CW = function(layers){

  # layers
  lyrs <- c('po_pathogens', 'po_nutrients_3nm', 'po_chemicals_3nm', 'po_trash',
            'cw_chemical_trend', 'cw_nutrient_trend', 'cw_trash_trend', 'cw_pathogen_trend')

  d <-  SelectLayersData(layers, layers=lyrs)  %>%
    select(region_id = id_num, layer, value = val_num)

  ### function to calculate geometric mean:
  geometric.mean2 <- function (x, na.rm = TRUE) {
    if (is.null(nrow(x))) {
      exp(mean(log(x), na.rm = TRUE))
    }
    else {
      exp(apply(log(x), 2, mean, na.rm = na.rm))
    }
  }


  d_pressures <- d %>%
    filter(layer %in% grep('po_', lyrs, value=TRUE))  %>%
    mutate(pressure = 1 - value) %>%  # invert pressures
    group_by(region_id) %>%
    summarize(score = geometric.mean2(pressure, na.rm=TRUE)) %>% # take geometric mean
    mutate(score = score * 100) %>%
    mutate(dimension = "status") %>%
    ungroup()

  d_trends <- d %>%
    filter(layer %in% grep('_trend', lyrs, value=TRUE)) %>%
    mutate(trend = -1 * value)  %>%  # invert trends
    group_by(region_id) %>%
    summarize(score = mean(trend, na.rm = TRUE)) %>%
    mutate(dimension = "trend") %>%
    ungroup()


  # return scores
  scores = rbind(d_pressures, d_trends) %>%
    mutate(goal = "CW") %>%
    select(region_id, goal, dimension, score) %>%
    data.frame()

  ## reference points
  rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "CW", method = "spatial: pressures scaled from 0-1 at raster level",
                     reference_point = NA))
  write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)

  return(scores)
}


HAB = function(layers){

  hab_data <- SelectLayersData(layers, layers = 'hab_iucn_status'); head(hab_data)
  hab_data <- select(hab_data, kingdom = category, species, year, risk.wt = val_num)

  # # lookup for weights status
  # #  LC <- "LOWER RISK/LEAST CONCERN (LR/LC)"
  # #  NT <- "LOWER RISK/NEAR THREATENED (LR/NT)"
  # #  T  <- "THREATENED (T)" treat as "EN"
  # #  VU <- "VULNERABLE (V)"
  # #  EN <- "ENDANGERED (E)"
  # #  LR/CD <- "LOWER RISK/CONSERVATION DEPENDENT (LR/CD)" treat as between VU and NT
  # #  CR <- "VERY RARE AND BELIEVED TO BE DECREASING IN NUMBERS"
  # #  DD <- "INSUFFICIENTLY KNOWN (K)"
  # #  DD <- "INDETERMINATE (I)"
  # #  DD <- "STATUS INADEQUATELY KNOWN-SURVEY REQUIRED OR DATA SOUGHT"

  #calculate status for coral habitats
  coral_status <- filter(hab_data, kingdom == 'ANIMALIA') %>%
    group_by(species) %>%
    filter(year == max(year)) %>%
    ungroup %>%
    summarize(score = (1 - mean(risk.wt)) * 100)

  #calculate status for plant habitats
  plant_status <- filter(hab_data, kingdom == 'PLANTAE') %>%
    group_by(species) %>%
    filter(year == max(year)) %>%
    ungroup %>%
    summarize(score = (1 - mean(risk.wt)) * 100)

  hab_status <- rbind(coral_status, plant_status) %>%
    summarize(score = mean(score)) %>%
    mutate(dimension = 'status',
           rgn_id = 1) %>%
    select(rgn_id, score, dimension)

  # Trend calculations
  ## find species with multiple assessments
  dup_species <- hab_data$species[duplicated(hab_data[,2:4])]
  ### there are no duplicates, meaning only one year of data for each species

  # Assigned a trend of zero (0) - no change - since there is only one year of data for each species, thus do not have information to calculate trend
  hab_trend <- data.frame(rgn_id = 1, score = 0, dimension = 'trend')

  # return scores
  scores_HAB <- rbind(hab_status, hab_trend) %>%
    mutate(goal = "HAB",
           rgn_id = as.integer(rgn_id)) %>%
    select(region_id = rgn_id, goal, dimension, score)

  # return scores
  return(scores_HAB)
}


SPP = function(layers){

  spp_data <- SelectLayersData(layers, layers = 'spp_iucn_status'); head(spp_data)
  spp_data <- select(spp_data, species = category, year, risk.wt = val_num)

  spp_status <- spp_data %>%
    group_by(species) %>%
    filter(year == max(year)) %>%
    ungroup %>%
    summarize(score = (1 - mean(risk.wt)) * 100) %>%
    mutate(dimension = 'status',
           rgn_id = 1) %>%
    select(rgn_id, score, dimension)

  # Trend calculations
  ## find species with multiple assessments
  dup_species <- spp_data$species[duplicated(spp_data[,1])]

  spp_trend_prep <- spp_data %>%
    filter(species %in% dup_species) %>%
    group_by(species) %>%
    filter(year == max(year) | year == min(year)) %>%
    ungroup() %>%
    group_by(species, year) %>%
    summarize(risk.wt_new = mean(risk.wt)) %>%
    ungroup() %>%
    group_by(species) %>%
    mutate(year = ifelse(year == max(year), "max_year", "min_year"))

    #remove species that have been assessed multiple times in the same year
    dup_species_multyear <- spp_trend_prep$species[duplicated(spp_trend_prep[,1])]

  spp_trend <- spp_trend_prep %>%
    filter(species %in% dup_species_multyear) %>%
    spread(year, risk.wt_new) %>%
    summarize(trend_species = ifelse(max_year > min_year, -0.5,
                                     ifelse(max_year < min_year, 0.5,
                                            0))) %>%
    summarize(score = mean(trend_species)) %>%
    mutate(rgn_id = 1,
           dimension = 'trend') %>%
    select(rgn_id, score, dimension)

  # # lookup for weights status
  # #  LC <- "LOWER RISK/LEAST CONCERN (LR/LC)"
  # #  NT <- "LOWER RISK/NEAR THREATENED (LR/NT)"
  # #  T  <- "THREATENED (T)" treat as "EN"
  # #  VU <- "VULNERABLE (V)"
  # #  EN <- "ENDANGERED (E)"
  # #  LR/CD <- "LOWER RISK/CONSERVATION DEPENDENT (LR/CD)" treat as between VU and NT
  # #  CR <- "VERY RARE AND BELIEVED TO BE DECREASING IN NUMBERS"
  # #  DD <- "INSUFFICIENTLY KNOWN (K)"
  # #  DD <- "INDETERMINATE (I)"
  # #  DD <- "STATUS INADEQUATELY KNOWN-SURVEY REQUIRED OR DATA SOUGHT"

  # return scores
  scores_SPP <- rbind(spp_status, spp_trend) %>%
    mutate(goal = "SPP",
           rgn_id = as.integer(rgn_id)) %>%
    select(region_id = rgn_id, goal, dimension, score)

  # return scores
  return(scores_SPP)

}

BD = function(scores){
  d <- scores %>%
    filter(goal %in% c('HAB', 'SPP')) %>%
    filter(!(dimension %in% c('pressures', 'resilience'))) %>%
    group_by(region_id, dimension) %>%
    summarize(score = mean(score, na.rm=TRUE)) %>%
    mutate(goal = 'BD') %>%
    data.frame()

  # return all scores
  return(rbind(scores, d[,c('region_id','goal','dimension','score')]))
}

FinalizeScores = function(layers, conf, scores){

  # get regions
  rgns = SelectLayersData(layers, layers=conf$config$layer_region_labels, narrow = TRUE)

  # add NAs to missing combos (region_id, goal, dimension)
  d = expand.grid(list(score_NA  = NA,
                       region_id = c(rgns[,'id_num'], 0),
                       dimension = c('pressures','resilience','status','trend','future','score'),
                       goal      = c(conf$goals$goal, 'Index')), stringsAsFactors = FALSE); head(d)
  d = subset(d,
             !(dimension %in% c('pressures','resilience','trend') & region_id==0) &
               !(dimension %in% c('pressures','resilience','status','trend') & goal=='Index'))
  scores = merge(scores, d, all = TRUE)[,c('goal','dimension','region_id','score')]

  # order
  scores = arrange(scores, goal, dimension, region_id)

  # round scores
  scores$score = round(scores$score, 2)

  return(scores)
}
