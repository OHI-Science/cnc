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
           !(species == "minor" & year == 2014 & risk.wt == "0")) %>%
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
