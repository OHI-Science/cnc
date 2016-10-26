#' PlotMapMulti
#' Creates multiple static maps by calling PlotMap() by looping through listed goals in the input dataframe
#'
#' @param scores         dataframe with a set of values to be plotted per region, per goal.
#'   OHI standard `scores.csv` dataframe typically includes fields for goal, region ID,
#'   dimension (e.g. 'status' or 'pressure'), and score value.
#' @param rgn_poly       SpatialPolygonsDataFrame object with region IDs to match with values from
#'   scores dataframe.  Default (NULL) will load a default spatial data file from the OHI+ repo
#'   spatial folder
#' @param fld_goal       column name of goal names/IDs; default (NULL) looks for 'goal'
#' @param fld_rgn        column name of region ID values; default (NULL) looks for 'rgn_id' or 'region_id'
#' @param fld_score      column name of value to plot; default (NULL) looks for 'score' or 'value'
#' @param dim_choice     dimension to plot; default (NULL) assumes only a single dimension in dataframe
#' @param print_figs     print figures to graphics device? default FALSE
#' @param figs_dir       save figures to a directory? default NULL will not save any figures
#' @param map_title      title to print on the top of each map; default is no title
#' @param scale_limits   default c(0, 100)
#'
#' @return
#' @export
#'
#' # example call after clone BHI repo and setwd('~/github/bhi/baltic2015')
#' @examples plot_tmap_multi(scores = scores_df %>% filter(region_id < 300), rgn_poly = readOGR('spatial/regions_gcs.geojson'), path_figures = 'reports/figures/BHI_regions')

plot_tmap_multi <- function(scores, #          = read.csv('scores.csv'), # dataframe with regions, goals, dimensions, scores
                         rgn_poly        = NULL, # readOGR(dsn = normalizePath(mapfile_path), "OGRGeoJSON"),
                         fld_goal        = NULL, # 'goal'
                         fld_rgn         = NULL, # 'region_id', # likely 'rgn_id' or 'region_id' of map regions
                         fld_score       = NULL, # 'score', # value to display on map
                         dim_choice      = NULL, # c('score', "future", "pressures", "resilience", "status", "trend")[1]
                         print_figs      = TRUE,
                         figs_dir        = NULL,
                         map_title       = NA,
                         scale_limits    = c(0, 100)) {

  if(is.null(rgn_poly)) {
    ### load from default; overwrite existing .master_rgn_poly just in case

    message('Loading rgn_poly from default .shp file')
    rgn_poly <- rgdal::readOGR(dsn = 'spatial', 'rgn_offshore_gcs')
    assign('.master_rgn_poly', rgn_poly, envir = .GlobalEnv)
  }

  ### rename columns for convenience...
  if(is.null(fld_rgn)) {
    fld_rgn <- names(scores)[names(scores) %in% c('rgn_id', 'region_id')][1]
    ### the [1] at the end is there in case, for some reason, the scores
    ### dataframe has both a rgn_id AND a region_id column...
    if(is.null(fld_rgn)) {
      stop('please assign a valid region ID field name to the fld_rgn argument')
    } else {
      message('using "', fld_rgn, '" as the region ID field')
    }
  }
  if(is.null(fld_goal)) {
    fld_goal <- names(scores)[names(scores) %in% c('goal')][1]
    ### the [1] at the end is there in case, for some reason, the scores
    ### dataframe has multiple valid columns...
    if(is.null(fld_goal)) {
      stop('please assign a valid goal field name to the fld_goal argument')
    } else {
      message('using "', fld_goal, '" as the goal field')
    }
  }
  if(is.null(fld_score)) {
    fld_score <- names(scores)[names(scores) %in% c('score', 'value')][1]
    ### the [1] at the end is there in case, for some reason, the scores
    ### dataframe has multiple columns in the acceptable list...
    if(is.null(fld_score)) {
      stop('please assign a valid score or value field name to the fld_score argument')
    } else {
      message('using "', fld_score, '" as the region score field')
    }
  }

  ### rename the region and score columns just for convenience
  names(scores)[names(scores) == fld_rgn]   <- 'rgn_id'
  names(scores)[names(scores) == fld_score] <- 'score'
  names(rgn_poly@data)[names(rgn_poly@data) == fld_rgn] <- 'rgn_id'

  ### if exists, remove region_id == 0 for mapping
  if(0 %in% scores$rgn_id) {
    message('ignoring region 0')
    scores <- scores %>%
      filter(rgn_id != 0)
  }

  ### if dimension column exists, filter for the chosen dim
  if ('dimension' %in% names(scores) & !is.null(dim_choice)) {
    message('filtering scores to just "', dim_choice, '" dimension')
    scores <- scores %>%
      filter(dimension == dim)
  } else {
    ### either no 'dimension' column or no 'dim' argument:
    ### if each region has one value for each goal, still OK
    if('dimension' %in% names(scores)) {
      message('no dimension chosen; ignoring "dimension" column')
    } else {
      message('no "dimension" column present; assuming all scores are same dimension')
    }
  }

  ### test to make sure no duplicate scores for goal/dimension/region combo
  scores_tmp <- scores %>%
    group_by(rgn_id, goal) %>%
    mutate(n_scores = n()) %>%
    filter(n_scores != 1)

  if(nrow(scores_tmp) > 0) {
    score_dupes <- scores_tmp %>%
      arrange(rgn_id) %>%
      .$rgn_id %>%
      unique()
    stop('Region IDs with multiple scores per goal: ', paste(score_dupes, collapse = ', '))
  }

  ### loop over each goal and subgoal ----

  goals <- unique(scores$goal)
  message('Generating maps for the following goals: \n  ', paste(goals, collapse = ', '))

  for (g in goals) { # g <- goals[1]

    message(sprintf('Mapping %s . . .', g))

    ### filter scores for goal g
    scores_g <-  scores %>%
      filter(goal == g)

    ### generate map!
    plot_tmap(scores       = scores_g,
              rgn_poly     = NULL,
              include_land = TRUE,
              fld_rgn      = 'rgn_id',
              fld_score    = 'score',
              print_fig    = print_figs,
              fig_path     = sprintf('%s/map_%s.png', figs_dir, g),
              map_title    = map_title,
              scale_label  = g,
              scale_limits = scale_limits)
  }
}
