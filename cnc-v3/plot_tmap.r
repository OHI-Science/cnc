#' PlotMap
#' Plots static maps of OHI scores.
#'
#' @param scores dataframe with at least 2 columns: rgn_id and values.  Must be no longer
#'   than the number of regions to be mapped.
#' @param rgn_poly SpatialPolygonsDataFrame of regions. Default (NULL) will load a default
#'   spatial file from the OHI+ repo's spatial folder
#' @param map_title optional title for map
#' @param include_land whether or not to map land on top of OHI regions.
#' @param fld_rgn column name holding region ID values; default (NULL) looks for either 'rgn_id' or 'region_id'
#' @param fld_score column name of value to plot; default (NULL) looks for 'score' or 'value'
#' @param scale_label label to place atop the scale in the legend; default (NULL) will use 'score'
#' @param scale_limits default to c(0, 100)
#' @param print_fig logical to print to display; default TRUE
#' @param fig_path file path to save png; default NULL will not save the figure.
#'
#' @return (invisible) ggplot object
#' @export
#'
#' @examples
#'
#'

##### temporary (until added to ohicore)
library(ggplot2) # install.packages('ggplot2')
library(RColorBrewer) # install.packages('RColorBrewer')
library(dplyr)
library(tidyr)
library(tmap) # devtools::install_github('mtennekes/tmap/pkg')


plot_tmap <- function(scores,
                      rgn_poly        = NULL, # PrepSpatial('spatial/regions_gcs.geojson'),
                      map_title       = NA,
                      include_land    = TRUE,
                      fld_rgn         = NULL,
                      fld_score       = NULL,
                      scale_label     = NA,
                      scale_limits    = c(0, 100),
                      print_fig       = TRUE, ### print to display
                      fig_path        = NULL) { ### path to save the plot as an image
                    # allow fig_png to be NULL and then pass it back as a list of ggplot objects so that you could modify it more on {

  ### If no shapefile passed in as rgn_poly, check to see if .master_rgn_poly
  ###   exists; if so, use it, if not, load from default.
  ### If a shapefile is passed in as rgn_poly, ignore .master_rgn_poly
  ### NOTE: operations will be done on rgn_poly, not the master.
  if(is.null(rgn_poly)) {
    if(!exists('.master_rgn_poly')) {
      ### save as global so future iterations don't reload
      # message('Loading rgn_poly from default .geojson file')
      # rgn_poly <- rgdal::readOGR(dsn = 'spatial/regions_gcs.geojson', 'OGRGeoJSON')
      message('Loading rgn_poly from default .shp file')
      rgn_poly <- rgdal::readOGR(dsn = 'spatial', 'rgn_offshore_gcs')
      assign('.master_rgn_poly', rgn_poly, envir = .GlobalEnv)
    } else {
      ### use poly stored in global variable
      message('Using previously-loaded .master_rgn_poly as rgn_poly')
      rgn_poly <- .master_rgn_poly
    }
  }

  ### rename columns for convenience...
  if(is.null(fld_rgn)) {
    fld_rgn <- names(scores)[names(scores) %in% c('rgn_id', 'region_id')][1]
      ### the [1] at the end is there in case, for some reason, the scores
      ### dataframe has both a rgn_id AND a region_id column...
    if(is.null(fld_rgn))
      stop('please assign a valid region ID field name to the fld_rgn argument')
    else
      message('using "', fld_rgn, '" as the region ID field')
  }
  if(is.null(fld_score)) {
    fld_score <- names(scores)[names(scores) %in% c('score', 'value')][1]
    ### the [1] at the end is there in case, for some reason, the scores
    ### dataframe has multiple columns in the acceptable list...
    if(is.null(fld_score))
      stop('please assign a valid score or value field name to the fld_score argument')
    else
      message('using "', fld_score, '" as the region score field')
  }
  if(is.null(scale_label)) {
    scale_label <- fld_score
  }

  ### rename the region and score columns just for convenience
  names(scores)[names(scores) == fld_rgn]   <- 'rgn_id'
  names(scores)[names(scores) == fld_score] <- 'score'
  names(rgn_poly@data)[names(rgn_poly@data) == fld_rgn] <- 'rgn_id'

  ### test if scores dataframe is longer than rgn_id list; this function
  ### can only deal with one score per region.
  if(any(scores$rgn_id %>% duplicated())) {
    score_dupes <- scores[duplicated(scores$rgn_id), ] %>%
      arrange(rgn_id) %>%
      .$rgn_id %>%
      unique()
    stop('Duplicated region IDs: ', paste(score_dupes, collapse = ', '))
  }

  ### join polygon with scores
  message('joining region polygons with scores')
  rgn_poly@data <- rgn_poly@data %>%
    left_join(scores %>%
                dplyr::select(rgn_id, score),
              by = 'rgn_id')

  ### generate map
  ### NOTE: using geojson seems to go MUCH faster, but some regions
  ###   showed up blank; I suspect issues with the polygons from
  ###   that file?
  # rgn_poly1 <- rgn_poly[order(rgn_poly@data$score), ]
  #
  # breaks_vec <- seq(min(scale_limits), max(scale_limits), (max(scale_limits) - min(scale_limits))/5)

  message('assembling the map')
  tmap_plot <- tm_shape(rgn_poly) +
    tm_polygons(col = 'score',
                id = 'rgn_id',
                border.col = 'grey80',
                lwd = .25,
                palette = 'RdYlBu',
                colorNA = 'grey70',
                style = 'cont',
                auto.palette.mapping = FALSE,
                title = scale_label,
                thres.poly = 1e-4) +
    tm_text('rgn_id',
            col = 'black',
            size = .8) +
    tm_layout(title = map_title,
              title.size = .75,
              # bg.color = '#ddeeff',
              # outer.bg.color = 'white',
              frame = FALSE,
              legend.text.size = .7,
              legend.title.size = .9,
              legend.outside = TRUE,
              legend.position = c('left', 'center'),
              legend.bg.color = 'white',
              legend.bg.alpha = .9)

  if(include_land) {
    if(!exists('.master_land_poly')) {
      ### NOTE: The wrld_simpl is an ugly hypersimplified file.
      ### We'd want to create a land file from a not-as-simplified global.
      message('Transforming and cropping land polygons from maptools::wrld_simpl')
      require(maptools)
      data(wrld_simpl)
      wrld_simpl_tmp <- wrld_simpl %>%
        spTransform(raster::crs(rgn_poly)) %>%
        raster::crop(raster::extent(rgn_poly))
      assign('.master_land_poly', wrld_simpl_tmp, envir = .GlobalEnv)
    }

    ### add land poly to tmap object
    tmap_plot <- tmap_plot +
      tm_shape(.master_land_poly) +
      tm_polygons(col = 'grey40',
                  border.col = 'grey50',
                  lwd = .25)
  }

  if(print_fig) {
    message('plotting the map')
    print(tmap_plot)
  }

  if(!is.null(fig_path)) {
    message('saving the map to: \n  ', fig_path)
    save_tmap(tmap_plot, fig_path, width = 7)
  }

  return(invisible(tmap_plot))

}
