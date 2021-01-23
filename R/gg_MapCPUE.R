#'
#' @title Create a \pkg{ggplot2} map of cpue-by-haul or by-station
#'
#' @description Function to create a \pkg{ggplot2} map of survey cpue-by-haul or by-station
#'
#' @param dfrCPUE - dataframe with cpue-by-haul or by-station
#' @param type - string indicating cpue type: "biomass" or "abundance"
#' @param basemap - \pkg{ggplot2} layers for basemap of plots (see \code{\link{gg_GetBasemapLayers}})
#' @param plotAsPolys - flag (T/F) to plot as cpue as colored polygons or as colored, scaled points
#' @param color_scale - \pkg{ggplot2} scale_color object to use for colors
#' @param fill_scale - \pkg{ggplot2} scale_fill object to use for fills
#' @param size_scale - \pkg{ggplot2} scale_size object to use for symbol sizes
#' @param facet_grid - formula to use for gridding plot facets
#' @param facet_wrap - formula to use for wrapping plot facets
#' @param nrow - number of rows for wrapped facets
#' @param ncol - number of columns for wrapped facets
#' @param scales - string indicating how to treat scales among facets
#'
#' @return \pkg{ggplot2} plot object
#'
#' @details None.
#'
#' @import ggplot2
#' @import wtsGIS
#'
#' @export
#'
gg_MapCPUE<-function(dfrCPUE,
                     type=c("biomass","abundance"),
                     basemap=gg_GetBasemapLayers(),
                     plotAsPolys=TRUE,
                     color_scale=ggplot2::scale_colour_viridis_c(option="plasma"),
                     fill_scale=ggplot2::scale_fill_viridis_c(option="plasma"),
                     size_scale=ggplot2::scale_size_area(),
                     facet_grid=NULL,
                     facet_wrap=NULL,
                     nrow=NULL,
                     ncol=NULL,
                     scales="fixed"){
  #--determine column to plot
  if (type[1]=="biomass") {
    col = "wgtCPUE";
  } else {
    col = "numCPUE";
  }
  #--get survey grid layers
  lst_survey = gisGetSurveyGridLayers();
  if (plotAsPolys){
    dfrCPUE = dfrCPUE[dfrCPUE[[col]]>0,];
    sfCPUE = wtsGIS::mergeDataframeWithLayer(dfrCPUE,
                                             lst_survey$grid,
                                             dataID="GIS_STATION",
                                             geomsID = "STATION_ID",
                                             sfJoinType="inner join");
    lyr = ggplot2::geom_sf(data=sfCPUE,mapping=ggplot2::aes_string(fill=col),colour=NA);
    p = ggplot2::ggplot()+
          basemap$land+basemap$bathym+
          lyr+fill_scale+
          basemap$grid+
          basemap$map_scale;
  } else {
    dfrCPUE = dfrCPUE[dfrCPUE[[col]]>0,];
    sfCPUE = wtsGIS::mergeDataframeWithLayer(dfrCPUE,
                                             lst_survey$stations,
                                             dataID="GIS_STATION",
                                             geomsID = "ID",
                                             sfJoinType="inner join");
    lyr = ggplot2::geom_sf(data=sfCPUE,mapping=ggplot2::aes_string(fill=col,colour=col,size=col));
    p = ggplot2::ggplot()+
          basemap$land+basemap$bathym+
          basemap$grid+
          lyr+color_scale+fill_scale+size_scale+
          basemap$map_scale;
  }

  if (!is.null(facet_grid)) p = p + ggplot2::facet_grid(facet_grid,scales=scales);
  if (!is.null(facet_wrap)) p = p + ggplot2::facet_wrap(facet_wrap,nrow=nrow,ncol=ncol,scales=scales);
  return(p);
}
