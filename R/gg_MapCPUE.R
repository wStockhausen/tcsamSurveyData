#'
#' @title Create a \pkg{ggplot2} map of cpue-by-haul or by-station
#'
#' @description Function to create a \pkg{ggplot2} map of survey cpue-by-haul or by-station.
#'
#' @param dfrCPUE - dataframe with cpue-by-haul or by-station
#' @param type - string indicating cpue type: "biomass" or "abundance"
#' @param column - name of column (if not wgtCPUE or numCPUE, which are identified automatically)
#' @param maxVal - max value for scales (values > maxVal will be set to maxVal)
#' @param label - label for color/fill/size legends
#' @param basemap - \pkg{ggplot2} layers for basemap of plots (see \code{\link{gg_GetBasemapLayers}})
#' @param plotAsPolys - flag (T/F) to plot as cpue as colored polygons or as colored, scaled points
#' @param plotLand - flag (T/F) to include land in plot
#' @param plotBathym - flag (T/F) to include bathymetry in plot
#' @param plotSurveyGrid - flag (T/F) to include survey grid in plot
#' @param color_scale - \pkg{ggplot2} scale_color object to use for colors
#' @param fill_scale - \pkg{ggplot2} scale_fill object to use for fills
#' @param size_scale - \pkg{ggplot2} scale_size object to use for symbol sizes
#' @param facet_grid - formula to use for gridding plot facets
#' @param facet_wrap - formula to use for wrapping plot facets
#' @param nrow - number of rows for wrapped facets
#' @param ncol - number of columns for wrapped facets
#' @param scales - string indicating how to treat scales among facets
#'
#' @return \pkg{ggplot2} plot object.
#'
#' @details If "column" is NULL, column is set to "wgtCPUE" if \code{type="biomass"}
#' and column is set to "numCPUE" if \code{type="abundance"}.
#'
#' @import ggplot2
#' @import wtsGIS
#'
#' @export
#'
gg_MapCPUE<-function(dfrCPUE,
                     type=c("biomass","abundance"),
                     column=NULL,
                     maxVal=Inf,
                     label=ifelse(!is.null(column),column,type[1]),
                     basemap=gg_GetBasemapLayers(),
                     plotAsPolys=TRUE,
                     plotLand=TRUE,
                     plotBathymetry=TRUE,
                     plotSurveyGrid=TRUE,
                     color_scale=ggplot2::scale_colour_viridis_c(name=label,option="plasma"),
                     fill_scale=ggplot2::scale_fill_viridis_c(name=label,option="plasma"),
                     size_scale=ggplot2::scale_size_area(name=label),
                     facet_grid=NULL,
                     facet_wrap=NULL,
                     nrow=NULL,
                     ncol=NULL,
                     scales="fixed"){
  #--determine column to plot
  type=type[1];
  if (type=="biomass") {
    col = "wgtCPUE";
  } else {
    col = "numCPUE";
  }
  if (!is.null(column)) col = column; #--override col defined by type

  #--remove zeros
  dfrCPUE = dfrCPUE[dfrCPUE[[col]]>0,];

  #--impose maxVal on values, if necessary
  if (is.finite(maxVal)) {
    idx = dfrCPUE[[col]]>maxVal;
    dfrCPUE[[col]][idx] = maxVal;
  }

  #--get survey grid layers
  lst_survey = gisGetSurveyGridLayers();
  if (plotAsPolys){
    sfCPUE = wtsGIS::mergeDataframeWithLayer(dfrCPUE,
                                             lst_survey$grid,
                                             dataID="GIS_STATION",
                                             geomsID = "STATION_ID",
                                             sfJoinType="inner join");
    lyr = ggplot2::geom_sf(data=sfCPUE,mapping=ggplot2::aes_string(fill=col),colour=NA);

    p = ggplot2::ggplot();
    if (plotLand)       p = p + basemap$land;
    if (plotBathymetry) p = p + basemap$bathym;
    p = p + lyr+fill_scale;
    if (plotSurveyGrid) p = p + basemap$grid;
    p = p + basemap$map_scale + basemap$theme;
  } else {
    sfCPUE = wtsGIS::mergeDataframeWithLayer(dfrCPUE,
                                             lst_survey$stations,
                                             dataID="GIS_STATION",
                                             geomsID = "ID",
                                             sfJoinType="inner join");
    lyr = ggplot2::geom_sf(data=sfCPUE,mapping=ggplot2::aes_string(fill=col,colour=col,size=col));

    p = ggplot2::ggplot();
    if (plotLand)       p = p + basemap$land;
    if (plotBathymetry) p = p + basemap$bathym;
    p = p + lyr+color_scale+fill_scale+size_scale;
    if (plotSurveyGrid) p = p + basemap$grid;
    p = p + basemap$map_scale + basemap$theme;
  }

  if (!is.null(facet_grid)) p = p + ggplot2::facet_grid(facet_grid,scales=scales);
  if (!is.null(facet_wrap)) p = p + ggplot2::facet_wrap(facet_wrap,nrow=nrow,ncol=ncol,scales=scales);
  return(p);
}
