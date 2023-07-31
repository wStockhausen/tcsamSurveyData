#'
#' @title Create a \pkg{ggplot2} map of cpue-by-haul or by-station
#'
#' @description Function to create a \pkg{ggplot2} map of survey cpue-by-haul or by-station.
#'
#' @param dfrCPUE : dataframe with cpue-by-haul or by-station (see Details)
#' @param type : string indicating cpue type: "biomass" or "abundance" (or "other")
#' @param column : name of column (if not wgtCPUE or numCPUE, which are identified automatically)
#' @param minVal : min value for scales (values <= minVal are not plotted; default = 0)
#' @param maxVal : max value for scales (values > maxVal will be set to maxVal; default = Inf)
#' @param label : label for color/fill/size legends
#' @param basemap : \pkg{ggplot2} layers for basemap of plots (see [gg_GetBasemapLayers])
#' @param plotAsPolys : flag (T/F) to plot as cpue as colored polygons or as colored, scaled points
#' @param plotLand : flag (T/F) to include land in plot
#' @param plotBathymetry : flag (T/F) to include bathymetry in plot
#' @param plotSurveyGrid : flag (T/F) to include survey grid in plot
#' @param color_scale : \pkg{ggplot2} scale_color object to use for colors
#' @param fill_scale : \pkg{ggplot2} scale_fill object to use for fills
#' @param size_scale : \pkg{ggplot2} scale_size object to use for symbol sizes
#' @param facet_grid : formula to use for gridding plot facets
#' @param facet_wrap : formula to use for wrapping plot facets
#' @param nrow : number of rows for wrapped facets
#' @param ncol : number of columns for wrapped facets
#' @param scales : string indicating how to treat scales among facets
#'
#' @return a \pkg{ggplot2} plot object.
#'
#' @details If dfrCPUE is not a \pkg{sf} dataframe, then the function calls
#' [gisGetSurveyGridLayers()] to create a pkg{sf} dataframe using [wtsGIS::mergeDataframeWithLayer()],
#' merging the CPUE dataframe with either the \code{grid} or \code{points} element of the survey
#' grid layers list. The dataframe is joined to the gis layer using the `GIS_STATION` and `ID`
#' columns. If `column` is NULL, then it is set to "wgtCPUE" if \code{type="biomass"}
#' or "numCPUE" if \code{type="abundance"}.
#'
#' An alternative is to provide a \pkg{sf} dataframe, in which case data in the column identified by
#' `column` is mapped using the active geometry (in this case `plotAsPolys` and `plotAsPoints` are ignored).
#' This provides a way of "faking" the code to plot any scalar data associated with a \pkg{sf} dataframe.
#'
#' #'
#' @import ggplot2
#' @import sf
#' @import wtsGIS
#'
#' @export
#'
gg_MapCPUE<-function(dfrCPUE,
                     type=c("biomass","abundance"),
                     column=NULL,
                     minVal=0,
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
  col = ifelse(type=="biomass","wgtCPUE","numCPUE");
  if (!is.null(column)) col = column; #--override col defined by type

  #--impose minVal on values
  if (is.finite(minVal)) dfrCPUE = dfrCPUE[minVal<dfrCPUE[[col]],];

  #--impose maxVal on values, if necessary
  if (is.finite(maxVal)) {
    idx = dfrCPUE[[col]]>maxVal;
    dfrCPUE[[col]][idx] = maxVal;
  }

  isDFR = TRUE;
  if (inherits(dfrCPUE,"sf")) isDFR = FALSE;

  if (isDFR){
    #--get survey grid layers
    lst_survey = gisGetSurveyGridLayers();
    #--join dataframe to appropriate grid layer
    if (plotAsPolys){
      sfCPUE = wtsGIS::mergeDataframeWithLayer(dfrCPUE,
                                               lst_survey$grid,
                                               dataID="GIS_STATION",
                                               geomsID = "STATION_ID",
                                               sfJoinType="inner join");
      lyr = ggplot2::geom_sf(data=sfCPUE,mapping=ggplot2::aes_string(fill=col),colour=NA);
    } else {
      sfCPUE = wtsGIS::mergeDataframeWithLayer(dfrCPUE,
                                               lst_survey$stations,
                                               dataID="GIS_STATION",
                                               geomsID = "ID",
                                               sfJoinType="inner join");
      lyr = ggplot2::geom_sf(data=sfCPUE,mapping=ggplot2::aes_string(fill=col,colour=col,size=col));
    }
  } else {
    #--dfrCPUE is already an sf object
    plotAsPolys = FALSE;
    if (sf::st_geometry_type(dfrCPUE,by_geometry=FALSE) %in% c("POLYGON","MULTIPOLYGON")) plotAsPolys=TRUE;
    if (plotAsPolys) {
      message("plotting as polygons")
      lyr = ggplot2::geom_sf(data=dfrCPUE,mapping=ggplot2::aes_string(fill=col),colour=NA);
    } else {
      message("plotting as points")
      lyr = ggplot2::geom_sf(data=dfrCPUE,mapping=ggplot2::aes_string(fill=col,colour=col,size=col));
    }
  }


  p = ggplot2::ggplot();
  if (plotLand)       p = p + basemap$land;
  if (plotBathymetry) p = p + basemap$bathym;
  p = p + lyr+fill_scale;
  if (!plotAsPolys)   p = p + color_scale + size_scale;
  if (plotSurveyGrid) p = p + basemap$grid;
  p = p + basemap$map_scale + basemap$theme;

  if (!is.null(facet_grid)) p = p + ggplot2::facet_grid(facet_grid,scales=scales);
  if (!is.null(facet_wrap)) p = p + ggplot2::facet_wrap(facet_wrap,nrow=nrow,ncol=ncol,scales=scales);
  return(p);
}
