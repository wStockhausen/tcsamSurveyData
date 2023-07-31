#'
#' @title Create a basemap layer for maps based on the \pkg{tmap} package
#'
#' @description This function creates a basemap layer for maps based on the \pkg{tmap} package
#'
#' @details Uses [wtsGIS::tmap_CreateBasemap] and [wtsGIS::transformCRS].
#'
#' @param layer.land - a spatial layer representing land (or NULL)
#' @param layer.bathym - a spatial layer representing bathymetry (or NULL)
#' @param layers.survey -  a list of spatial layers representing the survey grid and survey stations (or NULL)
#' @param final.crs - string representation of CRS (default = WGS84) used for ALL shapefiles
#' @param bbox - a \pkg{sf} bounding box
#' @param colors.bathym - color for the bathymetry
#' @param points.size - size for the station locations
#'
#' @return - a \pkg{tmap}-style basemap
#'
#' @import tmap
#' @importFrom wtsGIS get_crs
#' @importFrom wtsGIS getStandardBBox
#' @importFrom wtsGIS tmap_CreateBasemap
#' @importFrom wtsGIS transformCRS
#'
#' @export
#'
tmap_CreateBasemap<-function(layer.land=NULL,
                             layer.bathym=NULL,
                             layers.survey=NULL,
                             final.crs=wtsGIS::get_crs("WGS84"),
                             bbox=wtsGIS::getStandardBBox("EBS"),
                             colors.bathym="darkblue",
                             points.size=0.01
                           ){

  basemap<-wtsGIS::tmap_CreateBasemap(layer.land=layer.land,
                                      layer.bathym=layer.bathym,
                                      final.crs=final.crs,
                                      bbox=bbox,
                                      colors.bathym=colors.bathym,
                                      points.size=points.size)

  surveyLayers<-layers.survey;
  if (is.null(surveyLayers)){
      surveyLayers<-gisGetSurveyGridLayers();
  }

  if (!is.null(surveyLayers$grid)){
      lyr<-wtsGIS::transformCRS(surveyLayers$grid,final.crs);
      basemap <- basemap + tmap::tm_shape(lyr)+tmap::tm_borders();
  }
  if (!is.null(surveyLayers$stations)){
      lyr<-wtsGIS::transformCRS(surveyLayers$stations,final.crs);
      basemap <- basemap + tmap::tm_shape(lyr)+tmap::tm_squares(size=points.size,col="black");
  }
  return(basemap);
}
