#'
#' @title Create a basemap layer for maps based on the tmap package
#'
#' @description This function creates a basemap layer for maps based on the tmap package
#'
#' @details Uses \code{wtsGIS::tmap.CreateLayerFromShapefile}.
#'
#' @param layer.land - a tmap layer representing land (or NULL)
#' @param layer.bathym - a tmap layer representing bathymetry (or NULL)
#' @param layers.survey -  a list of tmap layers representing the survey grid and survey stations (or NULL)
#' @param gisPath - path to top level folder for shapefiles
#' @param shapeFile.bathymetry - bathymetry shapefile name
#' @param shapeFile.land - land shapefile name
#' @param shapeFiles.survey - list (grid, stations) of shapfile names to define survey stations
#' @param strCRS - string representation of CRS (default = WGS84) used for ALL shapefiles
#' @param boundingbox - a bounding box (list with elements "bottomleft" and "topright")
#' @param colors.bathym - color for the bathymetry
#' @param points.size - size for the station locations
#' @param as.sf - flag to create spatial datasets as sf (simple features) objects (TRUE) or as sp objects (FALSE)
#'
#' @return - basemap layer in WGS84 based on the tmap package
#'
#' @export
#'
gisCreateBaseMap<-function(layer.land=NULL,
                            layer.bathym=NULL,
                            layers.survey=NULL,
                            gisPath=NULL,
                            shapeFile.bathymetry=NULL,
                            shapeFile.land      =NULL,
                            shapeFiles.survey=list(grid    =NULL,
                                                   stations=NULL),
                            strCRS=wtsGIS::getCRS("WGS84"),
                            boundingbox=list(bottomleft=list(lon=-179,lat=54),
                                             topright  =list(lon=-157,lat=62.5)),
                            colors.bathym="darkblue",
                            points.size=0.01,
                            as.sf=TRUE
                            ){

  basemap<-wtsGIS::createBaseTMap(layer.land=layer.land,
                                  layer.bathym=layer.bathym,
                                  gisPath=gisPath,
                                  shapeFile.land=shapeFile.land,
                                  shapeFile.bathymetry=shapeFile.bathymetry,
                                  strCRS.finl=strCRS,
                                  boundingbox=boundingbox,
                                  colors.bathym=colors.bathym,
                                  points.size=points.size)

  surveyLayers<-layers.survey;
  if (is.null(surveyLayers)){
      surveyLayers<-gisCreateSurveyGridLayers(gisPath=gisPath,
                                              shapeFiles=shapeFiles.survey,
                                              strCRS=strCRS,
                                              as.sf=as.sf);
  }

  if (!is.null(surveyLayers$grid))
      basemap <- basemap + tmap::tm_shape(surveyLayers$grid)+tmap::tm_borders()+
  if (!is.null(surveyLayers$stations))
      basemap <- basemap + tmap::tm_shape(surveyLayers$stations)+tmap::tm_squares(size=points.size);

  return(basemap);
}
