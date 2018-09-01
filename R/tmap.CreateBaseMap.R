#'
#' @title Create a basemap layer for maps based on the tmap package
#'
#' @description This function creates a basemap layer for maps based on the tmap package
#'
#' @details Uses \code{wtsGIS::tmap.CreateLayerFromShapefile}.
#'
#' @param layer.land - a tmap layer representing land
#' @param layer.bathym -
#' @param layers.survey -
#' @param gisDir - path to top level folder for shapefiles
#' @param shapeFile.bathymetry - bathymetry shapefile
#' @param shapeFile.land - land shapefile
#' @param shapeFiles.survey - list (grid, stations) of shapfiles to define survey stations
#' @param strCRS - string representation of CRS (default = WGS84) used for ALL shapefiles
#' @param boundingbox - a bounding box (list with elements "bottomleft" and "topright")
#' @param colors.bathym - color for the bathymetry
#' @param points.size - size for the station locations
#'
#' @return - basemap layer in WGS based on the tmap package
#'
#' @export
#'
tmap.CreateBaseMap<-function( layer.land=NULL,
                              layer.bathym=NULL,
                              layers.survey=NULL,
                              gisDir=system.file("extdata/Shapefiles",package="wtsGIS"),
                              shapeFile.bathymetry="Bathymetry/ShelfBathymetry.shp",
                              shapeFile.land      ="Land/Alaska.shp",
                              shapeFiles.survey=list(grid    ="NMFS_Survey_Info/NMFS_EBSSurveyBlocks.shp",
                                                     stations="NMFS_Survey_Info/NMFS_EBSSurveyStations.PointsLL.shp"),
                              strCRS=tmaptools::get_proj4("longlat",output="character"),
                              boundingbox=list(bottomleft=list(lon=-179,lat=54),
                                               topright  =list(lon=-157,lat=62.5)),
                              colors.bathym="darkblue",
                              points.size=0.01
                              ){

  land<-layer.land;
  if (is.null(land))
    if (!is.null(shapeFile.land))
        land<-wtsGIS::tmap.CreateLayerFromShapefile(file.path(gisDir,shapeFile.land),strCRS=strCRS);

  bathym<-layer.bathym;
  if (is.null(bathym))
    if (!is.null(shapeFile.bathymetry))
        bathym<-wtsGIS::tmap.CreateLayerFromShapefile(file.path(gisDir,shapeFile.bathymetry),strCRS=strCRS);


  surveyLayers<-layers.survey;
  if (is.null(surveyLayers))
    if (!is.null(shapeFiles.survey))
      surveyLayers<-tmap.CreateSurveyGridLayers(gisDir=gisDir,shapeFiles=shapeFiles.survey,strCRS=strCRS);

  #define bounding box for map extent
  bbext<-tmaptools::bb(land);#just to get a bounding box
  bbext['xmin']<-boundingbox$bottomleft$lon;
  bbext['ymin']<-boundingbox$bottomleft$lat;
  bbext['xmax']<-boundingbox$topright$lon;
  bbext['ymax']<-boundingbox$topright$lat;

  #basemap using WGS84
  basemap<-tmap::tm_shape(land,bbox=bbext,is.master=TRUE)+tmap::tm_fill();
  if (!is.null(bathym))
      basemap <- basemap + tmap::tm_shape(bathym)+tmap::tm_lines(col=colors.bathym);
  if (!is.null(surveyLayers$grid))
      basemap <- basemap + tmap::tm_shape(surveyLayers$grid)+tmap::tm_borders()+
  if (!is.null(surveyLayers$stations))
      basemap <- basemap + tmap::tm_shape(surveyLayers$stations)+tmap::tm_squares(size=points.size);

  return(basemap);
}
