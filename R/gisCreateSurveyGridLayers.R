#'
#' @title Create the polygon and point geometry tmap layers from polygon and point shapefiles for a survey grid
#'
#' @description This function reads shapefile and creates polygon and point tmap layers for a survey grid.
#'
#' @param gisPath - path to common toplevel folder for files
#' @param shapeFiles - a list of shapefiles to read ("grid" and "stations"), or NULL to use defaults for the NMFS EBS survey
#' @param strCRS - string describing the final coordinate reference system (CRS) (or NULL to use default)
#' @param as.sf - flag to create grid layers using simple features (sf) datasets (TRUE) or sp datasets (FALSE)
#'
#' @return a 2-element list with the grid and stations map layers consistent with the tmap package
#'
#' @details Uses \code{tmaptools::read_shape()} and \code{sp::spTransform()}.
#'
#' @export
#'
gisCreateSurveyGridLayers<-function(gisPath=NULL,
                                    shapeFiles=list(grid    =NULL,
                                                    stations=NULL),
                                    strCRS=wtsGIS::getCRS("WGS84"),
                                    as.sf=TRUE
                                    ){
  stns.polys<-NULL;
  stns.pnts <-NULL;

  if (!is.null(shapeFiles)){
    if (!is.null(shapeFiles$grid)){
        stns.polys<-tmaptools::read_shape(
                  file=file.path(gisPath,shapeFiles$grid),
                  as.sf=as.sf,
                  stringsAsFactors=FALSE)
        stns.polys<-stns.polys[!is.na(stns.polys$STATION_ID),];
        stns.polys<-sp::spTransform(stns.polys,strCRS);#convert input CRS to strCRS
    } else {
      stns.polys<-wtsGIS::getPackagedLayer(layerName="EBS_SurveyBlocks",as.sf=as.sf);
    }

    if (!is.null(shapeFiles$stations)){
        stns.pnts<-tmaptools::read_shape(
                  file=file.path(gisPath,shapeFiles$stations),
                  as.sf=as.sf,
                  stringsAsFactors=FALSE)
        stns.pnts<-stns.pnts[!is.na(stns.pnts$ID),];
        stns.pnts<-sp::spTransform(stns.pnts,strCRS);#convert input CRS to strCRS
    } else {
      stns.pnts<-wtsGIS::getPackagedLayer(layerName="EBS_SurveyStations",as.sf=as.sf);
    }
  } else {
      stns.polys<-wtsGIS::getPackagedLayer(layerName="EBS_SurveyBlocks",as.sf=as.sf);
      stns.pnts<-wtsGIS::getPackagedLayer(layerName="EBS_SurveyStations",as.sf=as.sf);
  }

  return(list(grid=stns.polys,stations=stns.pnts))
}
