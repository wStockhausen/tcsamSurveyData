#'
#' @title Create the polygon and point geometry tmap layers from polygon and point shapefiles for a survey grid
#'
#' @description This function reads shapefile and creates polygon and point tmap layers for a survey grid.
#'
#' @param gisDir - common toplevel directory for files
#' @param shapeFiles - a list of shapefiles to read ("grid" and "stations")
#' @param strCRS - string describing the final coordinate reference system (CRS)
#' @param as.sf - flag to create grid layers as simple features (sf)
#'
#' @return a 2-element list with the grid and stations map layers consistent with the tmap package
#'
#' @details Uses \code{tmaptools::read_shape()} and \code{sp::spTransform()}.
#'
#' @export
#'
tmap.CreateSurveyGridLayers<-function(
                                  gisDir=system.file("extdata/Shapefiles",package="wtsGIS"),
                                  shapeFiles=list(grid    ="NMFS_Survey_Info/NMFS_EBSSurveyBlocks.shp",
                                                  stations="NMFS_Survey_Info/NMFS_EBSSurveyStations.PointsLL.shp"),
                                  strCRS=tmaptools::get_proj4("longlat",output="character"),
                                  as.sf=FALSE
                                    ){
  stns.polys<-NULL;
  stns.pnts <-NULL;

  if (!is.null(shapeFiles$grid)){
      stns.polys<-tmaptools::read_shape(
                file=file.path(gisDir,shapeFiles$grid),
                as.sf=as.sf,
                stringsAsFactors=FALSE)
      stns.polys<-stns.polys[!is.na(stns.polys$STATION_ID),];
      stns.polys<-sp::spTransform(stns.polys,strCRS);#convert input CRS to strCRS
  }

  if (!is.null(shapeFiles$stations)){
      stns.pnts<-tmaptools::read_shape(
                file=file.path(gisDir,shapeFiles$stations),
                as.sf=as.sf,
                stringsAsFactors=FALSE)
      stns.pnts<-stns.pnts[!is.na(stns.pnts$ID),];
      stns.pnts<-sp::spTransform(stns.pnts,strCRS);#convert input CRS to strCRS
  }

  return(list(grid=stns.polys,stations=stns.pnts))
}
