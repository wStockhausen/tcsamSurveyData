#'
#' @title Create the polygon and point geometry tmap layers from polygon and point shapefiles for a survey grid
#'
#' @description This function reads shapefile and creates polygon and point tmap layers for a survey grid.
#'
#' @param gisDir - common toplevel directory for files
#' @param shapeFiles - a list of shapefiles to read ("grid" and "stations")
#' @param strCRS - string describing the final coordinate reference system (CRS)
#'
#' @return a 2-element list with the grid and stations map layers consistent with the tmap package
#'
#' @details Uses \code{tmaptools::read_shape()} and \code{sp::spTransform()}.
#'
#' @export
#'
tmap.CreateSurveyGridLayers<-function(
                                  gisDir="~",
                                  shapeFiles=list(grid    ="NMFS Survey Info/NMFS_EBSSurveyBlocks.shp",
                                                  stations="NMFS Survey Info/NMFS_EBSSurveyStations.PointsLL.shp"),
                                  strCRS=tmaptools::get_proj4("longlat")
                                    ){
  stns.polys<-NULL;
  stns.pnts <-NULL;

  if (!is.null(shapeFiles$grid)){
      stns.polys<-tmaptools::read_shape(
                file=file.path(gisDir,shapeFiles$grid),
                stringsAsFactors=FALSE)
      stns.polys<-stns.polys[!is.na(stns.polys$STATION_ID),];
      stns.polys<-sp::spTransform(stns.polys,strCRS);#convert input CRS to strCRS
  }

  if (!is.null(shapeFiles$stations)){
      stns.pnts<-tmaptools::read_shape(
                file=file.path(gisDir,shapeFiles$stations),
                stringsAsFactors=FALSE)
      stns.pnts<-stns.pnts[!is.na(stns.pnts$ID),];
      stns.pnts<-sp::spTransform(stns.pnts,strCRS);#convert input CRS to strCRS
  }

  return(list(grid=stns.polys,stations=stns.pnts))
}
