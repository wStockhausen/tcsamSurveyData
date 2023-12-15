#'
#' @title Create \pkg{sf} dataframes for a survey grid from polygon and point shapefiles
#'
#' @description This function reads shapefiles and creates \pkg{sf} polygon and point dataframes for a survey grid.
#'
#' @param gisPath - path to common toplevel folder for files
#' @param shapeFiles - a list of shapefiles to read ("grid" and "stations", either can be NULL to skip creation of that layer)
#' @param final_crs - a [sf::st_crs()] object to be used as the final coordinate reference system for the spatial layers
#'
#' @return a 2-element list with the grid and stations spatial layers as \pkg{sf} dataframes.
#'
#' @details None.
#'
#' @seealso [wtsGIS::getPackagedLayer()] for pre-packaged \pkg{sf} datasets that will be
#' used in the event that input \code{grid} and \code{stations} shapefiles are NULL (the default).
#'
#' @importFrom dplyr transmute
#' @import wtsGIS
#'
#' @md
#'
#' @export
#'
gisCreateSurveyGridLayers<-function(gisPath=NULL,
                                    shapeFiles=list(grid    =NULL,
                                                    stations=NULL),
                                    final_crs=wtsGIS::get_crs("WGS84")
                                    ){
  stns.polys<-NULL;
  stns.pnts <-NULL;

  if (!is.null(shapeFiles$grid)){
    stns.polys<-wtsGIS::readShapefile(file.path(gisPath,shapeFiles$grid));
    stns.polys<-stns.polys[!is.na(stns.polys$STATION_ID),];
    stns.polys<-wtsGIS::transformCRS(stns.polys,final_crs);
  } else {
    stns.polys = wtsGIS::getPackagedLayer("StandardCrabStations");
    stns.polys = wtsGIS::transformCRS(stns.polys,final_crs);
  }

  if (!is.null(shapeFiles$stations)){
    stns.pnts<-wtsGIS::readShapefile(file.path(gisPath,shapeFiles$stations));
    stns.pnts<-stns.pnts[!is.na(stns.pnts$ID),];
    stns.pnts<-wtsGIS::transformCRS(stns.pnts,final_crs);
  } else {
    tmp = wtsGIS::getPackagedLayer("StandardCrabStations");
    tmp1 = tmp |> as.data.frame() |> dplyr::transmute(ID=STATION_ID,LON=LONGITUDE,LAT=LATITUDE);
    stns.pnts = tmp1 |>
                wtsGIS::createSF_points(xCol="LON",yCol='LAT',crs = wtsGIS::get_crs(4326)) |>
                wtsGIS::transformCRS(final_crs);
    rm(tmp,tmp1);
  }

  return(list(grid=stns.polys,stations=stns.pnts))
}
