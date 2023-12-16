#'
#' @title Re-assign NMFS stations to BSFRF hauls using GIS
#'
#' @description Function to re-assign NMFS stations to BSFRF hauls using the mid-tow location of the latter.
#'
#' @param tbl : path to BSFRF csv file to read or dataframe with BSFRF haul info derived
#' from a dataframe created by [bsfrf.ReadCSV()] (see @details)
#' @param colLon - name of column with longitudes (default = "midtowlongitude")
#' @param colLat - name of column with latitudes (default = "midtowlatitude")
#'
#' @return dataframe identical to input except that "nmfs_stn" has been reassigned
#' based on the NMFS survey grid cell ID in which the mid-tow location fell.
#'
#' @details If \code{tbl} is a character string, it is assumed to be a file path; the
#' associated csv fle is read using [bsfrf.ReadCSV()]. This function creates an
#' \pkg{sf} points layer from the resulting (or input) dataframe
#' using the midtowlongitude and midtowlatitude coordinates for each haul, and
#' determines the NMFS EBS shelf survey station that the haul midpoint falls within using
#' [sf::st_join()] with \code{join=sf::st_within} and the NMFS EBS shelf survey station grid
#' from [tcsamSurveyData::gisGetSurveyGridLayers()]. The NMFS station ID is copied into
#' (or assigned to) the column "nmfs_stn".
#'
#' If \code{tbl} is not a BSFRF survey dataset filename, it must at least have columns with
#' lat and lon coordinates.
#'
#' @seealso [sf::st_within()].
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom sf st_drop_geometry
#' @importFrom sf st_join
#' @importFrom wtsGIS createSF_points
#' @importFrom wtsGIS get_crs
#'
#' @export
#'
bsfrf.AssignNMFSStations<-function(tbl,
                                   colLon="midtowlongitude",
                                   colLat="midtowlatitude"){
  if (is.character(tbl))
    tbl = bsfrf.ReadCSV(tbl);

  #--get NMFS station grid
  grid = tcsamSurveyData::gisGetSurveyGridLayers();

  #--create point geometries based on mid-tow coordinates and classify by NMFS station into which each falls
  dfrHD = tbl |>
             wtsGIS::createSF_points(xCol=colLon,yCol=colLat,crs=wtsGIS::get_crs(4326)) |> #--create sf dataframe using tow locations
             sf::st_join(grid$grid,join=sf::st_within) |> #--join to NMFS EBS shelf station grid, now has "STATION_ID" column
             dplyr::mutate(nmfs_stn=STATION_ID) |>        #--revise (or add) nmfs_stn column
             sf::st_drop_geometry();                      #--revert to dataframe
  #--keep only original columns + nmfs_stn (if not originally present)
  nms = names(tbl);
  if (!("nmfs_stn" %in% nms)) nms = c(nms,"nmfs_stn");
  dfrHD = dfrHD[,nms];
  return(dfrHD);
}
