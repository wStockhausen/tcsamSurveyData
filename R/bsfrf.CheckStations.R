#'
#' @title Check BSFRF data for bad station ids
#'
#' @description Function to check BSFRF data for bad station ids.
#'
#' @param tbl : path to BSFRF csv file to read or dataframe created by [bsfrf.ReadCSV()]
#' @return dataframe with "bad" station ids
#'
#' @details If \code{tbl} is a character string, it is assumed to be a file path; the
#' associated csv fle is read using [bsfrf.ReadCSV()]. This function extracts the
#' distinct hauls in the input file/dataframe, creates an \pkg{sf} points layer
#' using the midtowlongitude and midtowlatitude coordinates for each haul, and
#' determines the NMFS grid station that the haul midpoint falls within using
#' [sf::st_join()] with \code{join=sf::st_within}.
#'
#' @seealso [sf::st_within()].
#'
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom wtsGIS createSF_points
#' @importFrom wtsGIS get_crs
#' @importFrom sf st_drop_geometry
#' @importFrom sf st_join
#'
#' @export
#'
bsfrf.CheckStations<-function(tbl){
  if (is.character(tbl))
    tbl = bsfrf.ReadCSV(tbl);

  #--get NMFS station grid
  grid = tcsamSurveyData::gisGetSurveyGridLayers();

  #--determine distinct hauls
  dfrHD = tbl |> dplyr::distinct(year,study,boat,tow,nmfs_stn,date,time,
                                  depth_ftm,temp_c,aswept_nm2,midtowlatitude,midtowlongitude);

  #--create point geometries based on mid-tow coordinates and classify by NMFS station into which each falls
  sfHD = dfrHD |>
             wtsGIS::createSF_points(xCol="midtowlongitude",yCol="midtowlatitude",crs=wtsGIS::get_crs(4326)) |>
             sf::st_join(grid$grid,join=sf::st_within);#--now has "nmfs_stn" and "STATION_ID" columns

  ##--check for inconsistent nmfs_stn and NMFS station IDs and create table with non-joining IDs
  dfrBad = sfHD |> sf::st_drop_geometry() |>
                   dplyr::filter(nmfs_stn!=STATION_ID) |>
                   dplyr::select(year,study,boat,tow,nmfs_stn,STATION_ID,date,time,
                                  depth_ftm,temp_c,aswept_nm2,midtowlatitude,midtowlongitude);
  return(dfrBad);
}
