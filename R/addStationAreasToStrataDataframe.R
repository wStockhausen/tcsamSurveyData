#'
#'@title Add station area to strata information from a NMFS trawl survey strata dataframe
#'
#'@description This function counts the number of stations conducted per year and stratum,
#'             calculates the (effective) area associated with each individual station,
#'             and adds it as a column to the input dataframe. Note that this function is
#'             now (20190823) called in the course of evaluating \code{selectStrata.TrawlSurvey}.
#'
#'@param dfrSD : table (dataframe) of survey station/strata data
#'
#'@details  The returned dataframe has columns:
#'\itemize{\item {YEAR}
#'         \item {STRATUM}
#'         \item {STRATUM_CODE}
#'         \item {STRATUM_AREA - full stratum area, in square nautical miles}
#'         \item {GIS_STATION}
#'         \item {STATION_LONGITUDE}
#'         \item {STATION_LATITUDE}
#'         \item {STATION_AREA - in square nautical miles}
#'         \item {STRATUM_AREA_BYSTATION - 'stratum' area for included stations only, in square nautical miles}
#'         }
#' Areas are in square nautical miles (worth repeating).
#'
#' If the `STATION_AREA` and/or `STRATUM_AREA_BYSTATION` columns are present in the input
#' dataframe `dfrSD`, they are dropped prior to any other processing and re-created based only
#' on the stations included in `dfrSD`.
#'
#' @return a dataframe with strata/stations info + (effective) area by individual station.
#'
#' @import dplyr
#' @import tidyselect
#' @import wtsGIS
#'
#' @export
#'
addStationAreasToStrataDataframe<-function(dfrSD){
  #--drop old station area-related info, if any
  dfrSD = dfrSD |> dplyr::select(!tidyselect::any_of(c("STATION_AREA","STRATUM_AREA_BYSTATION")));

  #----get the survey grid layers (grid and stations)
  grid = tcsamSurveyData::gisGetSurveyGridLayers()$grid |>
           dplyr::rename(STRATUM_AREA_GIS=TOTAL_AREA,
                         STRATUM_CODE=STRATUM);

  #--merge stations from dfrSD with GIS polygon information
  #dfrUniqStns<-unique(dfrSD[,c("YEAR","STRATUM","GIS_STATION")]);
  dfrUniqStns   = dfrSD |> dplyr::distinct(YEAR,STRATUM,GIS_STATION);
  #--following used to use "right_join", but makes no sense when strata stations
  #----are a subset of complete strata stations
  polysUniqStns = wtsGIS::mergeDataframeWithLayer(dfrUniqStns,
                                                  grid,
                                                  dataID="GIS_STATION",
                                                  geomsID="STATION_ID",
                                                  sfJoinType="inner join");

  #--calculate the area of each stratum by summing over the area associated with each station
  tmp1 = polysUniqStns |>
           sf::st_drop_geometry() |>
           dplyr::select(YEAR,STRATUM,GIS_STATION,STATION_AREA=STN_AREA);
  #--add summed station area stratum areas-by year, station
  tmp1 = tmp1 |>
           dplyr::left_join(tmp1 |>
                              dplyr::group_by(YEAR,STRATUM) |>
                              dplyr::summarize(STRATUM_AREA_BYSTATION=sum(STATION_AREA)) |>
                              dplyr::ungroup());

  #--add station areas and summed station area stratum areas-by year, station to strata table
  dfr = dfrSD |> dplyr::left_join(tmp1,c("YEAR","STRATUM","GIS_STATION")); ##-changed from inner_join to left_join for crabpack compatibility

  return(dfr)
}
