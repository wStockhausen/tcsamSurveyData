#'
#'@title Add station area to strata information from a NMFS trawl survey strata file
#'
#'@description This function counts the number of stations conducted per year and stratum,
#'             calculates the (effective) area associated with each individual station,
#'             and adds it as a column to the input dataframe. Note that this function is
#'             now (20190823) called in the course of evaluating \code{selectStrat.TrawlSurvey}.
#'
#'@param dfrSD : table (dataframe) of survey station/strata data
#'
#'@details  The returned dataframe has columns:
#'\itemize{\item {YEAR}
#'         \item {STRATUM}
#'         \item {STRATUM_CODE}
#'         \item {STRATUM_AREA}
#'         \item {GIS_STATION}
#'         \item {STATION_LONGITUDE}
#'         \item {STATION_LATITUDE}
#'         \item {STATION_AREA}
#'         \item {STRATUM_AREA_BYSTATION}
#'         }
#' Areas are in square nautical miles.
#'
#' @return a dataframe with strata/stations info + (effective) area by individual station.
#'
#' @importFrom dplyr transmute
#' @import magrittr
#' @import wtsGIS
#'
#' @export
#'
addStationAreasToStrataDataframe<-function(dfrSD){
  #----get the survey grid layers (grid and stations)
  grid = tcsamSurveyData::gisGetSurveyGridLayers()$grid %>%
           dplyr::transmute(AREA=TOTAL_AREA,STATION_ID=STATION_ID);
  #merge stations from dfrSD with GIS polygon information
  dfrUniqStns<-unique(dfrSD[,c("YEAR","STRATUM","GIS_STATION")]);
  polysUniqStns <- wtsGIS::mergeDataframeWithLayer(dfrUniqStns,
                                                   grid,
                                                   dataID="GIS_STATION",
                                                   geomsID="STATION_ID");

  #----calculate the area of each stratum by summing over the area associated with each station
  #-----NOTE: STATION_AREA, STRATUM_AREA_BYSTATION will be in square nautical miles
  if ("TOTAL_AREA" %in% names(polysUniqStns)) polysUniqStns[["AREA"]] = polysUniqStns[["TOTAL_AREA"]];
  if ("TOTAL_AREA" %in% names(polysUniqStns)) polysUniqStns[["AREA"]] = polysUniqStns[["TOTAL_AREA"]];
  tmp1<-polysUniqStns[,c("YEAR","STRATUM","GIS_STATION","AREA"),drop=TRUE];#keep some columns, drop geometry
  tmp1$STATION_AREA <- tmp1$AREA/(1852*1852);#convert to sq. nm.
  qry<-"select YEAR,STRATUM,
        sum(STATION_AREA) as STRATUM_AREA_BYSTATION
        from tmp1
        group by YEAR,STRATUM
        order by YEAR,STRATUM;";
  tmp2<-sqldf::sqldf(qry);
  qry<-"select t1.YEAR,
               t1.STRATUM,
               t1.GIS_STATION,
               t1.STATION_AREA,
               t2.STRATUM_AREA_BYSTATION
        from tmp1 as t1, tmp2 as t2
        where t1.YEAR=t2.YEAR and t1.STRATUM=t2.STRATUM;"
  tmp3<-sqldf::sqldf(qry);

  #----add station area and straum area based on sum over station areas
  qry<-"select
          s.YEAR,s.STRATUM,s.STRATUM_CODE,s.STRATUM_AREA,
          s.GIS_STATION,s.STATION_LONGITUDE,s.STATION_LATITUDE,
          t.STATION_AREA as STATION_AREA,t.STRATUM_AREA_BYSTATION as STRATUM_AREA_BYSTATION
        from dfrSD as s, tmp3 as t
        where s.YEAR=t.YEAR and s.GIS_STATION=t.GIS_STATION;";
  dfr<-sqldf::sqldf(qry);

  return(dfr)
}
