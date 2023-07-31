#'
#' @title Calculate the effective area occupied by different life stages
#'
#' @description This function calculate the effective area occupied by different life stages.
#'
#' @param dfrCPUE - dataframe with cpue estimates by station (i.e., output from from [calcCPUE.ByStation])
#'
#' @return a 2-element list with dataframes giving effective area occupied based on abundance and biomass.
#'
#' @details Uses the respective survey station area from the survey grid
#'
#' @importFrom dplyr inner_join
#' @importFrom sqldf sqldf
#'
#' @export
#'
calcEffectiveAreaOccupied<-function(dfrCPUE){
  grid<-gisGetSurveyGridLayers()[[1]];#sf dataframe

  #--join grid to cpue dataframe by station id
  dfr<-dplyr::inner_join(dfrCPUE,grid,by=c("GIS_STATION"="STATION_ID"));

  #--1 nm = 1852 m
  #--area in m^2
  #--numCPUE in num/(sq. nm)
  #--wgtCPUE in  mt/(sq. nm)
  nm2m<-1852;

  #--calculate abundance-weighted areal coverage
  qry<-"select
          YEAR,SEX,MATURITY,SHELL_CONDITION,SIZE,
          sum(numCPUE*AREA*AREA)/(nm2m*nm2m) as numerator,
          sum(numCPUE*AREA)/nm2m as denominator,
          sum(numCPUE*AREA*AREA)/sum(numCPUE*AREA)*(1/nm2m) as eff_area
        from dfr
        group by YEAR,SEX,MATURITY,SHELL_CONDITION,SIZE;";
  cvr_abd<-sqldf::sqldf(qry);

  #--calculate biomass-weighted areal coverage
  qry<-"select
          YEAR,SEX,MATURITY,SHELL_CONDITION,SIZE,
          sum(wgtCPUE*AREA*AREA)/(nm2m*nm2m) as numerator,
          sum(wgtCPUE*AREA)/nm2m as denominator,
          sum(wgtCPUE*AREA*AREA)/sum(wgtCPUE*AREA)*(1/nm2m) as eff_area
        from dfr
        group by YEAR,SEX,MATURITY,SHELL_CONDITION,SIZE;";
  cvr_wgt<-sqldf::sqldf(qry);

  return(list(abd=cvr_abd,wgt=cvr_wgt))
}
