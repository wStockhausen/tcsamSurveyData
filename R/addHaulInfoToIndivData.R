#'
#' @title Add haul-associated info to an indiv data datframe
#'
#' @description Function to add haul-associated info to an indiv data datframe.
#'
#' @param tbl_hauls  : hauls table (dataframe) from call to selectHauls.TrawlSurvey(...) \[required\]
#' @param tbl_indivs : indiv data table (dataframe) from call to selectIndivs.TrawlSurvey(...) \[required\]
#'
#' @return dataframe with haul-associated info dropped
#'
#' @details Individuals from hauls not in tbl_hauls will be dropped.
#'
#' @importFrom sqldf sqldf
#'
#' @export
#'
addHaulInfoToIndivData<-function(
                          tbl_hauls,
                          tbl_indivs){
  ##add in haul info
  indivCols<-names(tbl_indivs);
  indivCols<-indivCols[tolower(indivCols)!="hauljoin"];
  indivCols<-paste0("i.",indivCols,collapse=",")
  qry<-"select
          h.hauljoin,h.YEAR,h.STRATUM,h.GIS_STATION as STATION,
          h.MID_LATITUDE as LATITUDE,h.MID_LONGITUDE as LONGITUDE,
          &&indivCols
        from
          tbl_hauls as h,
          tbl_indivs as i
        where
          h.hauljoin=i.hauljoin
        order by
          YEAR,STRATUM;";
  qry<-gsub("&&indivCols",indivCols,qry,fixed=TRUE);
  dfr<-sqldf(qry);
  return(dfr);
}
