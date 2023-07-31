#'
#' @title Calculate sample sizes for survey data
#'
#' @description Function to calculate total and relative sample sizes
#'
#' @param dfrACD : dataframe with aggregated catch data
#' @param colName : column name with sample sizes
#' @param totVal : total SS for relative SS calculations
#'
#' @return list with elements \code{totSS} and \code{relSS}. Both are dataframes.
#'
#' @details None.
#'
#' @export
#'
calcSSs<-function(dfrACD,
                 colName="numIndivs",
                 totVal=200){
  #--calculate sample sizes for survey size comps by XMS
  ss.ByXMS<-reshape2::dcast(dfrACD,STRATUM+YEAR+SEX+MATURITY+SHELL_CONDITION~.,
                                value.var=colName,fun.aggregate=wtsUtilities::Sum);
  names(ss.ByXMS)[6]<-"ss";

  #--calculate total sample sizes for survey size comps
  ss<-reshape2::dcast(ss.ByXMS,STRATUM+YEAR~.,
                      value.var="ss",fun.aggregate=wtsUtilities::Sum);
  names(ss)<-c("stratum","year","ssTot");

  qry<-"select o.STRATUM,o.YEAR,o.SEX,o.MATURITY,o.SHELL_CONDITION,&&totVal*o.ss/t.ssTot as relSS
        from `ss.ByXMS` o left join ss t
        on o.YEAR=t.year and o.STRATUM=t.stratum
        order by o.STRATUM,o.YEAR,o.SEX,o.MATURITY,o.SHELL_CONDITION;";
  qry<-gsub("&&totVal",as.character(totVal),qry,fixed=TRUE);
  relSS.ByXMS<-sqldf::sqldf(qry);
  return(list(totSS=ss.ByXMS,
              relSS=relSS.ByXMS))
}
