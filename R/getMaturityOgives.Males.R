#'
#' @title Function to extract male maturity ogives from a dataframe
#'
#' @description Function to extract female maturity ogives from a size composition dataframe.
#'
#' @param fn : filename for maturity-classified chela height dataset
#' @param tbl_strata : data frame from call to \code{\link{selectStrata.TrawlSurvey}} \[required\]
#' @param byEBS : flag to aggregate across strata to EBS
#' @param cutpts : vector of cutpoints to create size bins from
#' @param showPlot : flag to show plot of ogives
#' @param export  : boolean flag to export results to csv file
#' @param out.csv : name of output csv file                    (ignored if NULL)
#' @param out.dir : base path for output csv file              (set to folder of input csv file or current working directory)
#' @param verbosity : flags for intermediate output
#' @param ... : parameters passed to plotMaturityOgives(...)
#'
#' @return dataframe with columns: \cr
#'\itemize{
#'\item  YEAR
#'\item  STRATUM
#'\item  SEX
#'\item  SIZE
#'\item  numIndivs
#'\item  ogive
#'}
#'
#' @details None.\cr
#'
#' @importFrom reshape2 dcast
#' @importFrom utils read.csv write.csv
#' @importFrom sqldf sqldf

#' @export
#'
getMaturityOgives.Males<-function(
                           fn=NULL,
                           tbl_strata=NULL,
                           cutpts=seq(from=25,to=185,by=5),
                           byEBS=TRUE,
                           showPlot=TRUE,
                           export=FALSE,
                           out.csv="MaturityOgives.Males.csv",
                           out.dir=NULL,
                           verbosity=0,
                           ...){
  if (verbosity>0) cat("starting getMaturityOgives.Males().\n");
  dfr<-read.csv(fn);
  dfr$YEAR <- as.character(floor(dfr$CRUISE/100));
  #make sure this dfr is only new shell male crab
  dfr<-dfr[dfr$SHELL_CONDITION<3,];                    #use ONLY new shell crab
  cuts<-cut(dfr$WIDTH,cutpts,right=FALSE,labels=FALSE);
  dfr$SIZE <- cutpts[cuts];
  dfr$numIndivs<-1;

  qry<-"select
           t.YEAR,t.STRATUM,t.GIS_STATION,d.MATURITY,d.SIZE,
           sum(d.numIndivs) as numIndivs
        from
           dfr as d left join tbl_strata as t
        on
           d.YEAR = t.YEAR and
           d.STATION = t.GIS_STATION
        group by
           t.YEAR, t.STRATUM, t.GIS_STATION,d.MATURITY,d.SIZE;"
  dfr1<-sqldf::sqldf(qry);
  dfr1<-dfr1[!is.na(dfr1$YEAR),]

  if (byEBS){
    dfr1$STRATUM<-"EBS";
  }


  dfrN<-reshape2::dcast(dfr1,"YEAR+STRATUM+SIZE~.",fun.aggregate=wtsUtilities::Sum,value.var="numIndivs");
  names(dfrN)[4]<-"numIndivs";
  dfrA<-reshape2::dcast(dfr1,"YEAR+STRATUM+SIZE~MATURITY",fun.aggregate=wtsUtilities::Sum,value.var="numIndivs");
  dfrA$numIndivs<-dfrN$numIndivs;
  dfrA$ogive    <-dfrA$mature/(dfrA$immature+dfrA$mature);
  dfrA<-dfrA[dfrA$numIndivs>0,];
  dfrA$SEX<-"MALE";
  dfrA<-dfrA[,c("YEAR","STRATUM","SEX","SIZE","numIndivs","ogive")]

    if (export){
        ##save as csv file
        if (!is.null(out.dir)){
            if (verbosity>1) cat("\nTesting existence of folder '",out.dir,"'\n",sep='')
            if (!file.exists(out.dir)){
                if (verbosity>0) cat("Creating folder '",out.dir,"' for output.\n",sep='')
                dir.create(out.dir);
            } else {
                if (verbosity>0) cat("Using folder '",out.dir,"' for output.\n",sep='')
            }
            out.csv<-file.path(out.dir,out.csv)
        }
        write.csv(dfr,out.csv,na='',row.names=FALSE);
    }

    if (showPlot){
        ##plot maturity ogives
        plotMaturityOgives(dfrA,showPlot=TRUE,...);
    }

  if (verbosity>0) cat("finished getMaturityOgives.Males().\n");
  return(invisible(dfrA));
}
