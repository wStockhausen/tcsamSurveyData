#'
#' @title Function to extract female maturity ogives from a size composition dataframe
#'
#' @description Function to extract female maturity ogives from a size composition dataframe.
#'
#' @param dfrZCs : dataframe with new shell female size compositions by maturity state
#' @param showPlot - flag to show plot of ogives
#' @param export  - boolean flag to export results to csv file
#' @param out.csv - name of output csv file                    (ignored if NULL)
#' @param out.dir - base path for output csv file              (set to folder of input csv file or current working directory)
#' @param verbosity - flags for intermediate output
#' @param ... - parameters passed to plotMaturityOgives(...)
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
#' @details the input size composition dataframe should be in the output format of the calcSizeComps... functions.\cr
#'
#' @export
#'
getMaturityOgives.Females<-function(
                           dfrZCs=NULL,
                           showPlot=TRUE,
                           export=FALSE,
                           out.csv="MaturityOgives.Females.csv",
                           out.dir=NULL,
                           verbosity=0,
                           ...){
  if (verbosity>0) cat("starting getMaturityOgives.Females().\n");

  #make sure this dfrZCs is only new shell female crab
  idx<-(dfrZCs$SEX=="FEMALE")&(dfrZCs$SHELL_CONDITION=="NEW_SHELL")&(dfrZCs$MATURITY %in%  c("IMMATURE","MATURE"));
  dfrZCs<-dfrZCs[idx,];

  dfrN<-reshape2::dcast(dfrZCs,"YEAR+STRATUM+SIZE~.",fun.aggregate=wtsUtilities::Sum,value.var="numIndivs");
  names(dfrN)[4]<-"numIndivs";
  dfrA<-reshape2::dcast(dfrZCs,"YEAR+STRATUM+SIZE~MATURITY",fun.aggregate=wtsUtilities::Sum,value.var="totABUNDANCE");
  dfrA$numIndivs<-dfrN$numIndivs;
  dfrA$ogive    <-dfrA$MATURE/(dfrA$IMMATURE+dfrA$MATURE);
  dfrA<-dfrA[dfrA$numIndivs>0,];
  dfrA$SEX<-"FEMALE";
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
        plotMaturityOgives(dfrA,showPlot=TRUE,...)
    }

  if (verbosity>0) cat("finished getMaturityOgives.Females().\n");
  return(invisible(dfrA));
}
