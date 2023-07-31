#'
#' @title Function to extract chela height data on males w/ selected characteristics from NMFS trawl survey csv files.
#'
#' @description Function to extract chela height data on males w/ selected characteristics from NMFS trawl survey csv files.
#'
#' @param tbl_hauls : hauls table (dataframe) from call to selectHauls.TrawlSurvey(...) \[required\]
#' @param tbl       : table (dataframe) of survey data (or filename of survey csv file, or NULL)
#' @param col.Size  : column name for size information
#' @param shell_condition : one of 'NEW_SHELL','OLD_SHELL' or 'ALL' for narrowing selection of individuals
#' @param minSize : minimum size (width) of individuals to select
#' @param maxSize : maximum size (width) of individuals to select
#' @param showPlot : flag to show plot of chela heights
#' @param export  : boolean flag to export results to csv file
#' @param out.csv : name of output csv file                    (ignored if NULL)
#' @param out.dir : base path for output csv file              (set to folder of input csv file or current working directory)
#' @param verbosity : flags for intermediate output
#' @param ... : parameters passed to plotChelaHeights(...)
#'
#' @return dataframe
#'
#' @details If tbl is NULL, the user will be prompted for a survey csv file via a file dialog box.\cr
#'\itemize{\item Weights are in grams.}
#'
#' @importFrom sqldf sqldf
#'
#' @export
#'
getChelaHeights<-function(tbl_hauls,
                           tbl=NULL,
                           col.Size='WIDTH',
                           shell_condition=c('NEW_SHELL','OLD_SHELL','ALL'),
                           minSize=-Inf,
                           maxSize=Inf,
                           showPlot=TRUE,
                           export=FALSE,
                           out.csv="ChelaHeights.csv",
                           out.dir=NULL,
                           verbosity=0,
                           ...){
    if (verbosity>1) cat("starting getChelaHeights.\n");

    dfr<-selectIndivs.TrawlSurvey(tbl_hauls,
                                   tbl=tbl,
                                   col.Size=col.Size,
                                   export=FALSE,
                                   sex='MALE',
                                   shell_condition=shell_condition,
                                   maturity='ALL',
                                   calcMaleMaturity=FALSE,
                                   minSize=minSize,
                                   maxSize=maxSize,
                                   verbosity=verbosity);
    #drop unnecessary columns
    dfr<-dfr[,c("HAULJOIN","numIndivs","SEX","SHELL_CONDITION","MATURITY","SIZE",
                "CHELA_HEIGHT","SAMPLING_FACTOR","WEIGHT","CALCULATED_WEIGHT")];
    ##drop individuals w/out chela heights
    dfr<-dfr[!is.na(dfr[["CHELA_HEIGHT"]]),];

    ##add in haul info
    qry<-"select
            h.YEAR,h.STRATUM,h.GIS_STATION as STATION,
            h.MID_LATITUDE as LATITUDE,h.MID_LONGITUDE as LONGITUDE,
            i.numIndivs,i.SEX,i.SHELL_CONDITION,i.MATURITY,i.SIZE,i.CHELA_HEIGHT,i.SAMPLING_FACTOR
          from
            tbl_hauls as h,
            dfr as i
          where
            h.hauljoin=i.hauljoin
          order by
            YEAR,STRATUM;"
    dfr<-sqldf(qry);

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
        ##plot chela height data
        plotChelaHeights(dfr,showPlot=TRUE,...)
    }

    return(invisible(dfr));
}

#tbl.chs<-getChelaHeights(tbl.hauls,aggYears=10)
