#'
#' @title Function to extract chela height data on males w/ selected characteristics from NMFS trawl survey csv files.
#' 
#' @description Function to extract chela height data on males w/ selected characteristics from NMFS trawl survey csv files.
#' 
#' @param tbl_hauls - hauls table (dataframe) from call to selectHauls.TrawlSurvey(...) [required]
#' @param tbl       - table (dataframe) of survey data (or filename of survey csv file, or NULL)
#' @param shell_condition - one of 'NEW_SHELL','OLD_SHELL' or 'ALL' for narrowing selection of individuals
#' @param minSize - minimum size (width) of individuals to select 
#' @param maxSize - maximum size (width) of individuals to select 
#' @param export  - boolean flag to export results to csv file
#' @param out.csv - name of output csv file                    (ignored if NULL)
#' @param out.dir - base path for output csv file              (set to folder of input csv file or current working directory)
#' @param verbosity - flags for intermediate output
#' @param ...: parameters passed to plotChelaHeights(...)
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
                           verbosity=1,
                           ...){
    if (verbosity>1) cat("starting getChelaHeights.\n");
    
    dfr<-selectIndivs.TrawlSurvey(tbl_hauls,
                                   tbl=tbl,
                                   col.Size=col.Size,
                                   export=export,
                                   out.csv=out.csv,
                                   out.dir=out.dir,
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
    dfr<-dfr[!is.na(dfr[["CHELA_HEIGHT"]]),];
    
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
    dfr<-sqldf::sqldf(qry);
    
    if (showPlot){
        plotChelaHeights(dfr,showPlot=TRUE,...)
    }
    
    return(invisible(dfr));
}

#tbl.chs<-getChelaHeights(tbl.hauls,aggYears=10)
