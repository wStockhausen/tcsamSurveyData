#'
#' @title Function to extract chela height data on males w/ selected characteristics from NMFS trawl survey csv files.
#' 
#' @param tbl_hauls : hauls table (dataframe) from call to selectHauls.TrawlSurvey(...) [required]
#' @param tbl       : table (dataframe) of survey data (or filename of survey csv file, or NULL)
#' @param export    : boolean flag to export results to csv file
#' @param shell_condition: one of 'NEW_SHELL','OLD_SHELL' or 'ALL' for narrowing selection of individuals
#' @param minWidth : minimum size (width) of individuals to select 
#' @param maxWidth : maximum size (width) of individuals to select 
#' @param verbosity: integer flag indicating level of printed output (0=off,1=minimal,2=full)
#'
#' @return dataframe
#' 
#' @details If tbl is NULL, the user will be prompted for a survey csv file via a file dialog box.\cr
#'\itemize{\item Weights are in grams.}
#' 
#' @import sqldf
#' @import tcsamFunctions
#' @importFrom wtsUtilities addFilter
#' 
#' @export
#' 
getChelaHeights<-function(tbl_hauls,
                           tbl=NULL,
                           col.Size='WIDTH',
                           export=FALSE,
                           out.csv="ChelaHeights.csv",
                           out.dir=NULL,
                           shell_condition=c('NEW_SHELL','OLD_SHELL','ALL'),
                           minSize=-Inf,
                           maxSize=Inf,
                           verbosity=1){
    if (verbosity>1) cat("starting getChelaHeights.\n");
    
    dfr<-selectIndivs.TrawlSurvey(tbl_hauls,
                                   tbl=tbl,
                                   col.Size=col.size,
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
    return(invisible(dfr));
}