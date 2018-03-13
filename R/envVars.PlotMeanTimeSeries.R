#'
#' @title Plot time series of mean environmental variable
#'
#' @description Function to plot time series of mean environmental variable
#'
#' @param dfr - dataframe from call to \code{\link{calcEnvData.ByStation}}
#' @param yearCol - dataframe column name for year
#' @param envVarCol - dataframe column name for the environmental variable
#' @param xlab - x axis label
#' @param ylab - y axis label
#' @param showPlot - flag (T/F) to show plot
#' @param verbose - flag (T/F) to print debugging info
#'
#' @return ggplot2 object
#'
#' @details uses \code{\link{wtsUtilities::plotMDFR.XY}} and the \code{reshape2} package.
#'
#' @export
#'
envVars.PlotMeanTimeSeries<-function(dfr,
                                     yearCol="YEAR",
                                     envVarCol="BOTTOM_TEMP",
                                     xlab="year",
                                     ylab="Bottom Temperature (degC)",
                                     showPlot=FALSE,
                                     verbose=FALSE){
    if (verbose) cat("Starting envVars.PlotMeanTimeSeries()\n");

    idx<-!is.na(dfr[[envVarCol]]);
    dfr<-dfr[idx,];
    dfrp<-reshape2::dcast(data=dfr,formula=paste0(yearCol,"~."),fun.aggregate=mean,value.var=envVarCol);
    if (verbose) cat("names: ",names(dfrp),"\n");
    names(dfrp)[2]<-"val";
    if (verbose) cat("names: ",names(dfrp),"\n");

    p <- wtsPlots::plotMDFR.XY(dfrp,x=yearCol,value.var="val",xlab=xlab,ylab=ylab);
    if (showPlot) print(p);
    if (verbose) cat("Finished envVars.PlotMeanTimeSeries()\n");
    return(p)
}
