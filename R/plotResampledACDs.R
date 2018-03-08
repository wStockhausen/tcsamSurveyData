#'
#' @title Plot resampled aggregated catch data
#'
#' @description Function to plot resampled aggregated catch data.
#'
#' @param dfr - dataframe with resampled aggregated catch data
#' @param col - column to plot
#' @param scales - 'scales' parameter passed to facet_grid ("fixed" or "free_y")
#'
#' @details dfr should be an output from \code{\link{calcResampledACDs}}.
#'
#' @export
#'
plotResampledACDs<-function(dfr,
                            col=c("totBIOMASS","totABUNDANCE"),
                             scales="free_y"){
  #calculate mean size comps
  mnN<-reshape2::dcast(dfr,YEAR+STRATUM+SEX+SHELL_CONDITION+MATURITY~.,fun.aggregate=mean,value.var=col);
  names(mnN)[7]<-"mean";
  #calculate std dev by size for comps
  sdN<-reshape2::dcast(dfr,YEAR+STRATUM+SEX+SHELL_CONDITION+MATURITY~.,fun.aggregate=sd,value.var=col);
  #combine mean, std devs and drop unnecessary levels
  dfrStats<-cbind(mnN,stdev=sdN[["."]],ymin=mnN$mean-sdN[["."]],ymax=mnN$mean+sdN[["."]]);
  dfrStats<-dropLevels(dfrStats,
                       dropLevels=list(SEX=c("HERMAPHRODITE","HERMAPHRODITIC","UNDETERMINED"),
                                       MATURITY="UNDETERMINED"))
  #convert to lower case and replace "_"'s with spaces
  dfrStats$SEX<-tolower(dfrStats$SEX);
  dfrStats$MATURITY<-tolower(dfrStats$MATURITY);
  dfrStats$SHELL_CONDITION<-tolower(dfrStats$SHELL_CONDITION);
  dfrStats$SHELL_CONDITION<-gsub("_"," ",dfrStats$SHELL_CONDITION,fixed=TRUE);

  #create plot list by year for size compositions
  ps<-list();
  uY<-sort(unique(dfrStats$YEAR));
  for (y in uY){
    pdfrStats<-dfrStats[dfrStats$YEAR==y,];
    p <- ggplot2::ggplot(data=pdfrStats,mapping=aes_string(x="SIZE",y="mean",colour="SEX"));
    p <- p + ggplot2::geom_ribbon(mapping=aes_string(ymin="ymin",ymax="ymax",fill="SEX"),alpha=0.5);
    p <- p + ggplot2::geom_line();
    p <- p + ggplot2::facet_grid(MATURITY+SHELL_CONDITION~YEAR+STRATUM,scales=scales);
    p <- p + ggplot2::labs(x="size (mm CW)",y="abundance (millions)");
    ps[[paste0("Figure &&figno. Resampled size compositions for survey year ",y)]]<-p;
  }

  return(list(plots=ps));
}

#plotResampledACDs(dfr);
