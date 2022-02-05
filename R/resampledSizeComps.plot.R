#'
#' @title Plot resampled size compositions
#'
#' @description Function to plot resampled size compositions.
#'
#' @param dfr - dataframe with resampled size compositions
#' @param scales - 'scales' parameter passed to facet_grid ("fixed" or "free_y")
#'
#' @details dfr should be an output from \code{\link{resampledSizeComps.calc}}.
#'
#' @export
#'
resampledSizeComps.plot<-function(dfr,
                                  scales="free_y"){
  #convert to lower case and replace "_"'s with spaces
  dfr$SEX            <-tolower(dfr$SEX);
  dfr$MATURITY       <-tolower(dfr$MATURITY);
  dfr$SHELL_CONDITION<-tolower(dfr$SHELL_CONDITION);
  dfr$SHELL_CONDITION<-gsub("_"," ",dfr$SHELL_CONDITION,fixed=TRUE);

  #extract observed size comps
  dfr0<-dfr[dfr$i==0,];
  #add maturity/shell condition index
  dfr0$MS<-paste0(dfr0$MATURITY,"\n",dfr0$SHELL_CONDITION);
  #get original sample sizes
  origN<-reshape2::dcast(dfr0,YEAR+STRATUM+SEX+MATURITY+SHELL_CONDITION~.,fun.aggregate=sum,value.var="numIndivs");
  names(origN)[6]<-"numIndivs";
  tempH<-reshape2::dcast(dfr0,YEAR+STRATUM+SEX+MATURITY+SHELL_CONDITION~.,fun.aggregate=mean,value.var="numNonZeroHauls");
  origN$numNonZeroHauls<-tempH[[6]];

  #calculate mean size comps
  dfr<-dfr[dfr$i>0,];
  mnN<-reshape2::dcast(dfr,YEAR+STRATUM+SEX+SHELL_CONDITION+MATURITY+SIZE~.,fun.aggregate=mean,value.var="totABUNDANCE");
  names(mnN)[7]<-"mean";
  #calculate std dev by size for comps
  sdN<-reshape2::dcast(dfr,YEAR+STRATUM+SEX+SHELL_CONDITION+MATURITY+SIZE~.,fun.aggregate=sd,value.var="totABUNDANCE");
  #combine mean, std devs
  dfrStats<-cbind(mnN,stdev=sdN[["."]],ymin=mnN$mean-sdN[["."]],ymax=mnN$mean+sdN[["."]]);
  #add maturity/shell condition index
  dfrStats$MS<-paste0(dfrStats$MATURITY,"\n",dfrStats$SHELL_CONDITION);

  #create plot list by year for size compositions
  ps<-list();
  uY<-sort(unique(dfrStats$YEAR));
  uS<-sort(unique(dfrStats$STRATUM));
  for (s in uS){
    for (y in uY){
      pdfrStats<-dfrStats[(dfrStats$YEAR==y)&(dfrStats$STRATUM==s),];
      pdfr0    <-dfr0[(dfr0$YEAR==y)&(dfr0$STRATUM==s),];
      p <- ggplot2::ggplot(data=pdfrStats,mapping=ggplot2::aes_string(x="SIZE",y="mean",colour="SEX"));
      p <- p + ggplot2::geom_ribbon(mapping=ggplot2::aes_string(ymin="ymin",ymax="ymax",fill="SEX"),alpha=0.5);
      p <- p + ggplot2::geom_line();
      p <- p + ggplot2::geom_line(data=pdfr0,mapping=ggplot2::aes_string(x="SIZE",y="totABUNDANCE",colour="SEX"),linetype=2)
      p <- p + ggplot2::facet_grid(MS~YEAR+STRATUM,scales=scales);
      p <- p + ggplot2::labs(x="size (mm CW)",y="abundance (millions)");
      ps[[paste0("Figure &&figno. Original (dotted line) and resampled size compositions (envelope, mean +/- 1 std. dev.) for survey year ",
                 y," and stratum ",s," by indicated sex, maturity state and shell condition.")]]<-p;
    }#s
  }#y


  return(ps);
}

#resampledSizeComps.plot(dfr);
