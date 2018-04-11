#'
#' @title Plot resampled size compositions
#'
#' @description Function to plot resampled size compositions.
#'
#' @param dfr - dataframe with resampled size compositions
#' @param scales - 'scales' parameter passed to facet_grid ("fixed" or "free_y")
#'
#' @details dfr should be an output from \code{\link{calcResampledSizeComps}}.
#'
#' @export
#'
plotResampledSizeComps<-function(dfr,
                                 scales="free_y"){
  #calculate mean size comps
  mnN<-reshape2::dcast(dfr,YEAR+STRATUM+SEX+SHELL_CONDITION+MATURITY+SIZE~.,fun.aggregate=mean,value.var="totABUNDANCE");
  names(mnN)[7]<-"mean";
  #calculate std dev by size for comps
  sdN<-reshape2::dcast(dfr,YEAR+STRATUM+SEX+SHELL_CONDITION+MATURITY+SIZE~.,fun.aggregate=sd,value.var="totABUNDANCE");
  #combine mean, std devs and drop unnecessary levels
  dfrStats<-cbind(mnN,stdev=sdN[["."]],ymin=mnN$mean-sdN[["."]],ymax=mnN$mean+sdN[["."]]);
  dfrStats<-wtsUtilities::dropLevels(
                            dfrStats,
                            dropLevels=list(SEX=c("HERMAPHRODITE","HERMAPHRODITIC","UNDETERMINED"),
                                            MATURITY="UNDETERMINED")
                          );
  dfrStats<-wtsUtilities::dropLevels(
                            dfrStats,
                            dropLevels=list(SHELL_CONDITION=c("OLD_SHELL"),
                                            MATURITY="IMMATURE");
                          );
  #convert to lower case and replace "_"'s with spaces
  dfrStats$SEX<-tolower(dfrStats$SEX);
  dfrStats$MATURITY<-tolower(dfrStats$MATURITY);
  dfrStats$SHELL_CONDITION<-tolower(dfrStats$SHELL_CONDITION);
  dfrStats$SHELL_CONDITION<-gsub("_"," ",dfrStats$SHELL_CONDITION,fixed=TRUE);
  dfrStats$MS<-paste0(dfrStats$MATURITY,", ",dfrStats$MATURITY);

  #create plot list by year for size compositions
  ps<-list();
  uY<-sort(unique(dfrStats$YEAR));
  for (y in uY){
    pdfrStats<-dfrStats[dfrStats$YEAR==y,];
    p <- ggplot2::ggplot(data=pdfrStats,mapping=aes_string(x="SIZE",y="mean",colour="SEX"));
    p <- p + ggplot2::geom_ribbon(mapping=aes_string(ymin="ymin",ymax="ymax",fill="SEX"),alpha=0.5);
    p <- p + ggplot2::geom_line();
    p <- p + ggplot2::facet_grid(MS~YEAR+STRATUM,scales=scales);
    p <- p + ggplot2::labs(x="size (mm CW)",y="abundance (millions)");
    ps[[paste0("Figure &&figno. Resampled size compositions for survey year ",y)]]<-p;
  }

  #normalize size compositions across sex, maturity and shell condition
  pdfrStats<-dplyr::add_count(dplyr::as.tbl(dfrStats),
                              YEAR,STRATUM,wt=mean);
  names(pdfrStats)[length(names(pdfrStats))]<-"totN";
  pdfrStats$p <- pdfrStats$mean/pdfrStats$totN;

  #calculate effective N
  dfrEffN<-NULL;
  for (y in sort(unique(pdfrStats$YEAR))){
    for (s in sort(unique(pdfrStats$STRATUM))){
      idx<-(pdfrStats$YEAR==y)&(pdfrStats$STRATUM==s);
      p <- pdfrStats$p[idx];
      v <- pdfrStats$stdev[idx]^2;
      effN <- sum(v)/sum(p*(1-p));
      dfrEffN<-rbind(dfrEffN,data.frame(year=y,stratum=s,effective=effN,original=mean(pdfrStats$totN)))
    }
  }
  tmp<-reshape2::melt(dfrEffN,measure.vars=c("effective","original"),variable.name="type",value.name="N")
  p <- ggplot2::ggplot(tmp,aes_string(x="year",y="N",colour="stratum",linetype="type",shape="type"));
  p <- p + ggplot2::geom_line();
  p <- p + ggplot2::geom_point();
  p <- p + ggplot2::labs(x="year",y="N");
  ps[[paste0("Figure &&figno. Original N and effective N.")]]<-p;

  return(list(plots=ps,effN=dfrEffN));
}

#plotResampledSizeComps(dfr);
