#'
#' @title Compare effective N's from several resampled size compositions "scenarios"
#'
#' @description Function to compare effective N's from several resampled size compositions "scenarios".
#'
#' @param dfrEffNs - a dataframe with "stacked" scenarios of effective Ns from [resampledSizeComps.calcEffN]
#' @param byFacs - vector of column names for factors other than YEAR and STRATUM
#' @param nDefault - default sample size (for plotting)
#' @param nStations - typical number of survey stations (for plotting)
#'
#' @return 2-element list of plots (with and without number of crab sampled)
#'
#' @details dfrEffNs should "stacked" dataframe of outputs from [resampledSizeComps.calc],
#' with values of an additional column, \code{type}, that indicates the scenario each row
#' is associated with.
#'
#' @import ggplot2
#'
#' @export
#'
resampledSizeComps.CompareEffNs<-function(dfrEffNs,
                                          byFacs="",
                                          nDefault=200,
                                          nStations=375){
  #--define factor-related stuff
  nFacs<-0;
  frmlaFacs<-"";
  if (any(byFacs!="")) {
    nFacs<-length(byFacs);
    frmlaFacs<-paste0("+",paste(byFacs,collapse="+"));
  }

  ps<-list();
  measure.vars<-c("avg(N)","har(N)","num. crab","num. non-0 hauls");
  tmp<-reshape2::melt(dfrEffNs,measure.vars=measure.vars,variable.name="variable",value.name="N");
  frmla<-".";
  if (nFacs>0) frmla<-tolower(paste0(byFacs,collapse="+"));
  frmla<-paste0(frmla,"~stratum")
  p <- ggplot(tmp,aes_string(x="year",y="N",colour="variable",linetype="type",shape="type")) +
        geom_hline(yintercept=nDefault,linetype=2) +
        geom_hline(yintercept=nStations,linetype=3) +
        geom_line() +
        geom_point() +
        facet_grid(rows=as.formula(frmla)) +
        scale_y_continuous(limits=c(0,NA)) +
        labs(x="year",y="effective N") +
        theme(panel.background=element_rect(colour="black",fill="white"),
              panel.border=element_rect(colour="black",fill=NA));
  ps[[paste0("Figure &&figno. Effective N's (mean and harmonic mean), number of crab measured, and number of non-zero hauls.",
             "Default N (black dotted line) and number of stations sampled (black dashed line).")]]<-p;

  p <- p %+% tmp[tmp$variable!="num. crab",];
  ps[[paste0("Figure &&figno. Effective N's (mean and harmonic mean) ",
             "and number of non-zero hauls. Default N (black dotted line) ",
             "and number of stations sampled (black dashed line).")]]<-p;

  return(ps);
}

#lst=resampledSizeComps.calcEffN(dfrRZCs,byFacs="SEX");
