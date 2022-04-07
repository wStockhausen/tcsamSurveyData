#'
#' @title Create plots for resampled size comps statistics by year
#'
#' @description Function to create plots for resampled size comps statistics by year.
#'
#' @param dfrStats - dataframe with "stacked" results from [resampledSizeComps.CalcStats]
#' @param nrow - number of rows per plot page
#' @param ncol - number of columns per plot page
#' @param plotNorm - flgg (T/F) to plot normalized (vs. un-normalized) size comps
#' @param showCIs - flag (T/F) to plot confidence intervals (as ribbons)
#' @param showPlots - flag (T/F) to
#' @param savePlots - flag (T/F) to
#' @param fn - base filename (with path to output folder; sex, maturity, shell condition and page number will be appended)
#' @param ext - extension for output plot files
#' @param width - page width (in inches)
#' @param height - page height (in inches)
#' @param dpi - dots-per-inch
#'
#' @return list of plots
#'
#' @details This function makes plots for a single "scenario" of resampled size comps, showing
#' mean and median, in addition to original and (optionally) CIs. Use
#' [resampledSizeComps.CompareStats] to compare results from multiple scenarios.
#'
#' @import dplyr
#' @import ggplot2
#' @import magrittr
#' @import rlang
#' @import wtsUtilities
#'
#' @export
#'
resampledSizeComps.PlotStats<-function(dfrStats,
                                       plotNorm=TRUE,
                                       nrow=5,
                                       ncol=5,
                                       showPlots=FALSE,
                                       savePlots=TRUE,
                                       fn="fig_Stats",
                                       ext="png",
                                       width=6,
                                       height=6,
                                       dpi=200){
  tmp0 = dfrStats;
  yrs = sort(unique(tmp0$YEAR));
  nYrs = length(yrs);
  dfrU_XMS = tmp0 %>% dplyr::distinct(SEX,MATURITY,SHELL_CONDITION);

  nrm = ifelse(plotNorm,"nrm_","");
  org = rlang::sym(paste0(nrm,"original"));
  mn  = rlang::sym(paste0(nrm,"mn"));
  md  = rlang::sym(paste0(nrm,"md"));
  lci = rlang::sym(paste0(nrm,"lci"));
  uci = rlang::sym(paste0(nrm,"uci"));

  nppg = nrow*ncol;
  npg  = ceiling(nYrs/nppg);
  plots = list();
  for (rw in 1:nrow(dfrU_XMS)){
    #--testing: rw = 1;
    dfrU_XMSp = dfrU_XMS[rw,];
    tmp3 = tmp0 %>% dplyr::inner_join(dfrU_XMSp,by=c('SEX','MATURITY','SHELL_CONDITION'))
    for (pg in 1:npg){
      #--testing: pg = 1;
      yrsp = yrs[valid_indices((pg-1)*nppg + 1:nppg,nYrs)];
      tmp4 = tmp3 %>% dplyr::filter(YEAR %in% yrsp);
      p = ggplot(tmp4,aes(x=SIZE)) +
            geom_ribbon(aes(ymin=!!lci,ymax=!!uci),colour="light green",fill="light green") +
            geom_line(aes(y=!!md),colour="light blue") +
            geom_line(aes(y=!!mn),colour="dark blue") +
            geom_line(aes(y=!!org)) +
            facet_wrap(~YEAR,nrow=nrow) +
            theme(panel.background=element_rect(colour="black",fill="white"));
      if (showPlots) print(p);
      str = paste(dfrU_XMSp$SEX,dfrU_XMSp$MATURITY,dfrU_XMSp$SHELL_CONDITION,sep="_");
      if (savePlots) ggsave(filename=paste(fn,str,paste0("pg_",pg),ext,sep="."),plot=p,width=width,height=height,dpi=dpi)
      plots[[paste0(str,"_",pg)]] = p;
    }
  }
  return(plots);
}
