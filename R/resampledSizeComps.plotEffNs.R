#'
#' @title Plot effective Ns from resampled size compositions with original Ns
#'
#' @description Function to plot effective Ns from resampled size compositions, together
#' with the original Ns that would have been used as input sample sizes.
#'
#' @param dfrEffNs : dataframe with effective Ns from resampled size compositions (output from [resampledSizeComps.calcEffN])
#' @param dfrOrgNs : dataframe with original Ns, for comparison
#' @param byFacs : vector of column names for factors other than YEAR and STRATUM
#'
#' @return a list with two ggplot2 plot objects comparing time series of the default sample sizes (default N's),
#' the mean effective N's, the harmonic mean effective N's, the number of crab sampled, and
#' the number of non-zero stations., The first plot is with, and the second without,
#' the actual number of crabs measured.
#'
#' @details dfrEffNs should be a dataframe output from [resampledSizeComps.calcEffN].
#' dfrOrgNs should be a dataframe with columns
#' \itemize{
#'    \item{stratum}
#'    \item{year}
#'    \item{sex}
#'    \item{maturity}
#'    \item{shell_condition}
#'    \item{relSS - relative sample size}
#' }
#'
#' @import dplyr
#' @import ggplot2
#' @import reshape2
#'
#' @importFrom stats as.formula
#' @importFrom wtsPlots getStdTheme
#'
#' @export
#'
resampledSizeComps.plotEffNs<-function(dfrEffNs,
                                       dfrOrgNs,
                                       byFacs=""){

  #--define factor-related stuff
  nFacs<-0;
  frmlaFacs<-"";
  if (any(byFacs!="")) {
    nFacs<-length(byFacs);
    frmlaFacs<-paste0("+",paste(byFacs,collapse="+"));
  }
  frmla<-tolower(paste0("STRATUM+YEAR",frmlaFacs,"~."));
  tmp = resampledSizeComps.combineWithDefault(dfrEffNs,dfrOrgNs,frmla);

  frmla<-".";
  if (nFacs>0) frmla<-tolower(paste0(byFacs,collapse="+"));
  frmla<-paste0(frmla,"~stratum")
  p = ggplot(tmp,aes_string(x="year",y="N",colour="type",linetype="stratum",shape="stratum")) +
         geom_line() + geom_point() +
         facet_grid(rows=as.formula(frmla)) +
         scale_y_continuous(limits=c(0,NA)) +
         labs(x="year",y="effective N") +
         wtsPlots::getStdTheme();

  ps = list();
  ps[[paste0("Figure &&figno. Input N (dotted line), effective N's (arithmetic and harmonic means), number of crab measured, and number of non-zero hauls.")]]<-p;

  p <- p %+% tmp[tmp$type!="num. crab",];
  ps[[paste0("Figure &&figno. Input N (dotted line), effective N's (arithmetic and harmonic means), and number of non-zero hauls.")]]<-p;

  return(ps);
}

#lst=resampledSizeComps.calcEffN(dfrRZCs,byFacs="SEX");

