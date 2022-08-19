#'
#' @title Combine effective Ns from resampled size compositions with original Ns in dataframe
#'
#' @description Function to combine effective Ns from resampled size compositions, together
#' with the original Ns that would have been used as input sample sizes
#'
#' @param dfrEffNs - dataframe with effective Ns from resampled size compositions (output from [resampledSizeComps.calcEffNs])
#' @param dfrOrgNs - dataframe with original Ns, for comparison
#' @param frmla - formula for summing over factors (default: "stratum+year")
#'
#' @return a "melted" dataframe with values for default sample sizes (default N's),
#' the mean effective N's, the harmonic mean effective N's, the number of crab sampled, and
#' the number of non-zero stations in the "N" column, identified by the value in the "type" column.
#'
#' @details dfrEffNs should be a dataframe output from [resampledSizeComps.calcEffNs].
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
#' Output dataframe will have columns
#' \itemize{
#'    \item{stratum- unless stratum is not included in the formula}
#'    \item{year - unless year is not included in the formula}
#'    \item{sex}
#'    \item{maturity}
#'    \item{shell_condition}
#'    \item{type - type of sample size ("default N", "avg(N)","har(N)","num. crab","num. non-0 hauls")}
#'    \item{N - value}
#'}
#'
#' @import dplyr
#' @import reshape2
#'
#' @export
#'
resampledSizeComps.combineWithDefault<-function(dfrEffNs,
                                                dfrOrgNs,
                                                frmla="stratum+year~."){
  orgNs = reshape2::dcast(dfrOrgNs,frmla,fun.aggregate=sum,value.var="relSS");
  tmp1  = reshape2::melt(orgNs,measure.vars=".",variable.name="type",value.name="N") %>%
            dplyr::mutate(type="default N");
  if (!("sex"             %in% names(tmp1))) tmp1$sex             = "all";
  if (!("maturity"        %in% names(tmp1))) tmp1$maturity        = "all";
  if (!("shell_condition" %in% names(tmp1))) tmp1$shell_condition = "all";
  tmp1 %<>% dplyr::mutate(sex             = ifelse(sex=="undetermined","all",sex),
                          maturity        = ifelse(maturity=="undetermined","all",maturity),
                          shell_condition = ifelse(shell_condition=="undetermined","all",shell_condition));

  #--plot results
  ps<-list();
  measure.vars<-c("avg(N)","har(N)","num. crab","num. non-0 hauls");
  tmp2<-reshape2::melt(dfrEffNs,measure.vars=measure.vars,variable.name="type",value.name="N");
  tmp = dplyr::bind_rows(tmp1,tmp2) %>%
          dplyr::mutate(type=factor(type,levels=c("default N",measure.vars)));
  return(tmp);
}

