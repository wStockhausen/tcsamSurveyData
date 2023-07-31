#'
#' @title Calculate statistics on resampled size comps
#'
#' @description Function to calculate statistics on resampled size comps
#'
#' @param dfrRZCs - dataframe with resampled size comps from [resampledSizeComps.calc]
#'
#' @return dataframe with statistics from resampled size comps.
#'
#' @details Mean, median, and 80% confidence intervals are calculated by year,
#' stratum, sex, maturity state, and shell condition. Stats are calculated on
#' total abundance and normalized size comps (the latter preceded by "nrm_") in
#' the output dataframe.
#'
#' @import dplyr
#' @import magrittr
#' @importFrom stats median quantile
#' @importFrom wtsUtilities Sum
#'
#' @export
#'
resampledSizeComps.CalcStats<-function(dfrRZCs){
  tmp0 = dfrRZCs %>%
           dplyr::filter(i==0) %>%
           dplyr::mutate(original=totABUNDANCE) %>%
           dplyr::select(!c(i,STRATUM_AREA,totABUNDANCE,totBIOMASS)) %>%
           dplyr::group_by(YEAR,STRATUM,SEX,MATURITY,SHELL_CONDITION) %>%
           dplyr::mutate(nrm_original=original/wtsUtilities::Sum(original)) %>%
           dplyr::ungroup();
  tmp1 = dfrRZCs %>%
           dplyr::filter(i!=0)  %>%
           dplyr::group_by(i,YEAR,STRATUM,SEX,MATURITY,SHELL_CONDITION) %>%
           dplyr::mutate(nrm=totABUNDANCE/wtsUtilities::Sum(totABUNDANCE)) %>%
           dplyr::ungroup() %>%
           dplyr::group_by(YEAR,STRATUM,SEX,MATURITY,SHELL_CONDITION,SIZE) %>%
           dplyr::summarize(mn=mean(totABUNDANCE,na.rm=TRUE),
                            md=median(totABUNDANCE,na.rm=TRUE),
                            lci=quantile(totABUNDANCE,probs=0.1,na.rm=TRUE,names=FALSE),
                            uci=quantile(totABUNDANCE,probs=0.9,na.rm=TRUE,names=FALSE),
                            nrm_mn=mean(nrm,na.rm=TRUE),
                            nrm_md=median(nrm,na.rm=TRUE),
                            nrm_lci=quantile(nrm,probs=0.1,na.rm=TRUE,names=FALSE),
                            nrm_uci=quantile(nrm,probs=0.9,na.rm=TRUE,names=FALSE)) %>%
           dplyr::ungroup();
  tmp2 = tmp1 %>% dplyr::inner_join(tmp0,by=c('YEAR','STRATUM','SEX','MATURITY','SHELL_CONDITION','SIZE'));
  return(tmp2);
}
