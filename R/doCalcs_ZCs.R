#'
#'@title Function to calculate size compositions from station, haul and individual crab info.
#'
#' @description Function to calculate size compositions from station, haul and individual crab info.
#'
#' @param tbl_strata  : dataframe with survey strata info (output from \code{\link{selectStrata.TrawlSurvey}})
#' @param tbl_hauls   : dataframe with hauls info (output from \code{\link{selectHauls.TrawlSurvey}})
#' @param tbl_indivs  : dataframe with individual crab info (output from \code{\link{selectIndivs.TrawlSurvey}})
#' @param calcByEW166: TRUE (calculate size compositions by EW166 prior to calculating total for EBS)
#' @param aggBySex            - flag to agregate CPUE over sexes
#' @param aggByMaturity       - flag to aggregate of maturity states
#' @param aggByShellCondition - flag to aggregate over shell conditions
#' @param cutpts : seq(from=0,to=185,by=5)
#' @param truncate.low : TRUE
#' @param truncate.high : FALSE
#' @param dropLevels - NULL, or list (by factor name) of vectors of factor values to drop (see help for \code{wtsUtilities::dropLevels})
#' @param verbosity : integer flag indicating level of printed output (0=off,1=minimal,2=full)
#'
#'@return   a list with the following elements: \cr
#'\itemize{
#' \item {EBS}        {- data frame of size compositions by year, sex, maturity state, and shell condition}
#' \item {EW166}      {- data frame of size compositions by EW166, year, sex, maturity state, and shell condition}
#' \item {byStratum}  {- data frame of size compositions by survey stratum, year, sex, maturity state, and shell condition}
#'}
#'
#'@details
#'Notes: \itemize{
#'   \item abundance is in 10^6 individuals
#'   }
#'
#' @export
#'
doCalcs_ZCs<-function(tbl_strata,
                      tbl_hauls,
                      tbl_indivs,
                      calcByEW166=TRUE,
                      aggBySex=FALSE,
                      aggByMaturity=FALSE,
                      aggByShellCondition=FALSE,
                      cutpts=seq(from=0,to=185,by=5),
                      truncate.low=TRUE,
                      truncate.high=FALSE,
                      dropLevels=list(SEX=c('MISSING',"HERMAPHRODITIC")),
                      verbosity=0){

    if (verbosity>1) cat("starting doCalcs_ZCs.\n");

  ##calculate size comps by stratum for all individuals
  dfrZCs.ByS<-calcSizeComps.ByStratum(tbl_strata,
                                      tbl_hauls=tbl_hauls,
                                      tbl_indivs=tbl_indivs,
                                      avgHaulsByStation=TRUE,
                                      bySex=!aggBySex,
                                      byMaturity=!aggByMaturity,
                                      byShellCondition=!aggByShellCondition,
                                      cutpts=cutpts,
                                      truncate.low=truncate.low,
                                      truncate.high=truncate.high,
                                      export=FALSE,
                                      verbosity=verbosity);

    #--drop levels
    if (!is.null(dropLevels)){
      message(paste0("dropping levels"));
      dfrZCs.ByS<-wtsUtilities::dropLevels(dfrZCs.ByS,
                                              dropLevels=dropLevels);
    }

    ##recode if aggregated
    if (aggBySex)            dfrZCs.ByS$SEX            <-"ALL";
    if (aggByMaturity)       dfrZCs.ByS$MATURITY       <-"ALL";
    if (aggByShellCondition) dfrZCs.ByS$SHELL_CONDITION<-"ALL";

  dfrZCs.EW166<-NULL;
  if (calcByEW166){
    dfrZCs.EW166<-calcSizeComps.EW166(dfrZCs.ByS,
                                      export=FALSE,
                                      verbosity=verbosity);
    dfrZCs.EBS<-calcSizeComps.EBS(dfrZCs.EW166,
                                        export=FALSE,
                                        verbosity=verbosity);
  } else {
    dfrZCs.EBS<-calcSizeComps.EBS(dfrZCs.ByS,
                                        export=FALSE,
                                        verbosity=verbosity);
  }

  return(list(EBS=dfrZCs.EBS,EW166=dfrZCs.EW166,byStratum=dfrZCs.ByS))
}
