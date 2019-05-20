#'
#'@title Function to calculate sample sizes at various levels of spatial aggregation from station, haul and individual crab info.
#'
#'@description Function to calculate sample sizes from station, haul and individual crab info.
#'
#' @param tbl_strata  - dataframe with survey strata info (output from \code{\link{selectStrata.TrawlSurvey}})
#' @param tbl_hauls   - dataframe with hauls info (output from \code{\link{selectHauls.TrawlSurvey}})
#' @param tbl_indivs  - dataframe with individual crab info (output from \code{\link{selectIndivs.TrawlSurvey}})
#' @param averageHaulsByStation - TRUE (average hauls by station prior to calculating stratum-level quantities)
#' @param aggBySex            - flag to agregate CPUE over sexes
#' @param aggByMaturity       - flag to aggregate of maturity states
#' @param aggByShellCondition - flag to aggregate over shell conditions
#' @param dropLevels - NULL, or list (by factor name) of vectors of factor values to drop (see help for \code{wtsUtilities::dropLevels})
#' @param colName - column name with sample sizes
#' @param totVal - total SS for relative SS calculations
#' @param verbosity - integer flag indicating level of printed output (0=off,1=minimal,2=full)
#'
#'@return   a list with elements \code{ss.EBS} and \code{relSS.EBS}. \code{ss.EBS} is a dataframe with columns: \cr
#'\itemize{
#' \item {year}     {- survey year}
#' \item {ssTot}    {- total sample size}
#'} \cr
#' \code{relSS.EBS} is a dataframe with the following columns: \cr
#'\itemize{
#' \item {YEAR}             {- }
#' \item {SEX}              {- }
#' \item {MATURITY}         {- }
#' \item {SHELL_CONDITION}  {- }
#' \item {relSS}            {relative sample size}
#'}
#'
#'@details None.
#'
#' @export
#'
doCalcs_SSs<-function(tbl_strata,
                      tbl_hauls,
                      tbl_indivs,
                      averageHaulsByStation=TRUE,
                      aggBySex=FALSE,
                      aggByMaturity=FALSE,
                      aggByShellCondition=FALSE,
                      dropLevels=list(SEX=c("MISSING","HERMAPHRODITIC")),
                      colName="numIndivs",
                      totVal=200,
                      verbosity=verbosity){

  #--calculate aggregated catch data (ACD) quantities as required
  lstABs<-doCalcs_ABs(tbl_strata,
                      tbl_hauls,
                      tbl_indivs,
                      averageHaulsByStation=averageHaulsByStation,
                      aggBySex=aggBySex,
                      aggByMaturity=aggByMaturity,
                      aggByShellCondition=aggByShellCondition,
                      aggBySize=TRUE,
                      dropLevels=list(SEX=c("MISSING","HERMAPHRODITIC")),
                      verbosity=verbosity);

  #--calculate sample sizes
  lstSSs.ByStratum<-calcSSs(lstABs$ABs[["byStratum"]],colName=colName,totVal=totVal);
  lstSSs.EW166    <-calcSSs(lstABs$ABs[["EW166"]],    colName=colName,totVal=totVal);
  lstSSs.EBS      <-calcSSs(lstABs$ABs[["EBS"]],      colName=colName,totVal=totVal);


  return(list(totSS=list(byStratum=lstSSs.ByStratum$totSS,EW166=lstSSs.EW166$totSS,EBS=lstSSs.EBS$totSS),
              relSS=list(byStratum=lstSSs.ByStratum$relSS,EW166=lstSSs.EW166$relSS,EBS=lstSSs.EBS$relSS)));
}
