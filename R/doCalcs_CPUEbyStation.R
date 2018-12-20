#'
#' @title Calculate cpue by survey station and other factors from strata and haul files
#'
#' @description Function to calculate cpue by survey sstation and other factors (e.g., sex) from strata and haul files.
#'
#' @param tbl_strata  - dataframe with survey strata info (output from \code{\link{selectStrata.TrawlSurvey}})
#' @param tbl_hauls   - dataframe with hauls info (output from \code{\link{selectHauls.TrawlSurvey}})
#' @param tbl_indivs  - dataframe with individual crab info (output from \code{\link{selectIndivs.TrawlSurvey}})
#' @param aggBySex            - flag to agregate CPUE over sexes
#' @param aggByMaturity       - flag to aggregate of maturity states
#' @param aggByShellCondition - flag to aggregate over shell conditions
#' @param aggBySize           - flag to aggregate over sizes
#' @param cutpts        - vector of cutpoints to create size bins from
#' @param truncate.low  - flag (T/F) to exclude individuals smaller than minSize
#' @param truncate.high - flag (T/F) to exclude individuals larger than maxSize
#' @param dropLevels - NULL, or list (by factor name) of vectors of factor values to drop (see help for \code{wtsUtilities::dropLevels})
#' @param verbosity - integer flag indicating level of printed output (0=off,1=minimal,2=full)
#'
#'@details
#'The returned dataframes has the following columns: \cr
#'\itemize{
#'\item   YEAR
#'\item   STRATUM
#'\item   GIS_STATION
#'\item   LONGITUDE
#'\item   LATITUDE
#'\item   SEX
#'\item   MATURITY
#'\item   SHELL_CONDITION
#'\item   SIZE
#'\item   numHauls
#'\item   numNonZeroHauls
#'\item   numIndivs
#'\item   numCPUE
#'\item   wgtCPUE
#'}
#'\cr Other notes: \cr
#'\itemize{
#'\item   CPUE in numbers is in no/(sq. nm.)
#'\item   CPUE in weight  is in mt/(sq. nm.)
#'} \cr
#'
#'@return A dataframe of cpue (numbers and weight) by survey station. See Details.
#'
#' @importFrom sqldf sqldf
#' @importFrom wtsUtilities selectFile
#'
#'@export
#'
doCalcs_CPUEbyStation<-function(tbl_strata,
                                tbl_hauls,
                                tbl_indivs,
                                 aggBySex=FALSE,
                                 aggByMaturity=FALSE,
                                 aggByShellCondition=FALSE,
                                 aggBySize=FALSE,
                                 cutpts=seq(from=25,to=185,by=5),
                                 truncate.low=TRUE,
                                 truncate.high=FALSE,
                                 dropLevels=list(SEX=c('MISSING',"HERMAPHRODITIC")),
                                 verbosity=0){
    ##calculate CPUE by haul
    message("calculating CPUE by haul.");
    dfrCPUE.ByXMS<-calcCPUE.ByHaul(tbl_hauls,
                                   tbl_indivs,
                                   bySex=!aggBySex,
                                   byMaturity=!aggByMaturity,
                                   byShellCondition=!aggByShellCondition,
                                   bySize=!aggBySize,
                                   cutpts=cutpts,
                                   truncate.low=truncate.low,
                                   truncate.high=truncate.high,
                                   export=FALSE,
                                   verbosity=verbosity);

    #--drop levels
    if (!is.null(dropLevels)){
      message(paste0("dropping levels"));
      dfrCPUE.ByXMS<-wtsUtilities::dropLevels(dfrCPUE.ByXMS,
                                              dropLevels=dropLevels);
    }

    ##recode if aggregated
    if (aggBySex)            dfrCPUE.ByXMS$SEX            <-"ALL";
    if (aggByMaturity)       dfrCPUE.ByXMS$MATURITY       <-"ALL";
    if (aggByShellCondition) dfrCPUE.ByXMS$SHELL_CONDITION<-"ALL";
    if (aggBySize)           dfrCPUE.ByXMS$SIZE           <-"ALL";

    ##calculate CPUE by station
    message("calculating CPUE by station.");
    dfrCPUE.ByXMS<-calcCPUE.ByStation(tbl_strata,
                                      dfrCPUE.ByXMS,
                                      export=FALSE,
                                      verbosity=verbosity);

    return(dfrCPUE.ByXMS)
}
