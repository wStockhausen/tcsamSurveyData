#'
#'@title Function to calculate CPUE, abundance and biomass at various levels of spatial aggregation from station, haul and individual crab info.
#'
#'@description Function to calculate CPUE, abundance and biomass from station, haul and individual crab info.
#'
#' @param tbl_strata  - dataframe with survey strata info (output from \code{\link{selectStrata.TrawlSurvey}})
#' @param tbl_hauls   - dataframe with hauls info (output from \code{\link{selectHauls.TrawlSurvey}})
#' @param tbl_indivs  - dataframe with individual crab info (output from \code{\link{selectIndivs.TrawlSurvey}})
#' @param averageHaulsByStation - TRUE (average hauls by station prior to calculating stratum-level quantities)
#' @param useStratumArea : flag (T/F) to use STRATUM_AREA to expand average CPUE to stratum abundance/biomass (default is T)
#' @param calcByEW166 : TRUE (calculate AB by EW166 prior to calculating total for EBS)
#' @param aggBySex            - flag to agregate CPUE over sexes
#' @param aggByMaturity       - flag to aggregate of maturity states
#' @param aggByShellCondition - flag to aggregate over shell conditions
#' @param aggBySize           - flag to aggregate over sizes
#' @param cutpts - seq(from=0,to=185,by=5)
#' @param truncate.low - TRUE
#' @param truncate.high - FALSE
#' @param dropLevels - NULL, or list (by factor name) of vectors of factor values to drop (see help for \code{wtsUtilities::dropLevels})
#' @param verbosity - integer flag indicating level of printed output (0=off,1=minimal,2=full)
#'
#'@return   a nested list with elements cpue and ABs. \code{cpue} has the following elements: \cr
#'\itemize{
#' \item {byHaul}     {- data frame of cpue (numbers and weight) by year, haul and other factor levels}
#' \item {byStratum}     {- data frame of cpue (numbers and weight) by year, station and other factor levels (or NULL)}
#'} \cr
#' \code{ABs} has the following elements: \cr
#'\itemize{
#' \item {byStratum} {- data frame with abundance, biomass by year, stratum and other factor levels}
#' \item {EW166}     {- data frame with abundance, biomass by year, EW166 split and other factor levels (optional)}
#' \item {EBS}       {- data frame with abundance, biomass by year and other factor levels for the EBS}
#'}
#'
#'@details
#'Notes: \itemize{
#'   \item CPUE in numbers is in no/(sq. nm.)
#'   \item CPUE in weight  is in mt/(sq. nm.)
#'   \item Abundance is in 10^6 individuals
#'   \item Biomass   is in 10^3 mt
#'   }
#'
#' @export
#'
doCalcs_ABs<-function(tbl_strata,
                       tbl_hauls,
                       tbl_indivs,
                       averageHaulsByStation=TRUE,
                       useStratumArea=TRUE,
                       calcByEW166=TRUE,
                       aggBySex=FALSE,
                       aggByMaturity=FALSE,
                       aggByShellCondition=FALSE,
                       aggBySize=FALSE,
                       cutpts=seq(from=0,to=185,by=5),
                       truncate.low=TRUE,
                       truncate.high=FALSE,
                       dropLevels=list(SEX=c('MISSING',"HERMAPHRODITIC")),
                       verbosity=0){

    message("starting doCalcs_ABs.\n");

    #calc cpue by haul for all selected individuals
    cpue.byH<-NULL;
    cpue.byH<-calcCPUE.ByHaul(tbl_hauls=tbl_hauls,
                              tbl_indivs=tbl_indivs,
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
      cpue.byH<-wtsUtilities::dropLevels(cpue.byH,
                                         dropLevels=dropLevels);
    }

    ##recode if aggregated
    if (aggBySex)            cpue.byH$SEX            <-"ALL";
    if (aggByMaturity)       cpue.byH$MATURITY       <-"ALL";
    if (aggByShellCondition) cpue.byH$SHELL_CONDITION<-"ALL";
    if (aggBySize)           cpue.byH$SIZE           <-"ALL";

    #calc abundance, biomass by strata
    cpue.byS<-NULL;
    bio.byStr<-NULL;
    if (averageHaulsByStation){
        #calc cpue by station from cpue by haul
        message("averaging cpue by station.")
        cpue.byS<-calcCPUE.ByStation(tbl_strata=tbl_strata,
                                     tbl_cpue=cpue.byH,
                                     export=FALSE,
                                     verbosity=verbosity);

        bio.byStr<-calcAB.ByStratum(tbl_strata=tbl_strata,
                                     tbl_cpue=cpue.byS,
                                     useStratumArea=useStratumArea,
                                     export=FALSE,
                                     verbosity=verbosity);
    } else {
        message("NOT averaging cpue by station.")
        bio.byStr<-calcAB.ByStratum(tbl_strata=tbl_strata,
                                     tbl_cpue=cpue.byH,
                                     useStratumArea=useStratumArea,
                                     export=FALSE,
                                     verbosity=verbosity);
    }

    #calc total abundance, biomass
    bio.EW166<-NULL;
    bio.tot<-NULL;
    if (calcByEW166){
        #calc abundance, biomass in EW166 regions
        message("calculating AB by EW166.")
        bio.EW166<-calcAB.EW166(bio.byStr,
                                 export=FALSE,
                                 verbosity=verbosity);
        bio.tot<-calcAB.EBS(bio.EW166,
                             export=FALSE,
                             verbosity=verbosity)
    } else {
        message("NOT calculating AB by EW166.")
        bio.tot<-calcAB.EBS(bio.byStr,
                             export=FALSE,
                             verbosity=verbosity)
    }

    message("finished doCalcs_ABs.");
    return(list(cpue=list(byHaul=cpue.byH,byStation=cpue.byS),
                ABs=list(byStratum=bio.byStr,EW166=bio.EW166,EBS=bio.tot)))
}
