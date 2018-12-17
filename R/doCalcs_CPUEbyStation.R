#'
#'@title Calculate cpue by survey station and other factors from strata and haul files
#'
#'@description Function to calculate cpue by survey sstation and other factors (e.g., sex) from strata and haul files.
#'
#' @param species : character string indicating species (i.e., "Tanner crab","Snow crab","BKC", or "RKC")
#' @param surveyType : survey type ("NMFS" or "BSFRF")
#' @param bsfrfTypes : vector of BSFRF types to include ("SBS" and/or "IDX")
#' @param strataTbl : dataframe previously created by call to \code{\link{selectStrata.TrawlSurvey}}
#' @param strataFile : path to strata file (req'd if strataTbl is NULL)
#' @param strataType : "2015"
#' @param haulFiles : vector of paths to haul files
#' @param HaulTypes : haul types (NULL selects all)
#' @param minYr : min year to include
#' @param maxYr : max year to include
#' @param sex             : sexes to include ("MALE", "FEMALE", or "ALL")
#' @param maturity        : maturity states to include ("IMMATURE", "MATURE", "UNDETERMINED" , or "ALL")
#' @param shell_condition : shell conditions to include ("NEW", "OLD", "ALL")
#' @param minSize       : min size (CW) to include
#' @param maxSize       : max size (CW) to include
#' @param cutpts        : vector of cutpoints to create size bins from
#' @param truncate.low  : flag (T/F) to exclude individuals smaller than minSize
#' @param truncate.high : flag (T/F) to exclude individuals larger than maxSize
#' @param calcMaleMaturity    : flag (T/F) to calculate male maturity using the Rugolo/Turnock maturity ogive
#' @param aggBySex            : flag to agregate CPUE over sexes
#' @param aggByMaturity       : flag to aggregate of maturity states
#' @param aggByShellCondition : flag to aggregate over shell conditions
#' @param aggBySize           : flag to aggregate over sizes
#' @param verbosity : integer flag indicating level of printed output (0=off,1=minimal,2=full)
#'
#'@details
#'The returned dataframe has the following columns: \cr
#'\itemize{
#'\item   YEAR
#'\item   STRATUM
#'\item   GIS_STATION
#'\item   HAULJOIN
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
#'@return A dataframe of cpue (numbers and weight) by haul. See Details.
#'
#' @importFrom sqldf sqldf
#' @importFrom wtsUtilities selectFile
#'
#'@export
#'
doCalcs_CPUEbyStation<-function(species=c("Tanner crab","Snow crab","BKC","RKC"),
                                 surveyType=c("NMFS","BSFRF"),
                                 bsfrfTypes=c("SBS","IDX"),
                                 strataTbl=NULL,
                                 strataFile="",
                                 strataType="2015",
                                 haulFiles="",
                                 HaulTypes=NULL,
                                 minYr=2016,
                                 maxYr=2017,
                                 sex="ALL",
                                 maturity="ALL",
                                 shell_condition="ALL",
                                 minSize=0,
                                 maxSize=Inf,
                                 calcMaleMaturity=FALSE,
                                 cutpts=seq(from=25,to=185,by=5),
                                 truncate.low=TRUE,
                                 truncate.high=FALSE,
                                 aggBySex=FALSE,
                                 aggByMaturity=FALSE,
                                 aggByShellCondition=FALSE,
                                 aggBySize=FALSE,
                                 verbosity=0){
    verbose<-(verbosity>0);

    #--determine crab type
    if (tolower(species)=="tanner crab") {species<-"BTC"; crabType<-"Tanner crab";}    else
    if (tolower(species)=="snow crab")   {species<-"OTC"; crabType<-"snow crab";}      else
    if (tolower(species)=="BKC")         {species<-"BKC"; crabType<-"blue king crab";} else
    if (tolower(species)=="RKC")         {species<-"RKC"; crabType<-"red king crab";}

    #--creates trawl survey codes
    codes.TS<-Codes.TrawlSurvey();

    #--read strata definitions
    if (is.null(strataTbl)){
      message(paste0("Reading strata file '",strataFile,"'"));
      csvStrata<-read.csv(strataFile,check.names=FALSE,stringsAsFactors=FALSE)[,1:8];
      #--create dataframe of selected survey strata
      message(paste0("Selecting strata."));
      dfrSD<-selectStrata.TrawlSurvey(csvStrata,
                                       species=species,
                                       strataType=strataType,
                                       export=FALSE,
                                       verbosity=verbosity);
    } else dfrSD <-strataTbl;

    #--read crabhaul files
    csvCrabhauls<-NULL;
    for (f in haulFiles){
      message(paste0("Reading haul file '",f,"'"));
      tmp<-read.csv(file=f,check.names=FALSE,stringsAsFactors=FALSE);
      csvCrabhauls<-rbind(csvCrabhauls,tmp);
      rm(tmp);
    }
    rm(f);

    #--convert BSFRF haul data to NMFS format
    if (toupper(surveyType)=="BSFRF") {
      message("Converting BSFRF haul data to NMFS format.");
      csvCrabhauls <- convertFormat.BSFRF2NMFS(csvCrabhauls,types=bsfrfTypes,verbosity=verbosity);
    }

    #--create dataframe of selected haul data
    message(paste0("Selecting haul data."));
    dfrHD<-selectHauls.TrawlSurvey(dfrSD,
                                    tbl=csvCrabhauls,
                                    YearRange=c(minYr,maxYr),
                                    export=FALSE,
                                    verbosity=verbosity);

    #create dataframe of selected individuals
    message(paste0("Selecting individuals."));
    if (calcMaleMaturity){
      message(paste0(" Note that categorization of male maturity is based on an ogive",
                     " developed by Rugolo and Turnock, and is shown here for illustrative purposes."));
    }
    dfrID<-selectIndivs.TrawlSurvey(dfrHD,
                                     tbl=csvCrabhauls,
                                     sex=sex,
                                     maturity=maturity,
                                     shell_condition=shell_condition,
                                     calcMaleMaturity=calcMaleMaturity,
                                     minSize=minSize,
                                     maxSize=maxSize,
                                     export=FALSE,
                                     verbosity=verbosity);

    ##calculate CPUE by haul
    message(paste0("calculating CPUE by haul."));
    dfrCPUE.ByXMS<-calcCPUE.ByHaul(dfrHD,
                                   dfrID,
                                   bySex=!aggBySex,
                                   byMaturity=!aggByMaturity,
                                   byShellCondition=!aggByShellCondition,
                                   bySize=!aggBySize,
                                   cutpts=cutpts,
                                   truncate.low=truncate.low,
                                   truncate.high=truncate.high,
                                   export=FALSE,
                                   verbosity=verbosity);

    ##calculate CPUE by station
    message(paste0("calculating CPUE by station."));
    dfrCPUE.ByXMS<-calcCPUE.ByStation(dfrSD,
                                      dfrCPUE.ByXMS,
                                      export=FALSE,
                                      verbosity=verbosity);

    #--drop levels
    message(paste0("dropping 'missing' and 'hermaphroditic' sex levels."));
    dfrCPUE.ByXMS<-wtsUtilities::dropLevels(dfrCPUE.ByXMS,
                                            list(SEX=c('MISSING',"HERMAPHRODITIC")));

    ##recode if aggregated
    if (aggBySex)            dfrCPUE.ByXMS$SEX            <-"ALL";
    if (aggByMaturity)       dfrCPUE.ByXMS$MATURITY       <-"ALL";
    if (aggByShellCondition) dfrCPUE.ByXMS$SHELL_CONDITION<-"ALL";
    if (aggBySize)           dfrCPUE.ByXMS$SIZE           <-"ALL";

    return(dfrCPUE.ByXMS)
}
