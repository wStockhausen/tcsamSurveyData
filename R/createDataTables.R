#'
#'@title Create strata, haul and individual data tables (i.e., dataframes) from strata and crabhaul files
#'
#'@description Function to create strata, haul and individual data tables (i.e., dataframes) from strata and crabhaul files.
#'
#' @param species : character string indicating species (i.e., "Tanner crab","Snow crab","BKC", or "RKC")
#' @param surveyType : survey type ("NMFS" or "BSFRF")
#' @param bsfrfTypes : vector of BSFRF types to include ("SBS" and/or "IDX")
#' @param strataTbl : dataframe previously created by call to \code{\link{selectStrata.TrawlSurvey}}
#' @param strataFile : path to strata file (req'd if strataTbl is NULL)
#' @param strataType : "2015"
#' @param haulFiles : vector of paths to crabhaul files
#' @param HaulTypes : haul types to include (NULL selects all)
#' @param minYr : min year to include
#' @param maxYr : max year to include
#' @param sex             : sexes to include ("MALE", "FEMALE", or "ALL")
#' @param maturity        : maturity states to include ("IMMATURE", "MATURE", "UNDETERMINED" , or "ALL")
#' @param shell_condition : shell conditions to include ("NEW", "OLD", "ALL")
#' @param minSize       : min size (CW) to include
#' @param maxSize       : max size (CW) to include
#' @param verbosity : integer flag indicating level of printed output (0=off,1=minimal,2=full)
#'
#'@details
#'@return   a list with the following elements: \cr
#'\itemize{
#' \item {dfrStrataData} {- dataframe with stratum information; see \code{\link{selectStrata.TrawlSurvey}}}
#' \item {dfrHaulData}   {- dataframe with haul information; see \code{\link{selectHauls.TrawlSurvey}}}
#' \item {dfrIndivData}  {- dataframe with individual crab data; see \code{\link{selectIndivs.TrawlSurvey}}}
#'}
#'
#'@return A list with three dataframes. See Details.
#'
#' @importFrom sqldf sqldf
#' @importFrom wtsUtilities selectFile
#'
#'@export
#'
createDataTables<-function(species=c("Tanner crab","Snow crab","BKC","RKC"),
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
    message("Selecting individuals.");
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

  return(list(dfrStrataData=dfrSD,dfrHaulData=dfrHD,dfrIndivData=dfrID));
}
