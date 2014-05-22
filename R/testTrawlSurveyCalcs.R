#'
#'@title Test trawl survey functions.
#'
#'@description This function provides a test of the trawl survey functions.
#'
#'@details This function prompts the user for survey data and strata files. It then 
#'calls selectStrata.TrawlSurvey for Tanner crab (Bairdi tanner crab, 'BTC') twice, 
#'once for the revised strata, once for the original strata. It then selects hauls and
#'individuals, calculates biomass time series, cpue by haul and station, and 
#'biomass time series by stratum, E/W 166W, and total and outputs all (or most)
#'to csv files in the user's working directory.
#'
#'@importFrom wtsUtilities getCSV
#'
#'
#'@export
#'
testTrawlSurveyCalcs<-function(){
#    source("../Utilities/getCSV.R",chdir=TRUE)
    
    #read in trawl survey individual crab info and strata info
    info.Indivs<-getCSV(caption="Select AFSC trawl survey data file")
    info.Strata<-getCSV(caption="Select AFSC trawl survey strata file")
    
    #select strata/stations
#    source("selectStrata.TrawlSurvey.R")
    strata.rev<-selectStrata.TrawlSurvey(tbl=info.Strata,species='BTC')
    strata.org<-selectStrata.TrawlSurvey(tbl=info.Strata,species='BTC',useOrigStrata=TRUE)
    
    #select hauls
#    source("selectHauls.TrawlSurvey.R")
    #tbl.hauls<-selectHauls.TrawlSurvey(tbl.strata.org,tbl=tbl.TS.IndivsInfo,HaulTypes=NULL,,export=TRUE,out.csv="SurveyHauls.OrigStrata.csv");#standard haul_type
    #tbl.hauls<-selectHauls.TrawlSurvey(tbl.strata.rev,tbl=tbl.TS.IndivsInfo,HaulTypes=NULL,,export=TRUE,out.csv="SurveyHauls.RevdStrata.csv");#standard haul_type
    tbl.hauls<-selectHauls.TrawlSurvey(strata.org,tbl=info.Indivs,HaulTypes=3);
    #tbl.hauls<-selectHauls.TrawlSurvey(tbl.strata.org,tbl=tbl.TS.IndivsInfo,HaulTypes=c(0,3,4,5,19));
    
    #select individuals
#    source("selectIndivs.TrawlSurvey.R")
    tbl.indivs<-selectIndivs.TrawlSurvey(tbl.hauls,tbl=info.Indivs,
                                         sex='ALL',shell_condition='ALL',
                                         maturity='ALL',calcMaleMaturity=TRUE,
                                         export=FALSE,out.csv="indivs.csv");
    
#    source("doBiomassCalcs.R")
    bio.bySex.RS<-doBiomassCalcs(strata.rev,
                                 tbl.hauls,
                                 tbl.indivs,
                                 calcCPUE.byStation=TRUE,
                                 calcBiomass.byEW166=TRUE,
                                 bySex=TRUE,
                                 byShellCondition=FALSE,
                                 byMaturity=FALSE,
                                 bySize    =FALSE,
                                 binSizes  =FALSE,
                                 cutpts=seq(from=0,to=185,by=5),
                                 truncate.low =TRUE,
                                 truncate.high=FALSE,
                                 export.cpue.byH    =FALSE,
                                 export.cpue.byS    =FALSE,
                                 export.bio.byStrata=FALSE,
                                 export.bio.EW166   =TRUE,
                                 export.totBio      =TRUE,
                                 out.csv.cpue.byH='cpue.byH.csv',
                                 out.csv.cpue.byS='cpue.byS.csv',
                                 out.csv.bio.byStrata='bio.byStrata.csv',
                                 out.csv.bio.EW166='bio.RS.bySex.EW166.csv',
                                 out.csv.totBio='totbio.RS.bySex.csv',
                                 out.dir=getwd());
    bio.bySex.OH<-doBiomassCalcs(strata.org,
                                 tbl.hauls,
                                 tbl.indivs,
                                 calcCPUE.byStation=FALSE,
                                 calcBiomass.byEW166=TRUE,
                                 bySex=TRUE,
                                 byShellCondition=FALSE,
                                 byMaturity=FALSE,
                                 bySize    =FALSE,
                                 binSizes  =FALSE,
                                 cutpts=seq(from=0,to=185,by=5),
                                 truncate.low =TRUE,
                                 truncate.high=FALSE,
                                 export.cpue.byH    =FALSE,
                                 export.cpue.byS    =FALSE,
                                 export.bio.byStrata=FALSE,
                                 export.bio.EW166   =TRUE,
                                 export.totBio      =TRUE,
                                 out.csv.cpue.byH='cpue.byH.csv',
                                 out.csv.cpue.byS='cpue.byS.csv',
                                 out.csv.bio.byStrata='bio.OH.bySexStrata.csv',
                                 out.csv.bio.EW166='bio.OH.bySex.EW166.csv',
                                 out.csv.totBio='totbio.OH.bySex.csv',
                                 out.dir=getwd());
    bio.RS<-doBiomassCalcs(strata.rev,
                             tbl.hauls,
                             tbl.indivs,
                             calcCPUE.byStation=TRUE,
                             calcBiomass.byEW166=TRUE,
                             bySex=FALSE,
                             byShellCondition=FALSE,
                             byMaturity=FALSE,
                             bySize    =FALSE,
                             binSizes  =FALSE,
                             cutpts=seq(from=0,to=185,by=5),
                             truncate.low =TRUE,
                             truncate.high=FALSE,
                             export.cpue.byH    =FALSE,
                             export.cpue.byS    =FALSE,
                             export.bio.byStrata=TRUE,
                             export.bio.EW166   =TRUE,
                             export.totBio      =TRUE,
                             out.csv.cpue.byH='cpue.byH.csv',
                             out.csv.cpue.byS='cpue.byS.csv',
                             out.csv.bio.byStrata='bio.RS.byStrata.csv',
                             out.csv.bio.EW166='bio.RS.EW166.csv',
                             out.csv.totBio='totbio.RS.csv',
                             out.dir=getwd());
    bio.OH<-doBiomassCalcs(strata.org,
                             tbl.hauls,
                             tbl.indivs,
                             calcCPUE.byStation=FALSE,
                             calcBiomass.byEW166=TRUE,
                             bySex=FALSE,
                             byShellCondition=FALSE,
                             byMaturity=FALSE,
                             bySize    =FALSE,
                             binSizes  =FALSE,
                             cutpts=seq(from=0,to=185,by=5),
                             truncate.low =TRUE,
                             truncate.high=FALSE,
                             export.cpue.byH    =FALSE,
                             export.cpue.byS    =FALSE,
                             export.bio.byStrata=TRUE,
                             export.bio.EW166   =TRUE,
                             export.totBio      =TRUE,
                             out.csv.cpue.byH='cpue.byH.csv',
                             out.csv.cpue.byS='cpue.byS.csv',
                             out.csv.bio.byStrata='bio.OS.byStrata.csv',
                             out.csv.bio.EW166='bio.OH.EW166.csv',
                             out.csv.totBio='totbio.OH.csv',
                             out.dir=getwd());
    
    
#    source("calcCPUE.ByHaul.R")
    #calc cpue by haul for all individuals
    cpue.byH<-calcCPUE.ByHaul(tbl.hauls,tbl.indivs);
    #cpue.byH<-calcCPUE.ByHaul(tbl.hauls,tbl.indivs,bySex=TRUE,bySize=TRUE,binSizes=TRUE)
    
#    source("calcCPUE.ByStation.R")
    #calc cpue by station for all individuals
    cpue.byS<-calcCPUE.ByStation(cpue.byH)
    
#    source("calcBiomass.ByStratum.R")
    #calc average cpue, biomass by strata
    bio.frOS.byH<-calcBiomass.ByStratum(tbl.strata.org,cpue.byH);
    bio.frRS.byS<-calcBiomass.ByStratum(tbl.strata.rev,cpue.byS);
    
#    source("calcBiomass.EW166.R")
    bio.EW166.frOS.byH<-calcBiomass.EW166(bio.frOS.byH,export=TRUE,out.csv='bio.EW166.frOS.byH.csv')
    bio.EW166.frRS.byS<-calcBiomass.EW166(bio.frRS.byS,export=TRUE,out.csv='bio.EW166.frRS.byS.csv')
    
#    source("calcBiomass.EBS.R")
    #calc total abundance, biomass
    bio.TOT.frOS.byH<-calcBiomass.EBS(bio.frOS.byH,export=TRUE,out.csv='bio.TOT.frOS.byH.csv')
    bio.TOT.frRS.byS<-calcBiomass.EBS(bio.frRS.byS,export=TRUE,out.csv='bio.TOT.frRS.byS.csv')
}
