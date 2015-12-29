#'
#'@title Test trawl survey functions.
#'
#'@description This function provides a test of the trawl survey functions.
#'
#'@param out.dir   = directory for output files
#'@param verbosity = integer flag indicating level of printed output (0=off,1=minimal,2=full)
#'
#'@details This function prompts the user for input survey data and strata files. It then 
#'calls \code{\link{selectStrata.TrawlSurvey}} for Tanner crab (Bairdi tanner crab, 'BTC') twice, 
#'once for the revised strata, once for the original strata. It then selects hauls and
#'individuals, calculates biomass time series, cpue by haul and station, and 
#'biomass time series by stratum, E/W 166W, and total and outputs all (or most)
#'to csv files in the directory specified by \code{out.dir}.\cr
#'\cr Note: if \code{out.dir=NULL}, the directory associated with the 
#'trawl survey data file will be used as \code{out.dir}
#'
#'@importFrom wtsUtilities selectFile
#'
#'@export
#'
testTrawlSurveyCalcs<-function(out.dir=NULL,
                               verbosity=1){
    
    #select strata/stations
    strata.org<-selectStrata.TrawlSurvey(tbl=NULL,species='BTC',strataType='orig',   verbosity=verbosity)
    strata.rev<-selectStrata.TrawlSurvey(tbl=NULL,species='BTC',strataType='revised',verbosity=verbosity)
    strata.new<-selectStrata.TrawlSurvey(tbl=NULL,species='BTC',strataType='new2015',verbosity=verbosity)
    
    #read in trawl survey individual crab info and strata info
    info.Indivs<-selectFile("csv",caption="Select AFSC trawl survey data file")
    if (is.null(out.dir)) out.dir<-dirname(info.Indivs);
    info.Indivs<-read.csv(info.Indivs,stringsAsFactors=FALSE);
    
    info.Strata<-selectFile("csv",caption="Select AFSC trawl survey strata file")
    info.Strata<-read.csv(info.Strata,stringsAsFactors=FALSE);
    
    #select hauls
    #tbl.hauls<-selectHauls.TrawlSurvey(strata.org,tbl=tbl.TS.IndivsInfo,HaulTypes=NULL,,export=TRUE,out.csv="SurveyHauls.OrigStrata.csv",verbosity=verbosity);#standard haul_type
    #tbl.hauls<-selectHauls.TrawlSurvey(strata.rev,tbl=tbl.TS.IndivsInfo,HaulTypes=NULL,,export=TRUE,out.csv="SurveyHauls.RevdStrata.csv",verbosity=verbosity);#standard haul_type
    tbl.hauls<-selectHauls.TrawlSurvey(strata.org,tbl=info.Indivs,HaulTypes=3,verbosity=verbosity);
    #tbl.hauls<-selectHauls.TrawlSurvey(strata.org,tbl=tbl.TS.IndivsInfo,HaulTypes=c(0,3,4,5,19),verbosity=verbosity);
    
    #select individuals
    tbl.indivs<-selectIndivs.TrawlSurvey(tbl.hauls,
                                         tbl=info.Indivs,
                                         sex='ALL',
                                         shell_condition='ALL',
                                         maturity='ALL',
                                         calcMaleMaturity=TRUE,
                                         export=FALSE,
                                         out.csv="indivs.csv",
                                         verbosity=verbosity);
    
#     bio.bySex.RS<-doBiomassCalcs(strata.rev,
#                                  tbl.hauls,
#                                  tbl.indivs,
#                                  calcCPUE.byStation=TRUE,
#                                  calcBiomass.byEW166=TRUE,
#                                  bySex=TRUE,
#                                  byShellCondition=FALSE,
#                                  byMaturity=FALSE,
#                                  bySize    =FALSE,
#                                  cutpts=seq(from=0,to=185,by=5),
#                                  truncate.low =TRUE,
#                                  truncate.high=FALSE,
#                                  export.cpue.byH    =FALSE,
#                                  export.cpue.byS    =FALSE,
#                                  export.bio.byStrata=FALSE,
#                                  export.bio.EW166   =TRUE,
#                                  export.totBio      =TRUE,
#                                  out.csv.cpue.byH='cpue.byH.csv',
#                                  out.csv.cpue.byS='cpue.byS.csv',
#                                  out.csv.bio.byStrata='bio.byStrata.csv',
#                                  out.csv.bio.EW166='bio.RS.bySex.EW166.csv',
#                                  out.csv.totBio='totbio.RS.bySex.csv',
#                                  out.dir=out.dir,
#                                  verbosity=verbosity);
    bio.bySex.OH<-doBiomassCalcs(strata.org,
                                 tbl.hauls,
                                 tbl.indivs,
                                 calcCPUE.byStation=FALSE,
                                 calcBiomass.byEW166=TRUE,
                                 bySex=TRUE,
                                 byShellCondition=FALSE,
                                 byMaturity=FALSE,
                                 bySize    =FALSE,
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
                                 out.dir=out.dir,
                                 verbosity=verbosity);
#     bio.RS<-doBiomassCalcs(strata.rev,
#                              tbl.hauls,
#                              tbl.indivs,
#                              calcCPUE.byStation=TRUE,
#                              calcBiomass.byEW166=TRUE,
#                              bySex=FALSE,
#                              byShellCondition=FALSE,
#                              byMaturity=FALSE,
#                              bySize    =FALSE,
#                              cutpts=seq(from=0,to=185,by=5),
#                              truncate.low =TRUE,
#                              truncate.high=FALSE,
#                              export.cpue.byH    =FALSE,
#                              export.cpue.byS    =FALSE,
#                              export.bio.byStrata=TRUE,
#                              export.bio.EW166   =TRUE,
#                              export.totBio      =TRUE,
#                              out.csv.cpue.byH='cpue.byH.csv',
#                              out.csv.cpue.byS='cpue.byS.csv',
#                              out.csv.bio.byStrata='bio.RS.byStrata.csv',
#                              out.csv.bio.EW166='bio.RS.EW166.csv',
#                              out.csv.totBio='totbio.RS.csv',
#                              out.dir=out.dir,
#                              verbosity=verbosity);
    bio.OH<-doBiomassCalcs(strata.org,
                             tbl.hauls,
                             tbl.indivs,
                             calcCPUE.byStation=FALSE,
                             calcBiomass.byEW166=TRUE,
                             bySex=FALSE,
                             byShellCondition=FALSE,
                             byMaturity=FALSE,
                             bySize    =FALSE,
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
                             out.dir=out.dir,
                             verbosity=verbosity);
    
    
#    source("calcCPUE.ByHaul.R")
    #calc cpue by haul for all individuals
    cpue.byH<-calcCPUE.ByHaul(tbl.hauls,
                              tbl.indivs,
                              verbosity=verbosity);
    #cpue.byH<-calcCPUE.ByHaul(tbl.hauls,tbl.indivs,bySex=TRUE,bySize=TRUE)
    
#    source("calcCPUE.ByStation.R")
    #calc cpue by station for all individuals
    cpue.byS<-calcCPUE.ByStation(strata.org,cpue.byH,
                                 verbosity=verbosity)
    
#    source("calcBiomass.ByStratum.R")
    #calc average cpue, biomass by strata
    bio.frOS.byH<-calcBiomass.ByStratum(strata.org,
                                        cpue.byH,
                                        verbosity=verbosity);
    bio.frRS.byS<-calcBiomass.ByStratum(strata.rev,
                                        cpue.byS,
                                        verbosity=verbosity);
    
    bio.EW166.frOS.byH<-calcBiomass.EW166(bio.frOS.byH,
                                          export=TRUE,
                                          out.csv='bio.EW166.frOS.byH.csv',
                                          out.dir=out.dir,
                                          verbosity=verbosity)
    bio.EW166.frRS.byS<-calcBiomass.EW166(bio.frRS.byS,
                                          export=TRUE,
                                          out.csv='bio.EW166.frRS.byS.csv',
                                          out.dir=out.dir,
                                          verbosity=verbosity)
    
    #calc total abundance, biomass
    bio.TOT.frOS.byH<-calcBiomass.EBS(bio.frOS.byH,
                                      export=TRUE,
                                      out.csv='bio.TOT.frOS.byH.csv',
                                      out.dir=out.dir,
                                      verbosity=verbosity)
    bio.TOT.frRS.byS<-calcBiomass.EBS(bio.frRS.byS,
                                      export=TRUE,
                                      out.csv='bio.TOT.frRS.byS.csv',
                                      out.dir=out.dir,
                                      verbosity=verbosity)
}
