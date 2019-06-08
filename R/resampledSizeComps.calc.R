#'
#'@title Resample haul data to bootstrap size compositions by year/stratum from AFSC trawl survey data
#'
#'@param N          : number of resamples/year
#'@param tbl_strata : data frame from call to \code{\link{selectStrata.TrawlSurvey}} [required]
#'@param tbl_hauls  : dataframe from call to \code{\link{selectHauls.TrawlSurvey}} [required only if tbl_cpue not given]
#'@param tbl_indivs : dataframe from call to \code{\link{selectIndivs.TrawlSurvey}} (or crab survey filename, or NULL) [required only if tbl_cpue not given]
#'@param byEW166           : flag (T/F) to aggregate size comps to EW166
#'@param byEBS             : flag (T/F) to aggregate size comps to the EBS
#'@param bySex             : flag (T/F) to calc by sex
#'@param byShellCondition  : flag (T/F) to calc by shell condition
#'@param byMaturity        : flag (T/F) to calc by maturity state
#'@param cutpts        : vector of cutpoints to create size bins from
#'@param truncate.low  : flag (T/F) to exclude individuals smaller than minSize
#'@param truncate.high : flag (T/F) to exclude individuals larger than maxSize
#'@param verbosity : integer flag indicating level of printed output (0=off,1=minimal,2=full)
#'
#'@description This function resamples size compositions by year/stratum for AFSC trawl survey data.
#'
#'@details This function performs two-stage resampling (hauls w/in strata, individuals w/in hauls)
#'to calculate a set of resampled size compositions by year. The size comps with resampling index i=0
#'are the observed size comps.\cr
#'\cr
#'Other notes: \cr
#'\itemize{
#'   \item{Area is in square nautical miles}
#'   \item{Abundance is in 10^6 indivs}
#'   \item{Biomass   is in 10^3 mt}
#'}
#'
#'@return data frame with size comps by stratum. Columns are \cr
#'\itemize{
#'\item  i - resampling index (i=0 denotes the obsesrved size comps)
#'\item  YEAR
#'\item  STRATUM
#'\item  STRATUM_AREA
#'\item  SEX
#'\item  MATURTITY
#'\item  SHELL_CONDITION
#'\item  SIZE
#'\item  numStations
#'\item  numHauls
#'\item  numNonZeroHauls
#'\item  numIndivs
#'\item  totABUNDANCE = estimated abundance-by-size (by stratum)
#'\item  totBIOMASS = estimated biomass-by-size (by stratum)
#'}
#'
#'@export
#'
#######################################################################
resampledSizeComps.calc<-function(tbl_strata,
                                  tbl_hauls=NULL,
                                  tbl_indivs=NULL,
                                  N=100,
                                  byEW166=FALSE,
                                  byEBS=TRUE,
                                  bySex=FALSE,
                                  byShellCondition=FALSE,
                                  byMaturity=FALSE,
                                  cutpts=seq(from=25,to=185,by=5),
                                  truncate.low=TRUE,
                                  truncate.high=FALSE,
                                  verbosity=0){
  if (verbosity>0) cat("starting calcResampledSizeComps\n");

  dfr.zcs<-NULL;
  #calculate observed size comps
  zcs <- calcSizeComps.ByStratum(tbl_strata=tbl_strata,
                                 tbl_hauls=tbl_hauls,
                                 tbl_indivs=tbl_indivs,
                                 bySex=bySex,
                                 byShellCondition=byShellCondition,
                                 byMaturity=byMaturity,
                                 cutpts=cutpts,
                                 truncate.low=truncate.low,
                                 truncate.high=truncate.high);
  if (byEW166|byEBS){
    zcs<-calcSizeComps.EW166(tbl=zcs,export=FALSE);
    if (byEBS) zcs<-calcSizeComps.EBS(tbl=zcs,export=FALSE);
  }
  dfr.zcs<-rbind(dfr.zcs,cbind(i=0,zcs));

  if (N>0){
    #only do for years in tbl_hauls
    uY<-sort(unique(tbl_hauls$YEAR));
    for (y in uY){
      ytbl_strata<-tbl_strata[tbl_strata$YEAR==y,];
      #find unique strata for current year
      uS<-sort(unique(ytbl_strata$STRATUM));
      #loop over realizations
      for (i in 1:N){
        #loop over strata
        dfr.zcs.byStrat<-NULL;
        for (s in uS){
          #select stratum info
          sytbl_strata<-ytbl_strata[ytbl_strata$STRATUM==s,];
          #select hauls to sample from
          uStns<-unique(sytbl_strata$GIS_STATION);
          sytbl_hauls<-tbl_hauls[(tbl_hauls$YEAR==y)&(tbl_hauls$GIS_STATION %in% uStns),];
          sytbl_indivs<-tbl_indivs[tbl_indivs$HAULJOIN %in% sytbl_hauls$HAULJOIN,];
          nH  <- nrow(sytbl_hauls);#number of hauls in stratum s, year y
          #resample hauls by stratum
          idH <- ceiling(stats::runif(n=nH,min=1,max=nH));#random index to row of sytbl_hauls
          rHJs<-sytbl_hauls$HAULJOIN[idH];   #hauljoins from randomly-selected hauls
          rtbl_hauls<-sytbl_hauls[idH,];     #table of randomly-selected hauls
          rtbl_hauls$HAULJOIN<-1:length(idH);#renumber hauljoins so no duplicates
          rtbl_indivs<-NULL;
          for (h in idH){
            #resample individuals by haul
            htbl_indivs<-sytbl_indivs[sytbl_indivs$HAULJOIN==rHJs[h],];#indivs in randomly-selected haul
            nI<-nrow(htbl_indivs);                                     #number of individuals in haul
            if (nI>0){
              idI<-ceiling(stats::runif(n=nI,min=1,max=nI));           #random index to row of htbl_indivs
              rtbl<-htbl_indivs[idI,];                                   #randomly-selected individuals
              rtbl$HAULJOIN<-h;                                          #rewrite hauljoin
              rtbl_indivs<-rbind(rtbl_indivs,rtbl);                      #add to table of "all" indivs collected in stratum
            }
          }#h-loop
          #because a station may be selected multiple times, need to fake out
          #the strata and hauls tables to get unique GIS_STATIONs for the sample
          gisStns<-rtbl_hauls$GIS_STATION;
          rtbl_strata<-NULL;
          for (gisStn in gisStns){
            rtbl_strata<-rbind(rtbl_strata,sytbl_strata[sytbl_strata$GIS_STATION==gisStn,]);
          }
          rtbl_strata$GIS_STATION<-as.character(1:nH);
          rtbl_hauls$GIS_STATION<-as.character(1:nH);
          if (!is.null(rtbl_indivs)){
            #calculate size comps by stratum
            zcs <- calcSizeComps.ByStratum(tbl_strata=rtbl_strata,
                                           tbl_hauls=rtbl_hauls,
                                           tbl_indivs=rtbl_indivs,
                                           bySex=bySex,
                                           byShellCondition=byShellCondition,
                                           byMaturity=byMaturity,
                                           cutpts=cutpts,
                                           truncate.low=truncate.low,
                                           truncate.high=truncate.high);
            dfr.zcs.byStrat<-rbind(dfr.zcs.byStrat,zcs);
            rm(zcs);
          } else {
            cat("no indivs selected for year",y,",stratum",s,",sample",i,"\n");
          }
        }#s-loop
        if (!is.null(dfr.zcs.byStrat)){
          cat("calculating size comps for year",y,"sample",i,"\n");
          zcs<-dfr.zcs.byStrat;
          if (byEW166|byEBS){
            zcs<-calcSizeComps.EW166(tbl=dfr,export=FALSE);
            if (byEBS) zcs<-calcSizeComps.EBS(tbl=zcs,export=FALSE);
          }
          dfr.zcs<-rbind(dfr.zcs,cbind(i=i,zcs));
        } else {
          cat("no size comps for year",y,",sample",i,"\n");
        }
      }#i-loop
    }#y-loop
  }#N>0

  if (!("SEX" %in% names(dfr.zcs)))             dfr.zcs$SEX<-"all";
  if (!("MATURITY" %in% names(dfr.zcs)))        dfr.zcs$MATURITY<-"all";
  if (!("SHELL_CONDITION" %in% names(dfr.zcs))) dfr.zcs$SHELL_CONDITION<-"all";
  return(dfr.zcs)
}

# dfr<-calcResampledSizeComps(tbl_strata=dfr.SD,
#                             tbl_hauls=dfr.HD,tbl_indivs=dfr.ID,byEBS=TRUE,
#                             bySex=TRUE,byShellCondition=TRUE,byMaturity=TRUE,
#                             cutpts=cutpts);
