#'
#'@title Resample haul data to bootstrap size compositions by year/stratum from AFSC trawl survey data
#'
#'@param N          : number of resamples/year
#'@param tbl_strata : data frame from call to [selectStrata.TrawlSurvey] \[required\]
#'@param tbl_hauls  : dataframe from call to [selectHauls.TrawlSurvey] \[required\]
#'@param tbl_indivs : dataframe from call to [selectIndivs.TrawlSurvey] (or crab survey filename)
#'@param avgHaulsByStation : flag (T/F) to average hauls by station before calc'ing size comps
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
#'@importFrom dplyr bind_rows
#'@export
#'
#######################################################################
resampledSizeComps.calc<-function(N=100,
                                  tbl_strata,
                                  tbl_hauls=NULL,
                                  tbl_indivs=NULL,
                                  avgHaulsByStation=TRUE,
                                  byEW166=FALSE,
                                  byEBS=TRUE,
                                  bySex=FALSE,
                                  byMaturity=FALSE,
                                  byShellCondition=FALSE,
                                  cutpts=seq(from=25,to=185,by=5),
                                  truncate.low=TRUE,
                                  truncate.high=FALSE,
                                  verbosity=0){
  message("#--starting calcResampledSizeComps");

  bctr    = 0;     #--bootstrap counter (0 is non-bootstrapped version)
  lst.zcs = list();#--list of bootstrapped size comps
  #calculate observed size comps
  message("#----calculating observed size comps");
  zcs <- calcSizeComps.ByStratum(tbl_strata=tbl_strata,
                                 tbl_hauls=tbl_hauls,
                                 tbl_indivs=tbl_indivs,
                                 avgHaulsByStation=avgHaulsByStation,
                                 bySex=bySex,
                                 byMaturity=byMaturity,
                                 byShellCondition=byShellCondition,
                                 cutpts=cutpts,
                                 truncate.low=truncate.low,
                                 truncate.high=truncate.high,
                                 verbosity=verbosity);
  if (byEW166|byEBS){
    zcs<-calcSizeComps.EW166(tbl=zcs,export=FALSE,verbosity=verbosity);
    if (byEBS) zcs<-calcSizeComps.EBS(tbl=zcs,export=FALSE,verbosity=verbosity);
  }
  bctr=bctr+1;
  lst.zcs[[bctr]]<-dplyr::bind_cols(i=0,zcs);

  if (N>0){
    #only do for years in tbl_hauls
    uY<-sort(unique(tbl_hauls$YEAR));
    nY<-length(uY);#number of years
    nTBs<-nY*N;   #total number of bootstraps
    start_time<-Sys.time();
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
                                           avgHaulsByStation=avgHaulsByStation,
                                           bySex=bySex,
                                           byShellCondition=byShellCondition,
                                           byMaturity=byMaturity,
                                           cutpts=cutpts,
                                           truncate.low=truncate.low,
                                           truncate.high=truncate.high);
            dfr.zcs.byStrat<-rbind(dfr.zcs.byStrat,zcs);
            rm(zcs);
          } else {
            message("#----no indivs selected for year ",y,", stratum ",s,", sample ",i);
          }
        }#s-loop
        if (!is.null(dfr.zcs.byStrat)){
          message("calculating size comps for year ",y,", sample ",i);
          zcs<-dfr.zcs.byStrat;
          if (byEW166|byEBS){
            zcs<-calcSizeComps.EW166(tbl=zcs,export=FALSE);
            if (byEBS) zcs<-calcSizeComps.EBS(tbl=zcs,export=FALSE);
          }
          bctr=bctr+1;
          lst.zcs[[bctr]] = dplyr::bind_cols(i=i,zcs);
        } else {
          message("#----no size comps for year ",y,", sample",i);
        }
        curr_time<-Sys.time();
        el_time  <-curr_time - start_time;
        elt <- as.numeric(el_time,units="days");
        if (elt > 1) {message("#----elapsed time: ",elt," days.");} else {
          elt<-as.numeric(el_time,units="hours");
          if (elt > 1) {message("#----elapsed time: ",elt," hours.");} else {
            message("#----elapsed time: ",as.numeric(el_time,units="mins")," minutes.");
          }
        }
        rm_time<-as.numeric(nTBs*(el_time/(bctr-1))-el_time,units="days");
        if (rm_time > 1) {message("#----remaining time: ",rm_time," days.");} else {
          rm_time<-as.numeric(nTBs*(el_time/(bctr-1))-el_time,units="hours");
          if (rm_time > 1) {message("#----remaining time: ",rm_time," hours.");} else {
            message("#----remaining time: ",as.numeric(nTBs*(el_time/bctr)-el_time,units="mins")," minutes.");
          }
        }
        tt_time<-as.numeric(nTBs*(el_time/(bctr-1)),units="days");
        if (tt_time > 1) {message("#----total time: ",tt_time," days.");} else {
          tt_time<-as.numeric(nTBs*(el_time/(bctr-1)),units="hours");
          if (tt_time > 1) {message("#----total time: ",tt_time," hours.");} else {
            message("#----total time: ",as.numeric(nTBs*(el_time/bctr),units="mins")," minutes.");
          }
        }
      }#i-loop
    }#y-loop
  }#N>0
  dfr.zcs = dplyr::bind_rows(lst.zcs); #--rm(lst.zcs);

  if (!("SEX" %in% names(dfr.zcs)))             dfr.zcs$SEX<-"all";
  if (!("MATURITY" %in% names(dfr.zcs)))        dfr.zcs$MATURITY<-"all";
  if (!("SHELL_CONDITION" %in% names(dfr.zcs))) dfr.zcs$SHELL_CONDITION<-"all";
  curr_time - Sys.time();
  el_time  <-curr_time - start_time;
  elt <- as.numeric(el_time,units="days");
  if (elt > 1) {message("#----bootstrapping complete. total elapsed time: ",elt," days.");} else {
    elt<-as.numeric(el_time,units="hours");
    if (elt > 1) {message("#----bootstrapping complete. total elapsed time: ",elt," hours.");} else {
      message("#----bootstrapping complete. total elapsed time: ",as.numeric(el_time,units="mins")," minutes.");
    }
  }

  return(dfr.zcs)
}

# dfr<-calcResampledSizeComps(tbl_strata=dfr.SD,
#                             tbl_hauls=dfr.HD,tbl_indivs=dfr.ID,byEBS=TRUE,
#                             bySex=TRUE,byShellCondition=TRUE,byMaturity=TRUE,
#                             cutpts=cutpts);
