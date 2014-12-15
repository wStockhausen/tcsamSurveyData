#'
#'@title Calculate size compositions by stratum from AFSC trawl survey data.
#'
#'@param tbl_strata : data frame from call to \code{\link{selectStrata.TrawlSurvey}} [required]
#'@param tbl_cpue   : data frame from call to \code{\link{calcCPUE.ByHaul}} or \code{\link{calcCPUE.ByStation}}, or filename for csv file, or NULL
#'@param tbl_hauls  : dataframe from call to \code{\link{selectHauls.TrawlSurvey}} [required only if tbl_cpue not given]
#'@param tbl_indivs : dataframe from call to \code{\link{selectIndivs.TrawlSurvey}} (or crab survey filename, or NULL) [required only if tbl_cpue not given]
#'@param avgHaulsByStation : flag (T/F) to average hauls by station before calc'ing size comps
#'@param bySex            : flag (T/F) to calc by sex
#'@param byShellCondition : flag (T/F) to calc by shell condition
#'@param byMaturity       : flag (T/F) to calc by maturity state
#'@param cutpts        : vector of cutpoints to create size bins from
#'@param truncate.low  : flag (T/F) to exclude individuals smaller than minSize
#'@param truncate.high : flag (T/F) to exclude individuals larger than maxSize
#'@param Years      - vector of survey years to include in hauls             (ignored if NULL)
#'@param HaulTypes  - vector of haul types to include in hauls               (ignored if NULL)
#'@param YearRange  - vector of min, max survey years to include in hauls    (ignored if NULL)
#'@param DepthRange - vector of min, max haul depths to include in hauls     (ignored if NULL)
#'@param LatRange   - vector of min, max haul latitudes to include in hauls  (ignored if NULL)
#'@param LonRange   - vector of min, max haul longitudes to include in hauls (ignored if NULL)
#'@param col.Size        : name of tbl_indivs column containing size (CL or CW) information
#'@param sex             : one of 'MALE','FEMALE' or 'ALL' for narrowing selection of individuals
#'@param shell_condition : one of 'NEW_SHELL','OLD_SHELL' or 'ALL' for narrowing selection of individuals
#'@param maturity        : one of 'IMMATURE','MATURE' or 'ALL' for narrowing selection of individuals
#'@param calcMaleMaturity: flag (T/F) to calculate pr(mature|size) for males based on an ogive
#'@param minSize         : minimum size (width) of individuals to select 
#'@param maxSize         : maximum size (width) of individuals to select 
#'@param export  : boolean flag to write results to csv file
#'@param out.csv : output file name
#'@param out.dir : output file directory 
#'@param verbosity : integer flag indicating level of printed output (0=off,1=minimal,2=full)
#'
#'@description This function calculates size compositions by stratum from cpue (by survey station or by haul).
#'
#'@details If tbl_cpue is not a dataframe, it can be a cpue csv file or NULL. If it is NULL,
#'then tbl_hauls (tbl_indivs) must either be: 1) a dataframe of hauls (indivs); 2) a filename
#'for a hauls (indivs), or 3) NULL (in which case the user may select the csv file using 
#'a dialog box). \cr
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
#'\item  YEAR
#'\item  STRATUM
#'\item  STRATUM_AREA
#'\item  other user-defined factors (e.g., sex, shell_condition)
#'\item  SIZE
#'\item  numStations
#'\item  numHauls
#'\item  numNonZeroHauls
#'\item  numIndivs
#'\item  totABUNDANCE = estimated abundance-by-size (by stratum)
#'\item  totBIOMASS = estimated biomass-by-size (by stratum)
#'}
#'
#' @importFrom sqldf sqldf
#' @importFrom wtsUtilities selectFile
#'
#'@export
#'
#######################################################################
calcSizeComps.ByStratum<-function(tbl_strata,
                                  tbl_cpue=NULL,
                                  tbl_hauls=NULL,
                                  tbl_indivs=NULL,
                                  avgHaulsByStation=FALSE,
                                  bySex=FALSE,
                                  byShellCondition=FALSE,
                                  byMaturity=FALSE,
                                  cutpts=seq(from=25,to=185,by=5),
                                  truncate.low=TRUE,
                                  truncate.high=FALSE,
                                  Years=NULL,
                                  HaulTypes=NULL,
                                  YearRange=NULL,
                                  DepthRange=NULL,
                                  LatRange=NULL,
                                  LonRange=NULL,
                                  col.Size='WIDTH',
                                  sex=c('MALE','FEMALE','ALL'),
                                  shell_condition=c('NEW_SHELL','OLD_SHELL','ALL'),
                                  maturity=c('IMMATURE','MATURE','ALL'),
                                  calcMaleMaturity=FALSE,
                                  minSize=-Inf,
                                  maxSize=Inf,
                                  export=FALSE,
                                  out.csv='SurveySizeComps.ByStratum.csv',
                                  out.dir=NULL,
                                  verbosity=1){
    if (verbosity>1) cat("starting calcSizeComps.ByStratum\n");
    
    if (!is.data.frame(tbl_strata)) {
        cat("Error in calcSizeComps.ByStratum:",
            "tbl_strata is NULL. Must supply tbl_strata.",
            "Aborting...",sep='\n');
        return(NULL);
    }
    
    in.csv<-NULL;
    if (!is.data.frame(tbl_cpue)){
        cat("tbl_cpue is not a dataframe\n")
        if (is.character(tbl_cpue)){
            cat("reading cpue file\n")
            if (is.null(out.dir)) out.dir<-dirname(tbl_cpue);
            if (verbosity>1) cat("Reading cpue file.\n",sep='')
            tbl<-read.csv(in.csv,stringsAsFactors=FALSE);
            if (verbosity>1) cat("Done reading input csv file.\n")
        } else {
            cat("creating tbl_cpue from hauls and indivs info\n")
            if (!is.data.frame(tbl_hauls)){
                cat("tbl_hauls is not a dataframe\n")
                if (!is.character(tbl_hauls)) {
                    in.csv<-wtsUtilities::selectFile(ext="csv",caption="Select csv file with haul info");
                    if (is.null(in.csv)|(in.csv=='')) return(NULL);
                } else {
                    in.csv<-tbl_hauls;#tbl_hauls is a filename
                }
                tbl_hauls<-selectHauls.TrawlSurvey(tbl_strata,
                                                   tbl=in.csv,
                                                   export=FALSE,
                                                   Years=Years,
                                                   HaulTypes=HaulTypes,
                                                   YearRange=YearRange,
                                                   DepthRange=DepthRange,
                                                   LatRange=LatRange,
                                                   LonRange=LonRange,
                                                   verbosity=verbosity);
                if (is.null(out.dir)) {
                    out.dir<-dirname(file.path('.'));
                    if (!is.null(in.csv)) {out.dir<-dirname(file.path(in.csv));}
                    if (verbosity>0) cat("Output directory for calcSizeComps.ByStratum will be '",out.dir,"'\n",sep='');
                }
            }#creating tbl_hauls
            if (!is.data.frame(tbl_indivs)){
                cat("tbl_indivs is not a dataframe\n")
                if (!is.character(tbl_indivs)) {
                    in.csv<-wtsUtilities::selectFile(ext="csv",caption="Select csv file with indivs info");
                    if (is.null(in.csv)|(in.csv=='')) return(NULL);
                } else {
                    in.csv<-tbl_indivs;#tbl_indivs is a filename
                }
                tbl_indivs<-selectIndivs.TrawlSurvey(tbl_hauls,
                                                     tbl=in.csv,
                                                     col.Size=col.Size,
                                                     export=FALSE,
                                                     sex=sex,
                                                     shell_condition=shell_condition,
                                                     maturity=maturity,
                                                     calcMaleMaturity=calcMaturity,
                                                     minSize=minSize,
                                                     maxSize=maxSize,
                                                     verbosity=verbosity)
                if (is.null(out.dir)) {
                    out.dir<-dirname(file.path('.'));
                    if (!is.null(in.csv)) {out.dir<-dirname(file.path(in.csv));}
                    if (verbosity>0) cat("Output directory for calcSizeComps.ByStratum will be '",out.dir,"'\n",sep='');
                }
            }#creating tbl_cpue
            cat("creating tbl_cpue\n")
            tbl_cpue<-calcCPUE.ByHaul(tbl_hauls,
                                      tbl_indivs=tbl_indivs,
                                      bySex=bySex,
                                      byShellCondition=byShellCondition,
                                      byMaturity=byMaturity,
                                      bySize=TRUE,
                                      cutpts=cutpts,
                                      truncate.low=truncate.low,
                                      truncate.high=truncate.high,
                                      verbosity=verbosity);
            if (avgHaulsByStation) {
                cat("averaging cpue by station\n")
                tbl_cpue<-calcCPUE.ByStation(tbl_strata,tbl_cpue);
            }
        }#creating tbl_cpue
    }#read in or created tbl_cpue
    
    #Now calculate size comps
    tbl_zcs<-calcBiomass.ByStratum(tbl_strata,tbl_cpue=tbl_cpue,export=FALSE,verbosity=0);
    #Drop lots of columns
    tbl_zcs<-subset(tbl_zcs,select=-c(stdABUNDANCE,cvABUNDANCE,stdBIOMASS,cvBIOMASS))
    
    #now expand to all sizes
    tbl_ufacs<-subset(tbl_zcs,select=-c(SIZE,numIndivs,totABUNDANCE,totBIOMASS));
    ucols<-names(tbl_ufacs);
    qry<-"select distinct
            &&ucols,
            -1 as numNonZeroHauls
          from tbl_ufacs;";
    qry<-gsub("&&ucols",paste(ucols[1:(length(ucols)-1)],collapse=","),qry)
    tbl_ufacs<-sqldf::sqldf(qry);
    
    tbl_zs<-as.data.frame(list(SIZE=cutpts[1:(length(cutpts)-1)]))
    qry<-"select * from tbl_ufacs, tbl_zs;";
    tbl_uzfacs<-sqldf::sqldf(qry);
    
    #rearrange column names to get SIZE at end of other factors (if any)
    nms<-names(tbl_uzfacs);
    nc<-length(nms);
    nmsp<-c(nms[1:(nc-4)],nms[nc],nms[(nc-3):(nc-1)]);
    
    ucols<-paste("u",nmsp,sep='.');
    zcols<-paste("z",nmsp,sep='.');
    ucolstr<-paste(ucols,collapse=",")
    ncols<-length(ucols);
    joinConds<-paste(ucols[1:(ncols-3)],zcols[1:(ncols-3)],sep='=',collapse=' and ');
    
    qry<-"select
            &&ucols,z.numIndivs,z.totABUNDANCE,z.totBIOMASS
          from
            tbl_uzfacs u left join
            tbl_zcs z
          on
            &&joinConds
          order by
            &&ucols;"
    qry<-gsub("&&ucols",ucolstr,qry);
    qry<-gsub("&&joinConds",joinConds,qry);
    cat(qry,'\n')
    tbl_zcs1<-sqldf::sqldf(qry);
    
    #change NAs to 0s in formerly missing cells
    idx<-is.na(tbl_zcs1$numIndivs);
    tbl_zcs1$numIndivs[idx]<-0;
    tbl_zcs1$totABUNDANCE[idx]<-0;
    tbl_zcs1$totBIOMASS[idx]<-0;
            
    if (export){
        if (!is.null(out.dir)){
            if (verbosity>1) cat("\nTesting existence of folder '",out.dir,"'\n",sep='')
            if (!file.exists(out.dir)){
                if (verbosity>0) cat("Creating folder '",out.dir,"' for output.\n",sep='')
                dir.create(out.dir);
            } else {
                if (verbosity>0) cat("Using folder '",out.dir,"' for output.\n",sep='')
            }
            out.csv<-file.path(out.dir,out.csv)
        }
        write.csv(tbl_zcs1,out.csv,na='',row.names=FALSE);
    }
    
    if (verbosity>1) cat("finished calcSizeComps.ByStratum\n");
    return(tbl_zcs1);
}

# tbl_zcs<-calcSizeComps.ByStratum(strata.org,
#                                  tbl_hauls=tbl.hauls,
#                                  tbl_indivs=tbl.indivs,
#                                  bySex=TRUE,
#                                  byShellCondition=TRUE,
#                                  byMaturity=TRUE,
#                                  export=TRUE)