#'
#'@title Extract size compositions from size-specific CPUE data by station
#'
#'@param tbl_cpue : data frame from call to \code{\link{calcCPUE.ByStation}} or filename for csv file
#'@param cutpts   : vector of cutpoints to create size bins from
#'@param truncate.low  : flag (T/F) to exclude individuals smaller than minSize
#'@param truncate.high : flag (T/F) to exclude individuals larger than maxSize
#'@param export   : boolean flag to write results to csv file
#'@param out.csv  : output file name
#'@param out.dir  : output file directory
#'@param verbosity : integer flag indicating level of printed output (0=off,1=minimal,2=full)
#'
#'@description This function extracts size compositions from size-specific CPUE data by NMFS station.
#'
#'@details If tbl_cpue is not a dataframe, it can be a cpue csv file or NULL.\cr
#'\cr
#'Other notes: \cr
#'\itemize{
#'   \item{Area is in square nautical miles}
#'   \item{avgNumCPUE is in indivs/square nautical mile}
#'   \item{avgBioCPUE is in mt/square nautical mile}
#'}
#'
#'@return data frame with size comps by station. Columns are \cr
#'\itemize{
#'\item  YEAR
#'\item  STRATUM
#'\item  STRATUM_AREA
#'\item  SEX
#'\item  MATURITY
#'\item  SHELL_CONDITION
#'\item  SIZE
#'\item  numStations
#'\item  numHauls
#'\item  numIndivs
#'\item  avgNumCPUE = average cpue-by-size (numbers/sq. nautical mile) by stratum
#'\item  stdNumCPUE = standard deviation in cpue-by-size (numbers/sq. nautical mile) by stratum
#'\item  cvNumCPUE = cv in  cpue-by-size (numbers/sq. nautical mile) by stratum
#'\item  avgWgtCPUE = estimated cpue-by-size (mt/sq. nautical mile) by stratum
#'\item  stdWgtCPUE = standard deviation in cpue-by-size (mt/sq. nautical mile) by stratum
#'\item  cvWgtCPUE = cv in  cpue-by-size (mt/sq. nautical mile) by stratum
#'}
#'
#' @importFrom sqldf sqldf
#' @importFrom utils head
#' @importFrom wtsUtilities selectFile
#'
#'@export
#'
#######################################################################
extractSizeComps.fromCPUEbyStation<-function(
                                    tbl_cpue=NULL,
                                    cutpts=seq(from=25,to=185,by=5),
                                    truncate.low=TRUE,
                                    truncate.high=FALSE,
                                    export=FALSE,
                                    out.csv='SurveySizeComps.CPUE.ByStratum.csv',
                                    out.dir=NULL,
                                    verbosity=0){
    if (verbosity>0) cat("starting extractSizeComps.fromCPUEbyStation.\n");

    in.csv<-NULL;
    if (!is.data.frame(tbl_cpue)){
        if (verbosity>1) cat("tbl_cpue is not a dataframe\n")
        if (!is.character(tbl_cpue)){
            in.csv<-wtsUtilities::selectFile(ext="csv",caption="Select csv file with size-specific CPUE-by-station info");
            if (is.null(in.csv)|(in.csv=='')) return(NULL);
        } else {
            in.csv<-tbl_cpue;#tbl is a filename
        }
        if (verbosity>1) cat("reading cpue file\n")
        if (is.null(out.dir)) out.dir<-dirname(in.csv);
        if (verbosity>1) cat("Reading cpue file.\n",sep='')
        tbl_cpue<-read.csv(in.csv,stringsAsFactors=FALSE);
        if (verbosity>1) cat("Done reading input csv file.\n")
    }#read in or created tbl_cpue

    #bin current sizes to new size bins
    ##expand cutpts to truncate or not
    nCtPts<-length(cutpts);
    ctpts.tmp<-cutpts;
    if (!truncate.low ) ctpts.tmp[1]<-0;
    if (!truncate.high) ctpts.tmp[nCtPts]<-Inf;
    ##apply cutpts to sizes
    cuts<-cut(tbl_cpue$SIZE,ctpts.tmp,right=FALSE,labels=FALSE)
    tbl_cpue$SIZE<-cutpts[cuts];
    tbl_cpue<-tbl_cpue[!is.na(tbl_cpue$SIZE),];

    #now expand to all sizes
    ##get table with unique factors other than size
    qry<-"select distinct
            YEAR,STRATUM,GIS_STATION,LONGITUDE,LATITUDE,
            SEX,MATURITY,SHELL_CONDITION,numHauls
          from tbl_cpue
          order by
            YEAR,STRATUM,GIS_STATION,LONGITUDE,LATITUDE,
            SEX,MATURITY,SHELL_CONDITION,numHauls;";
    tbl_ufacs<-sqldf::sqldf(qry);
    ##get table with sizes defined by lower cutpts
    tbl_uzs<-data.frame(SIZE=cutpts[1:(length(cutpts)-1)]);
    ##combine them
    qry<-"select
            YEAR,STRATUM,GIS_STATION,LONGITUDE,LATITUDE,
            SEX,MATURITY,SHELL_CONDITION,SIZE,numHauls
          from tbl_ufacs,tbl_uzs
          order by
            YEAR,STRATUM,GIS_STATION,LONGITUDE,LATITUDE,
            SEX,MATURITY,SHELL_CONDITION,SIZE,numHauls;";
    tbl_uzfacs<-sqldf::sqldf(qry);
    if (verbosity>1) print(utils::head(tbl_uzfacs));

    #expand tbl_cpue to include missing sizes
    qry<-"select
            u.YEAR,u.STRATUM,u.GIS_STATION,u.LONGITUDE,u.LATITUDE,
            u.SEX,u.MATURITY,u.SHELL_CONDITION,u.SIZE,u.numHauls,
            z.numNonZeroHauls,z.numIndivs,
            z.numCPUE,z.wgtCPUE
          from
            tbl_uzfacs u left join
            tbl_cpue z
          on
            u.YEAR=z.YEAR and u.STRATUM=z.STRATUM and u.GIS_STATION=z.GIS_STATION and
            u.SEX=z.SEX and u.MATURITY=z.MATURITY and u.SHELL_CONDITION=z.SHELL_CONDITION and
            u.SIZE=z.SIZE
          order by
            u.YEAR,u.STRATUM,u.GIS_STATION,u.LONGITUDE,u.LATITUDE,
            u.SEX,u.MATURITY,u.SHELL_CONDITION,u.SIZE;"
    # qry<-gsub("&&ucols",ucolstr,qry);
    # qry<-gsub("&&joinConds",joinConds,qry);
    # if (verbosity>0) cat(qry,'\n')
    tbl_zcs<-sqldf::sqldf(qry);

    #change NAs to 0s in formerly missing cells
    idx<-is.na(tbl_zcs$numIndivs);
    tbl_zcs$numNonZeroHauls[idx] <-0;
    tbl_zcs$numIndivs[idx]       <-0;
    tbl_zcs$numCPUE[idx]         <-0;
    tbl_zcs$wgtCPUE[idx]         <-0;

    #finally, sum within new size bins
    qry<-"select
            YEAR,STRATUM,GIS_STATION,LONGITUDE,LATITUDE,
            SEX,MATURITY,SHELL_CONDITION,SIZE,numHauls,
            sum(numIndivs) as numIndivs,
            sum(numCPUE) as numCPUE,
            sum(wgtCPUE) as wgtCPUE
          from
            tbl_zcs
          group by
            YEAR,STRATUM,GIS_STATION,LONGITUDE,LATITUDE,
            SEX,MATURITY,SHELL_CONDITION,SIZE,numHauls
          order by
            YEAR,STRATUM,GIS_STATION,LONGITUDE,LATITUDE,
            SEX,MATURITY,SHELL_CONDITION,SIZE;"
    tbl_zcs<-sqldf::sqldf(qry);

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
        write.csv(tbl_zcs,out.csv,na='',row.names=FALSE);
    }

    if (verbosity>1) cat("finished extractSizeComps.fromCPUEbyStation\n");
    return(tbl_zcs);
}

