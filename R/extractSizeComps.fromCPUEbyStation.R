#'
#'@title Extract size compositions from size-specific CPUE data by station
#'
#'@param tbl_cpue : data frame from call to \code{\link{calcCPUE.ByStation}} or filename for csv file
#'@param cutpts   : vector of cutpoints to create size bins from
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
#'\item  avgNumCPUE = average cpue-by-size (numbers/sq. nautical mile) by stratum
#'\item  stdNumCPUE = standard deviation in cpue-by-size (numbers/sq. nautical mile) by stratum
#'\item  cvNumCPUE = cv in  cpue-by-size (numbers/sq. nautical mile) by stratum
#'\item  avgWgtCPUE = estimated cpue-by-size (mt/sq. nautical mile) by stratum
#'\item  stdWgtCPUE = standard deviation in cpue-by-size (mt/sq. nautical mile) by stratum
#'\item  cvWgtCPUE = cv in  cpue-by-size (mt/sq. nautical mile) by stratum
#'}
#'
#' @importFrom sqldf sqldf
#' @importFrom wtsUtilities selectFile
#'
#'@export
#'
#######################################################################
extractSizeComps.fromCPUEbyStation<-function(
                                    tbl_cpue=NULL,
                                    cutpts=seq(from=25,to=185,by=5),
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

    #now expand to all sizes
    tbl_ufacs<-subset(tbl_cpue,select=-c(SIZE,numIndivs,numCPUE,wgtCPUE));
    ucols<-names(tbl_ufacs);
    if (verbosity>1) cat("ucols:",ucols,"\n");
    nf<-length(ucols)-7;#number of factors other than SIZE
    qry<-"select distinct
            &&ucols
          from tbl_ufacs;";
    qry<-gsub("&&ucols",paste(ucols,collapse=","),qry)
    if (verbosity>1) cat(qry,"\n");
    tbl_ufacs<-sqldf(qry);
    if (verbosity>1) print(head(tbl_ufacs));

    tbl_zs<-as.data.frame(list(SIZE=cutpts[1:(length(cutpts)-1)]))
    qry<-"select * from tbl_ufacs, tbl_zs;";
    tbl_uzfacs<-sqldf(qry);
    if (verbosity>1) print(head(tbl_uzfacs));

    #rearrange column names to get SIZE at end of other factors (if any)
    nms<-names(tbl_uzfacs);
    if (verbosity>1) cat("nms:",nms,"\n");
    nc<-length(nms);
    nmsp<-c(nms[1:(5+nf)],nms[nc],nms[(6+nf):(nc-1)]);
    if (verbosity>1) cat("nmsp:",nmsp,"\n");

    ucols<-paste("u",nmsp,sep='.');
    zcols<-paste("z",nmsp,sep='.');
    ucolstr<-paste(ucols,collapse=",")
    ncols<-length(ucols);
    joinConds<-paste(ucols,zcols,sep='=',collapse=' and ');

    qry<-"select
            &&ucols,z.numIndivs,
            z.numCPUE,z.wgtCPUE
          from
            tbl_uzfacs u left join
            tbl_cpue z
          on
            &&joinConds
          order by
            &&ucols;"
    qry<-gsub("&&ucols",ucolstr,qry);
    qry<-gsub("&&joinConds",joinConds,qry);
    if (verbosity>0) cat(qry,'\n')
    tbl_zcs<-sqldf(qry);

    #change NAs to 0s in formerly missing cells
    idx<-is.na(tbl_zcs$numIndivs);
    tbl_zcs$numIndivs[idx] <-0;
    tbl_zcs$numCPUE[idx]   <-0;
    tbl_zcs$wgtCPUE[idx]   <-0;

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

