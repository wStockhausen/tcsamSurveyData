#'
#'@title Calculate EBS-level size comps (abundance and biomass) from a by-stratum data frame or csv file.
#'
#'@description This function calculates total abundance and biomass from a by-stratum data frame or csv file.
#'
#'@param tbl     : data frame with size comps by stratum info from call to [calcSizeComps.ByStratum] or [calcSizeComps.EW166], or a csv file from such a call, or NULL
#'@param in.csv  : csv filename from which to read input dataframe
#'@param export  : boolean flag to write results to csv file
#'@param out.csv : output file name
#'@param out.dir : output file directory
#'@param verbosity : integer flag indicating level of printed output (0=off,1=minimal,2=full)
#'
#'@return data frame with columns: \cr
#'\itemize{
#'\item  YEAR = survey year
#'\item  STRATUM         = 'EBS'
#'\item  STRATUM_AREA    = area of stratum
#'\item  other user-defined factors (e.g., sex, shell_condition)
#'\item  SIZE
#'\item  numStations     = number of stations included
#'\item  numHauls        = number of hauls included
#'\item  numNonZeroHauls = number of hauls included
#'\item  numIndivs       = number of individuals sampled
#'\item  totABUNDANCE = total abundance-by-size estimate
#'\item  totBIOMASS = estimate of total biomass-by-size estimate
#'}
#'
#'@details If \code{tbl} and \code{in.csv} are both NULL, the user is prompted to enter a csv file with biomass by stratum info. \cr
#'\cr Notes: \cr
#'\itemize{
#'\item   Area is in square nautical miles
#'\item   Abundance is in 10^6 individuals
#'\item   Biomass   is in 10^3 mt
#'}
#'
#' @importFrom sqldf sqldf
#' @importFrom utils read.csv write.csv
#' @importFrom wtsUtilities selectFile
#'
#'@export
#'
#######################################################################
calcSizeComps.EBS<-function(tbl=NULL,
                            in.csv=NULL,
                            export=FALSE,
                            out.csv='SurveySizeComps.EBS.csv',
                            out.dir=NULL,
                            verbosity=0){
    if (verbosity>1) cat("starting calcSizeComps.EBS\n");

    in.csv<-NULL;
    if (!is.data.frame(tbl)){
        if (!is.character(tbl)) {
            in.csv<-selectFile(ext="csv",caption="Select csv file with size comps-by-stratum info");
            if (is.null(in.csv)|(in.csv=='')) return(NULL);
        } else {
            in.csv<-tbl;#tbl is a filename
        }
        if (verbosity>1) cat("Reading csv file for size comps-by-stratum info.\n",sep='')
        tbl<-read.csv(in.csv,stringsAsFactors=FALSE);
        if (verbosity>1) cat("Done reading input csv file.\n")
    }

    if (is.null(out.dir)) {
        out.dir<-dirname(file.path('.'));
        if (!is.null(in.csv)) {out.dir<-dirname(file.path(in.csv));}
    }
    if (verbosity>0) cat("Output directory for calcSizeComps.EBS will be '",out.dir,"'\n",sep='');

    #determine columns of size comps by stratum table
    nc0f<-9;#number of coulmns if SIZE is the only 'factor' in the table
    cols<-names(tbl);
    nc<-length(cols);
    if (nc==nc0f){cols<-'';} else
    {cols<-cols[4:(3+nc-nc0f)];}#extract factor columns (including SIZE)

    #
    qry<-"select
            YEAR,
            'EBS' as STRATUM,
            sum(STRATUM_AREA) as STRATUM_AREA&&cols,
            sum(numStations) as numStations,
            sum(numHauls) as numHauls,
            sum(numNonZeroHauls) as numNonZeroHauls,
            sum(numIndivs) as numIndivs,
            sum(totABUNDANCE) as totABUNDANCE,
            sum(totBIOMASS) as totBIOMASS
          from
            tbl
          group by
            YEAR&&cols
          order by
            YEAR&&cols;"
    if (nc==nc0f) {
        qry<-gsub("&&cols",'',qry);
    } else {
        qry<-gsub("&&cols",paste(',',cols,collapse=""),qry);
    }
    if (verbosity>1) cat("\nquery is:\n",qry,"\n");
    tbl1<-sqldf::sqldf(qry);

    idx = tbl1$numNonZeroHauls<0;
    tbl1$numNonZeroHauls[idx] = -1;

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
        write.csv(tbl1,out.csv,na='',row.names=FALSE);
    }

    if (verbosity>1) cat("finished calcSizeComps.EBS\n");
    return(tbl1)
}
