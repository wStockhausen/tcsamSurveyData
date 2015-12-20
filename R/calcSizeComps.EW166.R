#'
#'@title Calculate abundance, size comps size comps by year, east/west of 166W, and other factors from a size comps-by-stratum data frame or csv file.
#'
#'@description This function estimates size comps for abundance, size comps by year, east/west of 166W from a size comps-by-stratum data frame or csv file.
#'
#'@param tbl         : data frame with size comps by stratum info from call to \code{\link{calcSizeComps.ByStratum}} or csv file with size comps by stratum info, or NULL
#'@param strata_toEW166 : data frame w/ conversion from original strata to EW166 strata
#'@param export      : boolean flag to write results to csv file
#'@param out.csv     : output file name
#'@param out.dir     : output file directory 
#'@param verbosity   : integer flag indicating level of printed output (0=off,1=minimal,2=full)
#'
#'@return  Dataframe w/ estimates of abundance, size comps size comps by year, strata as east/west of 166W, and other factors. Columns are \cr
#'\itemize{
#'\item  YEAR            = survey year
#'\item  STRATUM         = 'EAST' or 'WEST' of 166W
#'\item  STRATUM_AREA    = area of stratum
#'\item  other user-defined factors (e.g., sex, shell_condition)
#'\item  SIZE
#'\item  numStations     = number of stations included
#'\item  numHauls        = number of hauls included
#'\item  numNonZeroHauls = number of hauls included
#'\item  numIndivs       = number of individuals sampled
#'\item  totABUNDANCE    = estimated abundance
#'\item  totBIOMASS      = estimated size comps
#'}
#'
#'@details Note: if tbl and in.csv are both NULL, the user is prompted to enter a csv file with size comps by stratum info. \cr
#'Notes: \cr
#'\itemize{
#'\item   Area is in square nautical miles
#'\item   Abundance is in 10^6 individuals
#'\item   Biomass   is in 10^3 mt
#'}
#'
#' @importFrom sqldf sqldf
#' @importFrom wtsUtilities selectFile
#'      
#'@export
#'
calcSizeComps.EW166<-function(tbl=NULL,
                              strata_toEW166=Codes.TrawlSurvey()[["strata.EW166"]],
                              export=TRUE,
                              out.csv='SurveySizeComps.EW166.csv',
                              out.dir=NULL,
                              verbosity=1){
    if (verbosity>1) cat("starting calcSizeComps.EW166\n");
    
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
    if (verbosity>0) cat("Output directory for calcSizeComps.EW166 will be '",out.dir,"'\n",sep='');
    
    
    #determine columns of size comps by stratum table
    nc0f<-9;#number of coulmns if SIZE is the only 'factor' in the table
    cols<-names(tbl); 
    nc<-length(cols);
    if (nc==nc0f){cols<-'';} else 
    {cols<-cols[4:(3+nc-nc0f)];}#extract factor columns (including SIZE)
                                 
    qry<-"select
            t.YEAR,
            s.revd as newSTRATUM,
            t.STRATUM as oldSTRATUM,
            STRATUM_AREA&&cols,
            numStations,
            numHauls,
            numNonZeroHauls,
            numIndivs,
            totABUNDANCE,
            totBIOMASS
          from
            tbl as t,
            strata_toEW166 as s
          where
            t.STRATUM=s.orig
          order by 
            t.YEAR,s.revd,t.STRATUM&&cols;"
    if (nc==9) {
        qry<-gsub("&&cols",'',qry);
    } else {
        qry<-gsub("&&cols",paste(',t.',cols,collapse="",sep=''),qry);
    }
    if (verbosity>1) cat("\nquery is:\n",qry,"\n");
    tbl2<-sqldf(qry);
    
    qry<-"select
            YEAR,
            newSTRATUM as STRATUM,
            sum(STRATUM_AREA) as STRATUM_AREA&&cols,
            sum(numStations) as numStations,
            sum(numHauls) as numHauls,
            -1 as numNonZeroHauls,
            sum(numIndivs) as numIndivs,
            sum(totABUNDANCE) as totABUNDANCE,
            sum(totBIOMASS) as totBIOMASS
          from
            tbl2
          group by 
            YEAR,newSTRATUM&&cols
          order by 
            YEAR,newSTRATUM&&cols;"
    if (nc==9) {
        qry<-gsub("&&cols",'',qry);
    } else {
        qry<-gsub("&&cols",paste(',',cols,collapse="",sep=''),qry);
    }
    if (verbosity>1) cat("\nquery is:\n",qry,"\n");
    tbl1<-sqldf(qry);
                                 
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
    
    if (verbosity>1) cat("finished calcSizeComps.EW166\n");
    return(tbl1)
}
