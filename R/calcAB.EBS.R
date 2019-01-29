#'
#'@title Calculate total abundance and biomass from a by-stratum data frame or csv file.
#'
#'@param tbl     : data frame with abundance/biomass by stratum info from call to \code{\link{calcAB.ByStratum} or \link{calcAB.EW166}}, or a csv file from such a call, or NULL
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
#'\item  numStations     = number of stations included
#'\item  numHauls        = number of hauls included
#'\item  numNonZeroHauls = number of hauls included
#'\item  numIndivs       = number of individuals sampled
#'\item  totABUNDANCE = total abundance estimate
#'\item  stdABUNDANCE = std deviation of total abundance estimate
#'\item  cvABUNDANCE  = cv of total abundance estimate
#'\item  totBIOMASS = estimate of total biomass estimate
#'\item  stdBIOMASS = std deviation of total biomass estimate
#'\item  cvBIOMASS  = cv of total biomass estimate
#'}
#'
#'@description This function calculates total EBS abundance and biomass from a by-stratum data frame or csv file.
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
#' @importFrom wtsUtilities selectFile
#'
#'@export
#'
#######################################################################
calcAB.EBS<-function(tbl=NULL,
                          in.csv=NULL,
                          export=TRUE,
                          out.csv='SurveyBiomass.EBS.csv',
                          out.dir=NULL,
                          verbosity=0){
    if (verbosity>1) cat("starting calcAB.EBS\n");

    in.csv<-NULL;
    if (!is.data.frame(tbl)){
        if (!is.character(tbl)) {
            in.csv<-selectFile(ext="csv",caption="Select csv file with biomas-by-stratum info");
            if (is.null(in.csv)|(in.csv=='')) return(NULL);
        } else {
            in.csv<-tbl;#tbl is a filename
        }
        if (verbosity>1) cat("Reading csv file for biomass-by-stratum info.\n",sep='')
        tbl<-read.csv(in.csv,stringsAsFactors=FALSE);
        if (verbosity>1) cat("Done reading input csv file.\n")
    }

    if (is.null(out.dir)) {
        out.dir<-dirname(file.path('.'));
        if (!is.null(in.csv)) {out.dir<-dirname(file.path(in.csv));}
    }
    if (verbosity>0) cat("Output directory for calcAB.EBS will be '",out.dir,"'\n",sep='');

    #determine factor column names in tbl
    cols<-names(tbl);
    nonFacs<-c("YEAR","STRATUM","STRATUM_AREA","numStations","numHauls","numNonZeroHauls","numIndivs",
               "totABUNDANCE","stdABUNDANCE","cvABUNDANCE","totBIOMASS","stdBIOMASS","cvBIOMASS");
    facs<-cols[!(cols %in% nonFacs)]; #extract factor column names

    #
    qry<-"select
            YEAR,
            'EBS' as STRATUM,
            sum(STRATUM_AREA) as STRATUM_AREA&&facs,
            sum(numStations) as numStations,
            sum(numHauls) as numHauls,
            sum(numNonZeroHauls) as numNonZeroHauls,
            sum(numIndivs) as numIndivs,
            sum(totABUNDANCE) as totABUNDANCE,
            sum(stdABUNDANCE*stdABUNDANCE) as stdABUNDANCE,
            1.1 as cvABUNDANCE,
            sum(totBIOMASS) as totBIOMASS,
            sum(stdBIOMASS*stdBIOMASS) as stdBIOMASS,
            1.1 as cvBIOMASS
          from
            tbl
          group by
            YEAR&&facs
          order by
            YEAR&&facs;"
    if (length(facs)==0) {
        qry<-gsub("&&facs",'',qry);
    } else {
        qry<-gsub("&&facs",paste(',',facs,collapse=""),qry);
    }
    if (verbosity>1) cat("\nquery is:\n",qry,"\n");
    tbl1<-sqldf(qry);
    #convert columns to final values
    tbl1$stdABUNDANCE<-sqrt(tbl1$stdABUNDANCE);#convert from var to stdv
    tbl1$cvABUNDANCE <-tbl1$stdABUNDANCE/tbl1$totABUNDANCE;
    idx<-is.nan(tbl1$cvABUNDANCE);
    tbl1$cvABUNDANCE[idx]<-0;

    tbl1$stdBIOMASS  <-sqrt(tbl1$stdBIOMASS);  #convert from var to stdv
    tbl1$cvBIOMASS   <-tbl1$stdBIOMASS/tbl1$totBIOMASS;
    idx<-is.nan(tbl1$cvBIOMASS);
    tbl1$cvBIOMASS[idx]<-0;

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

    if (verbosity>1) cat("finished calcAB.EBS\n");
    return(tbl1)
}
