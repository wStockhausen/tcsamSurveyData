#'
#'@title Calculate total abundance and biomass from a by-stratum data frame or csv file.
#'
#'@param tbl     : data frame with abundance/biomass by stratum info from call to \code{\link{calcBiomass.ByStratum}} or calcBiomass.EW166(...)
#'@param in.csv  : csv file with abundance/biomass by stratum info from call to \code{\link{calcBiomass.ByStratum}} or calcBiomass.EW166(...)
#'@param export  : boolean flag to write results to csv file
#'@param out.csv : output file name
#'@param out.dir : output file directory 
#'@param verbosity : integer flag indicating level of printed output (0=off,1=minimal,2=full)
#'
#'@return data frame with columns: \cr
#'\itemize{
#'\item  YEAR = survey year
#'\item  other user-defined factors (e.g., sex, shell_condition)
#'\item  numStations = number of stations included
#'\item  numHauls    = number of hauls included
#'\item  numIndivs   = number of individuals sampled
#'\item  TOT_AREA    = total area included in strata
#'\item  totABUNDANCE = total abundance estimate
#'\item  stdABUNDANCE = std deviation of total abundance estimate
#'\item  cvABUNDANCE  = cv of total abundance estimate
#'\item  totBIOMASS = estimate of total biomass estimate
#'\item  stdBIOMASS = std deviation of total biomass estimate
#'\item  cvBIOMASS  = cv of total biomass estimate
#'}
#'
#'@description This function calculates total abundance and biomass from a by-stratum data frame or csv file.
#'
#'@details If \code{tbl} and \code{in.csv} are both NULL, the user is prompted to enter a csv file with biomass by stratum info. \cr
#'\cr Notes: \cr
#'\itemize{
#'\item   Area is in square nautical miles
#'\item   Abundance is in 10^6 individuals
#'\item   Biomass   is in 10^3 mt
#'}
#'
#' @import sqldf
#' @importFrom tcltk tk_choose.files
#' @importFrom wtsUtilities addFilter
#'      
#'@export
#'
#######################################################################
calcBiomass.EBS<-function(tbl=NULL,
                          in.csv=NULL,
                          export=TRUE,
                          out.csv='SurveyBiomass.csv',
                          out.dir=NULL,
                          verbosity=1){    
    if (verbosity>1) cat("starting calcBiomass.EBS\n");
    
    if (is.null(tbl)){
        if (is.null(in.csv)) {
            Filters<-addFilter("csv","csv files (*.csv)","*.csv");
            in.csv<-tk_choose.files(caption=paste("Select csv file with biomass by stratum info"),
                                    multi=FALSE,filters=matrix(Filters[c("csv"),],1,2,byrow=TRUE));
            if (is.null(in.csv)|(in.csv=='')) return(NULL);
        }
        if (is.null(out.dir)) {
            out.dir<-dirname(file.path('.'));
            if (!is.null(in.csv)) {out.dir<-dirname(file.path(in.csv));}
        }
        if (verbosity>0) cat("Output directory for calcBiomass.EBS will be '",out.dir,"'\n",sep='');
        
        if (verbosity>1) cat("Reading csv file for biomass by stratum info.\n",sep='')
        tbl<-read.csv(in.csv,stringsAsFactors=FALSE);
        if (verbosity>1) cat("Done reading input csv file.\n")
    }
    
    #determine columns of biomass by stratum table
    cols<-names(tbl); 
    if (any(cols=='avgNUMCPUE')) {nc0f<-16;} else {nc0f<-12;}
    nc<-length(cols);
    if (nc==nc0f){cols<-'';} else 
    {cols<-cols[4:(3+nc-nc0f)];}#extract factor columns
                                 
    #
    qry<-"select
            YEAR&&cols,
            sum(numStations) as numStations,
            sum(numHauls) as numHauls,
            sum(numIndivs) as numIndivs,
            sum(STRATUM_AREA) as TOT_AREA,
            sum(totABUNDANCE) as totABUNDANCE,
            sum(stdABUNDANCE*stdABUNDANCE) as stdABUNDANCE,
            1.1 as cvABUNDANCE,
            sum(totBIOMASS) as totBIOMASS,
            sum(stdBIOMASS*stdBIOMASS) as stdBIOMASS,
            1.1 as cvBIOMASS
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
    tbl1<-sqldf(qry);
    #convert columns to final values
    tbl1$stdABUNDANCE<-sqrt(tbl1$stdABUNDANCE);#convert from var to stdv
    tbl1$cvABUNDANCE <-tbl1$stdABUNDANCE/tbl1$totABUNDANCE;
    tbl1$stdBIOMASS  <-sqrt(tbl1$stdBIOMASS);  #convert from var to stdv
    tbl1$cvBIOMASS   <-tbl1$stdBIOMASS/tbl1$totBIOMASS;
                                 
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
    
    if (verbosity>1) cat("finished calcBiomass.EBS\n");
    return(tbl1)
}

#tbl.totBio.frBBS<-calcBiomass.EBS(tbl.BiomassByStratum)
#tbl.totBio.frEW<-calcBiomass.EBS(tbl.EW166Biomass)