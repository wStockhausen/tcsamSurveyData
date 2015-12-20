#'
#'@title Calculate abundance, biomass by year, east/west of 166W, and other factors from a biomass-by-stratum data frame or csv file.
#'
#'@description This function estimates abundance, biomass by year, east/west of 166W from a biomass-by-stratum data frame or csv file.
#'
#'@param tbl         : data frame with biomass by stratum info from call to \code{\link{calcBiomass.ByStratum}} or csv file with biomass by stratum info, or NULL
#'@param strata_toEW166 : data frame w/ conversion from original strata to EW166 strata
#'@param export      : boolean flag to write results to csv file
#'@param out.csv     : output file name
#'@param out.dir     : output file directory 
#'@param verbosity   : integer flag indicating level of printed output (0=off,1=minimal,2=full)
#'
#'@return  Dataframe w/ estimates of abundance, biomass by year, strata as east/west of 166W, and other factors. Columns are \cr
#'\itemize{
#'\item  YEAR            = survey year
#'\item  STRATUM         = 'EAST' or 'WEST' of 166W
#'\item  STRATUM_AREA    = area of stratum
#'\item  other user-defined factors (e.g., sex, shell_condition)
#'\item  numStations     = number of stations included
#'\item  numHauls        = number of hauls included
#'\item  numNonZeroHauls = number of hauls included
#'\item  numIndivs       = number of individuals sampled
#'\item  totABUNDANCE    = estimated abundance
#'\item  stdABUNDANCE    = std deviation of estimated abundance
#'\item  cvABUNDANCE     = cv of estiamted abundance
#'\item  totBIOMASS      = estimated biomass
#'\item  stdBIOMASS      = std deviation of estimated biomass 
#'\item  cvBIOMASS       = cv of estimated biomass
#'}
#'
#'@details Note: if tbl and in.csv are both NULL, the user is prompted to enter a csv file with biomass by stratum info. \cr
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
calcBiomass.EW166<-function(tbl=NULL,
                          strata_toEW166=Codes.TrawlSurvey()[["strata.EW166"]],
                          export=TRUE,
                          out.csv='SurveyBiomass.EW166.csv',
                          out.dir=NULL,
                          verbosity=1){
    if (verbosity>1) cat("starting calcBiomass.EW166\n");
    
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
    if (verbosity>0) cat("Output directory for calcBiomass.EW166 will be '",out.dir,"'\n",sep='');
    
    #determine columns of biomass by stratum table
    cols<-names(tbl); 
    nc<-length(cols);
    nc0f<-13;
    if (nc==nc0f){facs<-'';} else 
    {facs<-cols[4:(3+nc-nc0f)];}#extract factor columns
                                 
    qry<-"select
            t.YEAR,
            s.revd as newSTRATUM,
            t.STRATUM as oldSTRATUM,
            STRATUM_AREA&&facs,
            numStations,
            numHauls,
            numNonZeroHauls,
            numIndivs,
            totABUNDANCE,
            stdABUNDANCE,
            totBIOMASS,
            stdBIOMASS
          from
            tbl as t,
            strata_toEW166 as s
          where
            t.STRATUM=s.orig
          order by 
            t.YEAR,s.revd,t.STRATUM&&facs;"
    if (nc==13) {
        qry<-gsub("&&facs",'',qry);
    } else {
        qry<-gsub("&&facs",paste(',t.',facs,collapse="",sep=''),qry);
    }
    if (verbosity>1) cat("\nquery is:\n",qry,"\n");
    tbl2<-sqldf(qry);
    
    qry<-"select
            YEAR,
            newSTRATUM as STRATUM,
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
            tbl2
          group by 
            YEAR,newSTRATUM&&facs
          order by 
            YEAR,newSTRATUM&&facs;"
    if (nc==17) {
        qry<-gsub("&&facs",'',qry);
    } else {
        qry<-gsub("&&facs",paste(',',facs,collapse="",sep=''),qry);
    }
    if (verbosity>1) cat("\nquery is:\n",qry,"\n");
    tbl1<-sqldf(qry);
    
    #convert columns to final values
    tbl1$stdABUNDANCE<-sqrt(tbl1$stdABUNDANCE);#convert from var to stdv
    tbl1$cvABUNDANCE <-tbl1$stdABUNDANCE/tbl1$totABUNDANCE;
    idx<-is.nan(tbl1$cvABUNDANCE);
    tbl1$cvABUNDANCE[idx]<-0; 
    
    tbl1$stdBIOMASS  <-sqrt(tbl1$stdBIOMASS);  #convert from var to stdv
    tbl1$cvBIOMASS  <-tbl1$stdBIOMASS/tbl1$totBIOMASS;
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
    
    if (verbosity>1) cat("finished calcBiomass.EW166\n");
    return(tbl1)
}

#bio.EW166.frOS.byH<-calcBiomass.EW166(bio.frOS.byH);
#bio.EW166.frRS.byS<-calcBiomass.EW166(bio.frRS.byS);
