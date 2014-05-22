#'
#'Calculates biomass/abundance for EW166 from a biomass by stratum data frame or csv file.
#'
#'@param tbl   : data frame with biomass by stratum info from call to calcBiomass.ByStratum(...)
#'@param in.csv: csv file with biomass by stratum info from call to calcBiomass.ByStratum(...)
#'@param strata.revd : data frame w/ conversion from original strata to EW166 strata
#'@param export  : boolean flag to write results to csv file
#'@param out.csv : output file name
#'@param out.dir : output file directory 
#'
#'@return   data frame w/ abundance, biomass by year, EW166 strata, factors
#'
#'@details \cr
#'\cr Note: if tbl and in.csv are both NULL, the user is prompted to enter a csv file with biomass by stratum info. \cr
#'Notes: \cr
#'   Abundance is in 10^6 individuals \cr
#'   Biomass   is in 10^3 mt \cr
#'
#' @import sqldf
#' @importFrom tcltk tk_choose.files
#' @importFrom wtsUtilities addFilter
#'      
#'@export
#'
calcBiomass.EW166<-function(tbl=NULL,
                          in.csv=NULL,
                          strata_revd=Codes.TrawlSurvey()[["strata.EW166"]],
                          export=TRUE,
                          out.csv='SurveyBiomass.csv',
                          out.dir=NULL){
    
    if (is.null(tbl)){
        cat("Reading csv file for biomass by stratum info.\n",sep='')
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
        cat("Output directory will be '",out.dir,"'\n",sep='');
        
        tbl<-read.csv(in.csv,stringsAsFactors=FALSE);
        cat("Done reading input csv file.\n")
    }
    
    #determine columns of biomass by stratum table
    cols<-names(tbl); 
    nc<-length(cols);
    if (nc==16){cols<-'';} else 
    {cols<-cols[4:(nc-13)];}#extract factor columns
                                 
    #note: in the sql code below, instr(X,Y) finds the first occurrence of string Y within string X 
    #and returns the number of prior characters plus 1, or 0 if Y is nowhere found within X.
    #So the where clause below tests if stratum_revd.orig is a substring of tbl.STRATUM and, if so,
    #essentially recodes the value in tbl$STRATUM to corresponding value in stratum_revd$revd.
    qry<-"select
            t.YEAR,
            s.revd as STRATUM,
            sum(t.STRATUM_AREA) as STRATUM_AREA&&cols,
            sum(t.numStations) as numStations,
            sum(t.numHauls) as numHauls,
            sum(t.numIndivs) as numIndivs,
            sum(t.totABUNDANCE) as totABUNDANCE,
            sum(t.stdABUNDANCE*stdABUNDANCE) as stdABUNDANCE,
            1.1 as cvABUNDANCE,
            sum(totBIOMASS) as totBIOMASS,
            sum(stdBIOMASS*stdBIOMASS) as stdBIOMASS,
            1.1 as cvBIOMASS
          from
            tbl as t,
            strata_revd as s
          where
            instr(t.STRATUM,s.orig)>0
          group by 
            t.YEAR,s.revd&&cols
          order by 
            t.YEAR,s.revd&&cols;"
    if (nc==16) {
        qry<-gsub("&&cols",'',qry);
    } else {
        qry<-gsub("&&cols",paste(',t.',cols,collapse=""),qry);
    }
    cat("\nquery is:\n",qry,"\n");
    tbl1<-sqldf(qry);
    #convert columns to final values
    tbl1$stdABUNDANCE<-sqrt(tbl1$stdABUNDANCE);#convert from var to stdv
    tbl1$cvABUNDANCE <-tbl1$stdABUNDANCE/tbl1$totABUNDANCE;
    tbl1$stdBIOMASS  <-sqrt(tbl1$stdBIOMASS);  #convert from var to stdv
    tbl1$cvBIOMASS  <-tbl1$stdBIOMASS/tbl1$totBIOMASS;
                                 
    if (export){
        if (!is.null(out.dir)){
            cat("\nTesting existence of folder '",out.dir,"'\n",sep='')
            if (!file.exists(out.dir)){
                cat("Creating folder '",out.dir,"' for output.\n",sep='')
                dir.create(out.dir);
            } else {
                cat("Using folder '",out.dir,"' for output.\n",sep='')
            }
            out.csv<-file.path(out.dir,out.csv)
        }
        write.csv(tbl1,out.csv,na='',row.names=FALSE);
    }
    
    return(tbl1)
}

#bio.EW166.frOS.byH<-calcBiomass.EW166(bio.frOS.byH);
#bio.EW166.frRS.byS<-calcBiomass.EW166(bio.frRS.byS);
