#'
#'@title Function to calculate total biomass from a biomass-by-stratum data frame or csv file.
#'
#'@param tbl     : data frame with biomass by stratum info from call to calcBiomass.ByStratum(...) or calcBiomass.EW166(...)
#'@param in.csv  : csv file with biomass by stratum info from call to calcBiomass.ByStratum(...) or calcBiomass.EW166(...)
#'@param export  : boolean flag to write results to csv file
#'@param out.csv : output file name
#'@param out.dir : output file directory 
#'
#'@return data frame w/ abundance, biomass by year
#'
#'@details \cr
#'Note: if tbl and in.csv are both NULL, the user is prompted to enter a csv file with biomass by stratum info \cr
#'Notes: \cr
#'   Abundance is in 10^6 individuals  \cr
#'   Biomass   is in 10^3 mt \cr
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
    cat("\nquery is:\n",qry,"\n");
    tbl1<-sqldf(qry);
    #convert columns to final values
    tbl1$stdABUNDANCE<-sqrt(tbl1$stdABUNDANCE);#convert from var to stdv
    tbl1$cvABUNDANCE <-tbl1$stdABUNDANCE/tbl1$totABUNDANCE;
    tbl1$stdBIOMASS  <-sqrt(tbl1$stdBIOMASS);  #convert from var to stdv
    tbl1$cvBIOMASS   <-tbl1$stdBIOMASS/tbl1$totBIOMASS;
                                 
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

#tbl.totBio.frBBS<-calcBiomass.EBS(tbl.BiomassByStratum)
#tbl.totBio.frEW<-calcBiomass.EBS(tbl.EW166Biomass)