#'
#'@title Calculate average cpue, numbers and biomass by stratum from AFSC trawl survey data.
#'
#'@param   tbl_strata : data frame w/ stations/strata from call to selectStrata.TrawlSurvey(...)
#'@param   tbl_cpue   : data frame w/ cpue by year, station, other factor levels (or csv filename or NULL)
#'@param   export  : boolean flag to write results to csv file
#'@param   out.csv : output file name
#'@param   out.dir : output file directory 
#'@param verbosity : integer flag indicating level of printed output (0=off,1=minimal,2=full)
#'
#'@description This function calculates average cpue, numbers and biomass by stratum from cpue (by survey station or by haul).
#'
#'@details If tbl_cpue is NULL, the user is prompted to enter a csv file with cpue info. \cr
#'\cr Other notes: \cr
#'\itemize{
#'   \item   Area is in square nautical miles
#'   \item CPUE in numbers is in no/(sq. nm.)
#'   \item CPUE in weight  is in mt/(sq. nm.) 
#'   \item Abundance is in 10^6 indivs 
#'   \item Biomass   is in 10^3 mt
#'}
#'
#'@return data frame with average cpue (numbers, weight), abundance and biomass by stratum. Columns are \cr
#'\itemize{
#'\item  YEAR
#'\item  STRATUM
#'\item  STRATUM_AREA
#'\item  other user-defined factors
#'\item  numStations
#'\item  numHauls
#'\item  numIndivs
#'\item  avgNUMCPUE
#'\item  seNUMCPUE
#'\item  avgWGTCPUE
#'\item  seWGTCPUE
#'\item  totABUNDANCE = estimated abundance (by stratum)
#'\item  stdABUNDANCE = std deviation of estimated abundance (by stratum)
#'\item  cvABUNDANCE  = cv of estimated abundance (by stratum)
#'\item  totBIOMASS = estimated biomass (by stratum)
#'\item  stdBIOMASS = std deviation of estimated biomass (by stratum)
#'\item  cvBIOMASS  = cv of estimated biomass (by stratum)
#'}
#'
#' @import sqldf
#' @importFrom wtsUtilities selectFile
#'
#'@export
#'
#######################################################################
calcBiomass.ByStratum<-function(tbl_strata,
                                tbl_cpue=NULL,
                                export=FALSE,
                                out.csv='BiomassByStratum.csv',
                                out.dir=NULL,
                                verbosity=1){
    if (verbosity>1) cat("starting calcBiomass.ByStratum\n");
    
    if (!is.data.frame(tbl_strata)) {
        cat("Error in calcBiomass.ByStratum:",
            "tbl_strata is NULL. Must supply tbl_strata.",
            "Aborting...",sep='\n');
        return(NULL);
    }
    
    in.csv<-NULL;
    if (!is.data.frame(tbl_cpue)){
        if (!is.character(tbl_cpue)) {
            in.csv<-wtsUtilities::selectFile(ext="csv",caption="Select csv file with CPUE-by-haul or -by-station info");
            if (is.null(in.csv)|(in.csv=='')) return(NULL);
        } else {
            in.csv<-tbl_cpue;#tbl is a filename
        }
        if (verbosity>1) cat("Reading csv file for CPUE-by-haul or -by-station info.\n",sep='')
        tbl<-read.csv(in.csv,stringsAsFactors=FALSE);
        if (verbosity>1) cat("Done reading input csv file.\n")
    }
    
    if (is.null(out.dir)) {
        out.dir<-dirname(file.path('.'));
        if (!is.null(in.csv)) {out.dir<-dirname(file.path(in.csv));}
    }
    if (verbosity>0) cat("Output directory for calcCPUE.ByStratum will be '",out.dir,"'\n",sep='');
    
    #determine columns of cpue table
    cols<-names(tbl_cpue); 
    nc<-length(cols);
    byHaul<-any(cols=="HAULJOIN");
    if (byHaul){
        nc0f<-7;#number of col.s w/ no factors if cpue is by haul
        facs<-'';
        if (nc>nc0f){
            facs<-cols[5:(nc-3)];#drop the 1st 4 (YEAR,STRATUM,GIS_STATION,HAULJOIN) and last 3 column names (numIndivs,numCPUE,wgtCPUE)
        }
        byHaulSub<-"c.HAULJOIN,";
        numHaulSub<-"1 as numHauls,";#number of hauls/haul (=1, of course)
    } else {
        nc0f<-7;#number of col.s w/ no factors if cpue is by station
        facs<-'';
        if (nc>nc0f){
            facs<-cols[4:(nc-4)];#drop the 1st 3 (YEAR,STRATUM,GIS_STATION) and last 4 column names (numHauls,numIndivs,numCPUE,wgtCPUE)
        }
        byHaulSub<-"";
        numHaulSub<-"c.numHauls,";#number of hauls/station
    }
    
    #assign (possibly) new strata to cpue table
    qry<-"select
            c.YEAR,
            s.STRATUM,
            s.STRATUM_AREA,
            c.GIS_STATION,
            &&byHaulSub
            &&facs
            &&numHaulSub
            c.numIndivs,
            c.numCPUE,
            c.wgtCPUE
          from
            tbl_cpue as c,
            tbl_strata as s
          where
            c.YEAR=s.YEAR and
            c.GIS_STATION=s.GIS_STATION;";
    qry<-gsub("&&byHaulSub",byHaulSub,qry);
    qry<-gsub("&&numHaulSub",numHaulSub,qry);
    if (nc==nc0f){
        qry<-gsub("&&facs",'',qry);#no factors
    } else {
        qry<-gsub("&&facs",paste(paste("c.",facs,sep='',collapse=","),",",sep=''),qry);
    }
    if (verbosity>1) cat("\nquery is:\n",qry,"\n");
    tbl1<-sqldf(qry);
    
    #calculate number of unique stations
    #calculate average cpues over strata
    qry<-"select
            YEAR,
            STRATUM,
            STRATUM_AREA&&facs,
            count(DISTINCT GIS_STATION) as numStations,
            sum(numHauls)  as numHauls,
            sum(numIndivs) as numIndivs,
            avg(numCPUE)   as avgNUMCPUE,
            avg(wgtCPUE)   as avgWGTCPUE
          from
            tbl1
          group by
            YEAR,STRATUM,STRATUM_AREA&&facs
          order by
            YEAR,STRATUM&&facs;";
    if (nc==nc0f) {
        qry<-gsub("&&facs",'',qry);#no factors
    } else {
        qry<-gsub("&&facs",paste(',',facs,sep='',collapse=''),qry);
    }
    if (verbosity>1) cat("\nquery is:\n",qry,"\n");
    tbl2<-sqldf(qry);
    
    #calculate variances
    qry<-"select
            a.YEAR as YEAR,
            a.STRATUM as STRATUM,
            a.STRATUM_AREA as STRATUM_AREA&&facs,
            a.numStations as numStations,
            a.numHauls as numHauls,
            a.numIndivs as numIndivs,
            avgNUMCPUE,
            avg((numCPUE-avgNUMCPUE)*(numCPUE-avgNUMCPUE)) as seNUMCPUE,
            avgWGTCPUE,
            avg((wgtCPUE-avgWGTCPUE)*(wgtCPUE-avgWGTCPUE)) as seWGTCPUE
          from
            tbl1 as t,
            tbl2 as a
          where
            t.YEAR=a.YEAR and
            t.STRATUM=a.STRATUM and
            &&whrcols
            1=1
          group by
            a.YEAR,a.STRATUM,a.STRATUM_AREA,a.numStations,a.numHauls,a.numIndivs,avgNUMCPUE,avgWGTCPUE&&bycols
          order by
            a.YEAR,a.STRATUM&&bycols;";
    if (nc==nc0f) {
        qry<-gsub("&&facs",   '',qry);
        qry<-gsub("&&whrcols",'',qry);
        qry<-gsub("&&bycols", '',qry);
    } else {
        str<-'';
        for (fac in facs){str<-paste(str,paste(',a.',fac,' as ',fac,sep=''),sep='');}
        qry<-gsub("&&facs",str,qry);
        str<-paste(paste(paste('t.',facs,sep=''),paste('a.',facs,sep=''),sep="=",collapse=' and '),' and ',sep='');
        qry<-gsub("&&whrcols",str,qry);
        str<-paste(',a.',facs,sep='',collapse='');
        qry<-gsub("&&bycols",str,qry);
    }
    if (verbosity>1) cat("\nquery is:\n",qry,"\n");
    tbl3<-sqldf(qry);
    
    #compute std. errors of means from variances and scale to totals by stratum
    if (byHaul){n<-tbl3$numHauls;} else {n<-tbl3$numStations;}
    tbl3$seNUMCPUE<-sqrt(((n/(n-1))*tbl3$seNUMCPUE)/n);
    tbl3$totABUNDANCE<-tbl3$STRATUM_AREA*tbl3$avgNUMCPUE/1.0E6;#scale abundance to millions
    tbl3$stdABUNDANCE<-tbl3$STRATUM_AREA*tbl3$seNUMCPUE /1.0E6;#scale abundance to millions    
    tbl3$cvABUNDANCE <-tbl3$stdABUNDANCE/tbl3$totABUNDANCE;
    
    tbl3$seWGTCPUE<-sqrt(((n/(n-1))*tbl3$seWGTCPUE)/n);
    tbl3$totBIOMASS  <-tbl3$STRATUM_AREA*tbl3$avgWGTCPUE/1.0E3;#biomass in 1000's t
    tbl3$stdBIOMASS  <-tbl3$STRATUM_AREA*tbl3$seWGTCPUE/1.0E3; #biomass in 1000's t
    tbl3$cvBIOMASS   <-tbl3$stdBIOMASS/tbl3$totBIOMASS;
    
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
        write.csv(tbl3,out.csv,na='',row.names=FALSE);
    }
    
    if (verbosity>1) cat("finished calcBiomass.ByStratum\n");
    return(tbl3);
}

#tbl.BTC.MAA.BiomassByStratum<-calcBiomass.ByStratum(tbl.BTC.stns,tbl.BTC.MAA.cpue,export=FALSE);
