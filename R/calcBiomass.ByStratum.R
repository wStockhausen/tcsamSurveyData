#'
#'@title Function to calculate cpue by survey station from trawl survey files.
#'
#'@param   tbl_strata : data frame w/ stations/strata from call to selectStrata.TrawlSurvey(...)
#'@param   tbl_cpue   : data frame w/ cpue by year, station, other factor levels
#'@param   in.csv     : csv file w/ cpue by year, station, other factor levels
#'@param   export  : boolean flag to write results to csv file
#'@param   out.csv : output file name
#'@param   out.dir : output file directory 
#'
#'@description Note: if tbl_cpue and in.csv are both NULL, the user is prompted to enter a csv file with cpue info. \cr
#'Other notes: \cr
#'   CPUE in numbers is in no/(sq. nm.) \cr
#'   CPUE in weight  is in mt/(sq. nm.) \cr
#'   Abundance is in 10^6 indivs \cr
#'   Biomass   is in 10^3 mt \cr
#'
#'
#'@return data frame with average cpue (numbers, weight), abundance and biomass by stratum.
#'
#' @import sqldf
#' @importFrom tcltk tk_choose.files
#' @importFrom wtsUtilities addFilter
#'
#'@export
#'
#######################################################################
calcBiomass.ByStratum<-function(tbl_strata,
                                tbl_cpue=NULL,
                                in.csv=NULL,
                                export=FALSE,
                                out.csv='BiomassByStratum.csv',
                                out.dir=NULL){
    
    if (is.null(tbl_cpue)){
        cat("Reading csv file for cpue by haul or station.\n",sep='')
        if (is.null(in.csv)) {
            Filters<-addFilter("csv","csv files (*.csv)","*.csv");
            in.csv<-tk_choose.files(caption=paste("Select csv file with cpue by haul or station"),
                                    multi=FALSE,filters=matrix(Filters[c("csv"),],1,2,byrow=TRUE));
            if (is.null(in.csv)|(in.csv=='')) return(NULL);
        }
        if (is.null(out.dir)) {
            out.dir<-dirname(file.path('.'));
            if (!is.null(in.csv)) {out.dir<-dirname(file.path(in.csv));}
        }
        cat("Output directory will be '",out.dir,"'\n",sep='');
        
        tbl_cpue<-read.csv(in.csv,stringsAsFactors=FALSE);
        cat("Done reading input csv file.\n")
    }
    
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
    cat("\nquery is:\n",qry,"\n");
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
    cat("\nquery is:\n",qry,"\n");
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
    cat("\nquery is:\n",qry,"\n");
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
            cat("\nTesting existence of folder '",out.dir,"'\n",sep='')
            if (!file.exists(out.dir)){
                cat("Creating folder '",out.dir,"' for output.\n",sep='')
                dir.create(out.dir);
            } else {
                cat("Using folder '",out.dir,"' for output.\n",sep='')
            }
            out.csv<-file.path(out.dir,out.csv)
        }
        write.csv(tbl3,out.csv,na='',row.names=FALSE);
    }
    
    return(tbl3);
}

#tbl.BTC.MAA.BiomassByStratum<-calcBiomass.ByStratum(tbl.BTC.stns,tbl.BTC.MAA.cpue,export=FALSE);
