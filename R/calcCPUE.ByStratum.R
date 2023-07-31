#'
#'@title Calculate average cpue, numbers and biomass by stratum from AFSC trawl survey data.
#'
#'@param   tbl_strata : data frame w/ stations/strata from call to [selectStrata.TrawlSurvey]
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
#'   \item Area is in square nautical miles
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
#'\item  numNonZeroHauls
#'\item  numIndivs
#'\item  avgNumCPUE = average cpue (numbers) by stratum
#'\item  stdNumCPUE = std deviation of cpue (numbers) by stratum
#'\item  cvNumCPUE  = cv of cpue (numbers) by stratum
#'\item  avgWgtCPUE = average cpue (weight) by stratum
#'\item  stdWgtCPUE = std deviation of cpue (weight) by stratum
#'\item  cvWgtCPUE  = cv of estimated cpue (weight) by stratum
#'}
#'
#' @importFrom sqldf sqldf
#' @importFrom utils read.csv write.csv
#' @importFrom wtsUtilities selectFile
#'
#'@export
#'
#######################################################################
calcCPUE.ByStratum<-function(tbl_strata,
                                tbl_cpue=NULL,
                                export=FALSE,
                                out.csv='SurveyCPUE.ByStratum.csv',
                                out.dir=NULL,
                                verbosity=0){
    if (verbosity>1) cat("starting calcCPUE.ByStratum\n");

    if (!is.data.frame(tbl_strata)) {
        cat("Error in calcCPUE.ByStratum:",
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
        tbl_cpue<-read.csv(in.csv,stringsAsFactors=FALSE);
        if (verbosity>1) cat("Done reading input csv file.\n")
    }

    if (is.null(out.dir)) {
        out.dir<-dirname(file.path('.'));
        if (!is.null(in.csv)) {out.dir<-dirname(file.path(in.csv));}
    }
    if (verbosity>0) cat("Output directory for calcCPUE.ByStratum will be '",out.dir,"'\n",sep='');

    #determine columns of cpue table
    cols<-names(tbl_cpue);
    byHaul<-any(cols=="HAULJOIN");
    nonFacs<-c("YEAR","STRATUM","GIS_STATION","LONGITUDE","LATITUDE",
               "numHauls","numNonZeroHauls","numIndivs","SAMPLING_FACTOR","AREA_SWEPT_VARIABLE","numCPUE","wgtCPUE");
    facs<-cols[!(cols %in% nonFacs)];
#    nc<-length(cols);
    if (byHaul){
        # nc0f<-9;#number of col.s w/ no factors if cpue is by haul
        # facs<-'';
        # if (nc>nc0f){
        #     facs<-cols[7:(nc-3)];#drop the 1st 6 (YEAR,STRATUM,GIS_STATION,HAULJOIN,LONGITUDE,LATITUDE) and last 3 column names (numIndivs,numCPUE,wgtCPUE)
        # }
        byHaulSub<-"c.HAULJOIN,";
        numHaulSub<-"1 as numHauls, 1*(numIndivs>0) as numNonZeroHauls,";#number of hauls/haul (=1, of course)
    } else {
        # nc0f<-10;#number of col.s w/ no factors if cpue is by station
        # facs<-'';
        # if (nc>nc0f){
        #     facs<-cols[6:(nc-5)];#drop the 1st 5 (YEAR,STRATUM,GIS_STATION,LONGITUDE,LATITUDE) and last 5 column names (numHauls,numNonZeroHauls,numIndivs,numCPUE,wgtCPUE)
        # }
        byHaulSub<-"";
        numHaulSub<-"c.numHauls,c.numNonZeroHauls,";#number of hauls/station
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
    if (length(facs)==0){
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
            sum(numNonZeroHauls) as numNonZeroHauls,
            sum(numIndivs) as numIndivs,
            avg(numCPUE)   as avgNumCPUE,
            avg(wgtCPUE)   as avgWgtCPUE
          from
            tbl1
          group by
            YEAR,STRATUM,STRATUM_AREA&&facs
          order by
            YEAR,STRATUM&&facs;";
    if (length(facs)==0) {
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
            a.numNonZeroHauls as numNonZeroHauls,
            a.numIndivs as numIndivs,
            avgNumCPUE,
            avg((numCPUE-avgNumCPUE)*(numCPUE-avgNumCPUE)) as msqNumCPUE,
            0 as stdNumCPUE,
            0 as cvNumCPUE,
            avgWgtCPUE,
            avg((wgtCPUE-avgWgtCPUE)*(wgtCPUE-avgWgtCPUE)) as msqWgtCPUE,
            0 as stdWgtCPUE,
            0 as cvWgtCPUE
          from
            tbl1 as t,
            tbl2 as a
          where
            t.YEAR=a.YEAR and
            t.STRATUM=a.STRATUM and
            &&whrcols
            1=1
          group by
            a.YEAR,a.STRATUM,a.STRATUM_AREA,a.numStations,a.numHauls,a.numIndivs,avgNumCPUE,avgWgtCPUE&&bycols
          order by
            a.YEAR,a.STRATUM&&bycols;";
    if (length(facs)==0) {
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

    #compute std. errors and CVs of means
    if (byHaul){n<-tbl3$numHauls;} else {n<-tbl3$numStations;}

    tbl3$stdNumCPUE<-sqrt(((n/(n-1))*tbl3$msqNumCPUE)/n);
    tbl3$cvNumCPUE <-tbl3$stdNumCPUE/tbl3$avgNumCPUE;
    idx<-is.nan(tbl3$cvNumCPUE);
    tbl3$cvNumCPUE[idx]<-0;

    tbl3$stdWgtCPUE<-sqrt(((n/(n-1))*tbl3$msqWgtCPUE)/n);
    tbl3$cvWgtCPUE <-tbl3$stdWgtCPUE/tbl3$avgWgtCPUE;
    idx<-is.nan(tbl3$cvWgtCPUE);
    tbl3$cvWgtCPUE[idx]<-0;

    tbl3<-subset(tbl3,select=-c(msqNumCPUE,msqWgtCPUE));

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

    if (verbosity>1) cat("finished calcCPUE.ByStratum\n");
    return(tbl3);
}

#tbl.BTC.MAA.BiomassByStratum<-calcCPUE.ByStratum(tbl.BTC.stns,tbl.BTC.MAA.cpue,export=FALSE);
