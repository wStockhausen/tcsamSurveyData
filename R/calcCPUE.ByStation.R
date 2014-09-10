#'
#'@title Function to calculate cpue by survey station, averaging over cpue by haul. 
#'
#'@param tbl_strata: dataframe w/ station info
#'@param  tbl_cpue : data frame w/ cpue by haul from call to calcCPUE.ByHaul(...), or name of csv file w/ cpue by haul, or NULL to choose file
#'@param export  : boolean flag to write results to csv file
#'@param out.csv : output file name
#'@param out.dir : output file directory 
#'@param verbosity : integer flag indicating level of printed output (0=off,1=minimal,2=full)
#'
#'@return A dataframe of cpue (numbers and weight) by year, stratum, survey station 
#'and other factors with the following columns. See Details.
#'@details
#'Notes: \cr
#'\itemize{
#'\item   CPUE in numbers is in no/(sq. nm.) \cr
#'\item   CPUE in weight  is in mt/(sq. nm.) \cr
#'\item   CPUE is calculated only for stations at which at least 1 haul was conducted
#'} \cr
#' The returned dataframe has the following columns: \cr
#'\itemize{
#'\item   YEAR
#'\item   STRATUM
#'\item   GIS_STATION
#'\item   HAULJOIN
#'\item   LONGITUDE
#'\item   LATITUDE
#'\item   requested factors, if any
#'\item   numHauls
#'\item   numNonZeroHauls
#'\item   numIndivs
#'\item   numCPUE
#'\item   wgtCPUE
#'}
#'
#' @import sqldf
#' @importFrom wtsUtilities selectFile
#'      
#'@export
#'
calcCPUE.ByStation<-function(tbl_strata=NULL,
                             tbl_cpue=NULL,
                             export=FALSE,
                             out.csv='cpue.ByStation.csv',
                             out.dir=NULL,
                             verbosity=1){
    if (verbosity>1) cat("starting calcCPUE.ByStation\n");
    
    if (is.null(tbl_strata)){
        cat("Error in calcCPUE.ByStation\n");
        cat("tbl_strata is NULL but must be provided.\n")
        cat("Aborting...\n");
        return(NULL);
    }    
    
    in.csv<-NULL;
    if (!is.data.frame(tbl_cpue)){
        if (!is.character(tbl_cpue)) {
            in.csv<-wtsUtilities::selectFile(ext="csv",caption="Select csv file with CPUE-by-haul info");
            if (is.null(in.csv)|(in.csv=='')) return(NULL);
        } else {
            in.csv<-tbl_cpue;#tbl is a filename
        }
        if (verbosity>1) cat("Reading csv file for CPUE-by-haul info.\n",sep='')
        tbl<-read.csv(in.csv,stringsAsFactors=FALSE);
        if (verbosity>1) cat("Done reading input csv file.\n")
    }
    
    if (is.null(out.dir)) {
        out.dir<-dirname(file.path('.'));
        if (!is.null(in.csv)) {out.dir<-dirname(file.path(in.csv));}
    }
    if (verbosity>0) cat("Output directory for calcCPUE.ByStation will be '",out.dir,"'\n",sep='');
    
    #determine names of factor columns (if any) in cpue table
    cols<-names(tbl_cpue); 
    nc<-length(cols);
    nc0f<-9;#number of col.s w/ no factors for cpue by haul
    ci1f<-7;#column index of 1st factor (if any) [cols are YEAR,STRATUM,GIS_STATION,HAULJOIN,1st factor,...]
    if (nc==nc0f) {
        cols<-NULL;
        colstr<-'';#no factors
    } else {
        cols<-cols[ci1f:(nc-3)];#drop first 6 cols, last 3 column names (YEAR,STRATUM,GIS_STATION,HAULJOIN,HAUL_LONGITUDE,HAUL_LATITUDE,numIndivs,numCPUE,wgtCPUE)
        colstr<-paste(',',cols,sep='',collapse='');
    }
    
    #retain strata only for years in tbl_cpue
    qry<-"select 
            STATION_LONGITUDE as LONGITUDE,
            STATION_LATITUDE as LATITUDE,
            s.YEAR,STRATUM,GIS_STATION
            from 
              tbl_strata s,
              (select distinct YEAR from tbl_cpue) y
            where
              s.YEAR=y.YEAR;";
    tbl_strata<-sqldf(qry);
    
    #Calculate and average cpue by year, station and factor levels 
    #(e.g., sex, shell condition) over hauls.
    qry<-"select
            YEAR,
            STRATUM,
            GIS_STATION&&cols,
            count(distinct HAULJOIN) as numHauls,
            sum(numIndivs>0) as numNonZeroHauls,
            sum(numIndivs) as numIndivs,
            avg(numCPUE)   as numCPUE,
            avg(wgtCPUE)   as wgtCPUE
          from
            tbl_cpue
          group by
            YEAR,STRATUM,GIS_STATION&&cols
          order by
            YEAR,STRATUM,GIS_STATION&&cols;";
    qry<-gsub("&&cols",colstr,qry)
    if (verbosity>1) cat("\nquery is:\n",qry,"\n");
    tbl_cpue<-sqldf(qry);
    
    #get unique combination of factor levels
    strata_cols<-c('LONGITUDE','LATITUDE','YEAR','STRATUM','GIS_STATION');
    if (is.null(cols)){
        qry<-"select distinct &&stratacols from tbl_strata;";
        qry<-gsub("&&stratacols",paste(strata_cols,collapse=','),qry)
    } else {
        qry<-"select * from
                (select distinct &&stratacols from tbl_strata),
                (select distinct &&cols from tbl_cpue);";
        qry<-gsub("&&stratacols",paste(strata_cols,collapse=','),qry)
        qry<-gsub("&&cols",paste(cols,collapse=','),qry)
    }
    if (verbosity>1) cat("\nquery is:\n",qry,"\n");
    tbl_ufs<-sqldf(qry);
    ucols<-names(tbl_ufs);
    nufs<-length(ucols)
    
    qry<-"select
            &&ucols,
            numHauls,
            numNonZeroHauls,
            numIndivs,
            numCPUE,
            wgtCPUE
          from
            tbl_ufs u left join
            tbl_cpue c
          on
            &&joinstr;";
    qry<-gsub("&&cols",colstr,qry)
    qry<-gsub("&&ucols",paste('u.',ucols,sep='',collapse=','),qry)
    qry<-gsub("&&joinstr",paste(paste('u.',ucols[3:nufs],sep=''),"=",paste('c.',ucols[3:nufs],sep=''),sep='',collapse=' and \n'),qry)
    if (verbosity>1) cat("\nquery is:\n",qry,"\n");
    tbl_cpues<-sqldf(qry);
    
    idx<-is.na(tbl_cpues$numHauls);
    tbl_cpues$numHauls[idx]<-0;
    tbl_cpues$numHauls[idx]<-0;
    tbl_cpues$numNonZeroHauls[idx]<-0;
    tbl_cpues$numCPUE[idx]<-0;
    tbl_cpues$wgtCPUE[idx]<-0;
    
    #re-order rows/columns for output
    qry<-"select 
            YEAR,STRATUM,GIS_STATION,LONGITUDE,LATITUDE&&cols,
            numHauls,numNonZeroHauls,numIndivs,numCPUE,wgtCPUE
          from
            tbl_cpues
          order by
            YEAR,STRATUM,GIS_STATION&&cols;"
    qry<-gsub("&&cols",colstr,qry)
    if (verbosity>1) cat("\nquery is:\n",qry,"\n");
    tbl_cpues<-sqldf(qry);
    
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
        write.csv(tbl_cpues,out.csv,na='',row.names=FALSE);
    }
    
    if (verbosity>1) cat("finished calcCPUE.ByStation\n");
    return(tbl_cpues)
}

#tbl.cpue.byS<-calcCPUE.ByStation(tbl.cpue.byH,export=FALSE)
