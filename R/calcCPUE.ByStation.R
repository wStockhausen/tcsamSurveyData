#'
#'@title Function to calculate cpue by survey station, averaging over cpue by haul.
#'
#'@description Function to calculate cpue by survey station, averaging over cpue by haul.
#'
#'@param tbl_strata : dataframe w/ station info
#'@param  tbl_cpue  : data frame w/ cpue by haul from call to calcCPUE.ByHaul(...), or name of csv file w/ cpue by haul, or NULL to choose file
#'@param export     : boolean flag to write results to csv file
#'@param out.csv    : output file name
#'@param out.dir    : output file directory
#'@param verbosity  : integer flag indicating level of printed output (0=off,1=minimal,2=full)
#'
#'@return A dataframe of cpue (numbers and weight) by year, stratum, survey station
#'and other factors. Only stations where hauls were conducted are returned. See Details for dataframe structure.
#'
#'@details
#'Notes: \cr
#'\itemize{
#'\item   CPUE in numbers is in no/(sq. nm.)
#'\item   CPUE in weight  is in mt/(sq. nm.)
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
#'\item   SEX
#'\item   MATURITY
#'\item   SHELL_CONDITION
#'\item   SIZE
#'\item   numHauls
#'\item   numNonZeroHauls
#'\item   numIndivs
#'\item   SAMPLING_FACTOR
#'\item   AREA_SWEPT_VARIABLE
#'\item   numCPUE
#'\item   wgtCPUE
#'}
#'
#' @importFrom sqldf sqldf
#' @importFrom utils read.csv write.csv
#' @importFrom wtsUtilities selectFile
#'
#'@export
#'
calcCPUE.ByStation<-function(tbl_strata=NULL,
                             tbl_cpue=NULL,
                             export=FALSE,
                             out.csv='cpue.ByStation.csv',
                             out.dir=NULL,
                             verbosity=0){
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
            in.csv<-selectFile(ext="csv",caption="Select csv file with CPUE-by-haul info");
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
    nonFacs<-c("YEAR","STRATUM","GIS_STATION","HAULJOIN","LONGITUDE","LATITUDE",
               "numIndivs","SAMPLING_FACTOR","AREA_SWEPT_VARIABLE","numCPUE","wgtCPUE");
    facs<-cols[!(cols %in% nonFacs)];
    # nc<-length(cols);
    # nc0f<-9;#number of col.s w/ no factors for cpue by haul
    # ci1f<-7;#column index of 1st factor (if any) [cols are YEAR,STRATUM,GIS_STATION,HAULJOIN,1st factor,...]
    # if (nc==nc0f) {
    #     cols<-NULL;
    #     colstr<-'';#no factors
    # } else {
    #     cols<-cols[ci1f:(nc-3)];#drop first 6 cols, last 3 column names (YEAR,STRATUM,GIS_STATION,HAULJOIN,HAUL_LONGITUDE,HAUL_LATITUDE,numIndivs,numCPUE,wgtCPUE)
    #     colstr<-paste(',',cols,sep='',collapse='');
    # }

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
    tbl_strata<-sqldf::sqldf(qry);

    #Calculate and average cpue by year, station and factor levels
    #(e.g., sex, shell condition) over hauls.
    qry<-"select
            YEAR,STRATUM,GIS_STATION&&facs,
            count(distinct HAULJOIN) as numHauls,
            sum(numIndivs>0) as numNonZeroHauls,
            sum(numIndivs) as numIndivs,
            CASE WHEN sum(numIndivs)>0 THEN sum(numIndivs*SAMPLING_FACTOR)/sum(numIndivs) ELSE avg(SAMPLING_FACTOR)     END as SAMPLING_FACTOR,
            CASE WHEN avg(numCPUE)>0   THEN sum(numIndivs)/avg(numCPUE)                   ELSE avg(AREA_SWEPT_VARIABLE) END as AREA_SWEPT_VARIABLE,
            avg(numCPUE)   as numCPUE,
            avg(wgtCPUE)   as wgtCPUE
          from
            tbl_cpue
          group by
            YEAR,STRATUM,GIS_STATION&&facs
          order by
            YEAR,STRATUM,GIS_STATION&&facs;";
    if (length(facs)==0){
        qry<-gsub("&&facs",'',qry,fixed=TRUE);#no factors
    } else {
        qry<-gsub("&&facs",paste(",",paste(facs,sep='',collapse=","),sep=''),qry,fixed=TRUE);
    }
    if (verbosity>1) cat("\nquery is:\n",qry,"\n");
    tbl_cpue<-sqldf::sqldf(qry);

    #add in latitude, longitude corresponding to GIS_STATION
    qry<-"select
            c.YEAR,c.STRATUM,c.GIS_STATION,s.LONGITUDE,s.LATITUDE&&facs,
            c.numHauls,c.numNonZeroHauls,
            c.numIndivs,c.SAMPLING_FACTOR,c.AREA_SWEPT_VARIABLE,
            c.numCPUE,c.wgtCPUE
          from
            tbl_cpue c, tbl_strata s
          where
            c.YEAR=s.YEAR and
            c.STRATUM=s.STRATUM and
            c.GIS_STATION=s.GIS_STATION
          order by
            c.YEAR,c.STRATUM,c.GIS_STATION&&facs;";
    if (length(facs)==0){
        qry<-gsub("&&facs",'',qry,fixed=TRUE);#no factors
    } else {
        qry<-gsub("&&facs",paste(",",paste("c.",facs,sep='',collapse=","),sep=''),qry);
    }
    if (verbosity>1) cat("\nquery is:\n",qry,"\n");
    tbl_cpue<-sqldf::sqldf(qry);

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
        write.csv(tbl_cpue,out.csv,na='',row.names=FALSE);
    }

    if (verbosity>1) cat("finished calcCPUE.ByStation\n");
    return(tbl_cpue)
}

#tbl.cpue.byS<-calcCPUE.ByStation(tbl.cpue.byH,export=FALSE)
