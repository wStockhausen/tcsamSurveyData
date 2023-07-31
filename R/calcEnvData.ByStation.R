#'
#'@title Function to calculate environmental data by survey station, averaging over values by haul.
#'
#'@description Function to calculate environmental data by survey station, averaging over values by haul.
#'
#'@param tbl_strata : dataframe w/ station info
#'@param tbl_hauls  : data frame w/ haul data, or name of csv file w/ haul data, or NULL to choose file
#'@param export     : boolean flag to write results to csv file
#'@param out.csv    : output file name
#'@param out.dir    : output file directory
#'@param verbosity  : integer flag indicating level of printed output (0=off,1=minimal,2=full)
#'
#'@return A dataframe of environmental data (bottom depth, temperature) by year, stratum, survey station. See Details.
#'@details
#'Notes: \cr
#'\itemize{
#'\item   mean bottom depth in m
#'\item   mean bottom (gear) temperature in deg C
#'} \cr
#' The returned dataframe has the following columns: \cr
#'\itemize{
#'\item   YEAR
#'\item   STRATUM
#'\item   GIS_STATION
#'\item   LONGITUDE
#'\item   LATITUDE
#'\item   numHauls
#'\item   BOTTOM_DEPTH
#'\item   BOTTOM_TEMP
#'}
#'
#' @importFrom sqldf sqldf
#' @importFrom wtsUtilities selectFile
#'
#'@export
#'
calcEnvData.ByStation<-function(tbl_strata=NULL,
                                 tbl_hauls=NULL,
                                 export=FALSE,
                                 out.csv='envData.ByStation.csv',
                                 out.dir=NULL,
                                 verbosity=0){
        if (verbosity>1) cat("starting calcEnvData.ByStation\n");

    if (is.null(tbl_strata)){
        cat("Error in calcEnvData.ByStation\n");
        cat("tbl_strata is NULL but must be provided.\n")
        cat("Aborting...\n");
        return(NULL);
    }

    in.csv<-NULL;
    if (!is.data.frame(tbl_hauls)){
        if (!is.character(tbl_hauls)) {
            in.csv<-wtsUtilities::selectFile(ext="csv",caption="Select csv file with haul info");
            if (is.null(in.csv)|(in.csv=='')) return(NULL);
        } else {
            in.csv<-tbl_hauls;#tbl is a filename
        }
        if (verbosity>1) cat("Reading csv file for CPUE-by-haul info.\n",sep='')
        tbl<-read.csv(in.csv,stringsAsFactors=FALSE);
        if (verbosity>1) cat("Done reading input csv file.\n")
    }

    if (is.null(out.dir)) {
        out.dir<-dirname(file.path('.'));
        if (!is.null(in.csv)) {out.dir<-dirname(file.path(in.csv));}
    }
    if (verbosity>0) cat("Output directory for calcEnvData.ByStation will be '",out.dir,"'\n",sep='');

    #retain strata only for years in tbl_hauls
    qry<-"select
            STATION_LONGITUDE as LONGITUDE,
            STATION_LATITUDE as LATITUDE,
            s.YEAR,STRATUM,GIS_STATION
            from
              tbl_strata s,
              (select distinct YEAR from tbl_hauls) y
            where
              s.YEAR=y.YEAR;";
    if (verbosity>1) cat("\nyear query is:\n",qry,"\n");
    tbl_strata<-sqldf::sqldf(qry);

    #Calculate and average env var values by year, station and factor levels
    #(e.g., sex, shell condition) over hauls.
    qry<-"select
            s.YEAR,
            s.STRATUM,
            s.GIS_STATION,
            s.LONGITUDE,
            s.LATITUDE,
            count(distinct h.HAULJOIN) as numHauls,
            avg(h.BOTTOM_DEPTH) as BOTTOM_DEPTH,
            avg(h.GEAR_TEMPERATURE) as BOTTOM_TEMP
          from
            tbl_strata s join tbl_hauls h
          on
            s.YEAR=h.YEAR and
            s.STRATUM=h.STRATUM and
            s.GIS_STATION=h.GIS_STATION
          group by
            s.YEAR,s.STRATUM,s.GIS_STATION,s.LONGITUDE,s.LATITUDE
          order by
            s.YEAR,s.STRATUM,s.GIS_STATION;";
    if (verbosity>1) cat("\naveraging query is:\n",qry,"\n");
    tbl_hauls<-sqldf::sqldf(qry);

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
        write.csv(tbl_hauls,out.csv,na='',row.names=FALSE);
    }

    if (verbosity>1) cat("finished calcEnvData.ByStation\n");
    return(tbl_hauls)
}

#tbl.EnvVars.byS<-calcEnvData.ByStation(dfr.SD,dfr.HD,export=FALSE)
