#'
#'@title Function to calculate cpue by survey station, averaging over cpue by haul. 
#'
#'@param   tbl_cpue : data frame w/ cpue by haul from call to calcCPUE.ByHaul(...), or name of csv file w/ cpue by haul, or NULL to choose file
#'@param export  : boolean flag to write results to csv file
#'@param out.csv : output file name
#'@param out.dir : output file directory 
#'@param verbosity : integer flag indicating level of printed output (0=off,1=minimal,2=full)
#'
#'@return   data frame of cpue (numbers and weight) by year, stratum, station and other factor levels
#'
#'@details \cr
#'Notes: \cr
#'\itemize{
#'\item   CPUE in numbers is in no/(sq. nm.) \cr
#'\item   CPUE in weight  is in mt/(sq. nm.) \cr
#'\item   CPUE is calculated only for stations at which at least 1 haul was conducted
#'}
#'
#' @import sqldf
#' @importFrom wtsUtilities selectFile
#'      
#'@export
#'
calcCPUE.ByStation<-function(tbl_cpue=NULL,
                             export=FALSE,
                             out.csv='cpue.ByStation.csv',
                             out.dir=NULL,
                             verbosity=1){
    if (verbosity>1) cat("starting calcCPUE.ByStation\n");
    
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
    nc0f<-7;#number of col.s w/ no factors for cpue by haul
    ci1f<-5;#column index of 1st factor (if any) [cols are YEAR,STRATUM,GIS_STATION,HAULJOIN,1st factor,...]
    if (nc==nc0f) {
        cols<-'';#no factors
    } else {
        cols<-cols[ci1f:(nc-3)];#drop first 4 cols, last 3 column names (YEAR,STRATUM,GIS_STATION,HAULJOIN,numIndivs,numCPUE,wgtCPUE)
        cols<-paste(',',cols,sep='',collapse='');
    }
    
    #Calculate and average cpue by year, station and factor levels 
    #(e.g., sex, shell condition) over hauls.
    qry<-"select
            YEAR,
            STRATUM,
            GIS_STATION&&cols,
            count(distinct HAULJOIN) as numHauls,
            sum(numIndivs) as numIndivs,
            avg(numCPUE)   as numCPUE,
            avg(wgtCPUE)   as wgtCPUE
          from
            tbl_cpue
          group by
            YEAR,STRATUM,GIS_STATION&&cols
          order by
            YEAR,STRATUM,GIS_STATION&&cols;";
    qry<-gsub("&&cols",cols,qry)
    if (verbosity>1) cat("\nquery is:\n",qry,"\n");
    tbl_cpue<-sqldf(qry);
    
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
