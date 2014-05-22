#'
#'@title Function to calculate cpue by survey station, averaging over cpue by haul. 
#'
#'@param   tbl_cpue : data frame w/ cpue by haul from call to calcCPUE.ByHaul(...)
#'@param   in.csv    : name of csv file w/ cpue by haul (if tbl_cpue is not given)
#'@param export  : boolean flag to write results to csv file
#'@param out.csv : output file name
#'@param out.dir : output file directory 
#'
#'@return   data frame of cpue (numbers and weight) by year, stratum, station and other factor levels
#'
#'@details \cr
#'Notes: \cr
#'   CPUE in numbers is in no/(sq. nm.) \cr
#'   CPUE in weight  is in mt/(sq. nm.) \cr
#'   CPUE is calculated only for stations at which at least 1 haul was conducted
#'
#' @import sqldf
#' @importFrom tcltk tk_choose.files
#' @importFrom wtsUtilities addFilter
#'      
#'@export
#'
calcCPUE.ByStation<-function(tbl_cpue=NULL,
                             in.csv=NULL,
                             export=FALSE,
                             out.csv='cpue.ByStation.csv',
                             out.dir=NULL){
    if (is.null(tbl_cpue)){
        cat("Reading csv file for cpue by haul results.\n",sep='')
        if (is.null(in.csv)) {
            Filters<-addFilter("csv","csv files (*.csv)","*.csv");
            in.csv<-tk_choose.files(caption=paste("Select csv file with cpue by haul info"),
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
    cat("\nquery is:\n",qry,"\n");
    tbl_cpue<-sqldf(qry);
    
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
        write.csv(tbl_cpue,out.csv,na='',row.names=FALSE);
    }
    
    return(tbl_cpue)
}

#tbl.cpue.byS<-calcCPUE.ByStation(tbl.cpue.byH,export=FALSE)
