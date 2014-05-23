#'
#'@title Function to extract crab station data and strata information from a NMFS trawl survey strata file.
#'
#'@param tbl_hauls : hauls table (dataframe) from call to selectHauls.TrawlSurvey(...) [required]
#'@param tbl       : table (dataframe) of survey station/strata data (from read.csv() called on survey station/strata csv file)
#'@param in.csv    : name of survey station/strata csv file to read (if tbl is not given)
#'@param species   : code ('BKC','BTC','RKC','OTC') indicating species
#'@param useOrigStrata: boolean to use Bob Foy's "original" strata, rather than the revised strata
#'@param export    : boolean flag to export results to csv file
#'@param verbosity : integer flag indicating level of printed output (0=off,1=minimal,2=full)
#'
#'@details If neither tbl or in.csv is given, the user will be prompted for a csv file via a file dialog box
#'
#' @return a dataframe with strata
#' 
#' @import sqldf
#' @importFrom tcltk tk_choose.files
#' @importFrom wtsUtilities addFilter
#' 
#' @export
#' 
#library("sqldf");
#library("tcltk")
#source("Codes.TrawlSurvey.R",chdir=TRUE)
#source("../Utilities/addFilter.R",chdir=TRUE)
selectStrata.TrawlSurvey<-function(tbl_hauls,
                                   tbl=NULL,
                                   in.csv=NULL,
                                   species='BTC',
                                   useOrigStrata=FALSE,
                                   export=FALSE,
                                   out.csv=paste('SelectedStations',species,'csv',sep='.'),
                                   out.dir=NULL,
                                   verbosity=1){
    if (verbosity>1) cat("starting selectStations.TrawlSurvey.\n");
    
    if (is.null(tbl)){
        if (is.null(in.csv)) {
            Filters<-addFilter("csv","csv files (*.csv)","*.csv");
            in.csv<-tk_choose.files(caption=paste("Select AFSC crab survey strata file"),
                                    multi=FALSE,filters=matrix(Filters[c("csv"),],1,2,byrow=TRUE));
            if (is.null(in.csv)|(in.csv=='')) return(NULL);
        }
        if (is.null(out.dir)) {
            out.dir<-dirname(file.path('.'));
            if (!is.null(in.csv)) {out.dir<-dirname(file.path(in.csv));}
        }
        if (verbosity>0) cat("Output directory for selectStations.TrawlSurvey will be '",out.dir,"'\n",sep='');
        
        if (verbosity>1) cat("Reading AFSC crab survey strata file (csv) for station info.\n",sep='')
        tbl<-read.csv(in.csv,stringsAsFactors=FALSE);
        if (verbosity>1) cat("Done reading input csv file.\n")
    }
    
    #rearrange columns, drop some
    cols<-c("SURVEY_YEAR","STATION_ID","STRATUM","TOTAL_AREA")
    tbl<-tbl[,cols];
    new.cols<-c("SURVEY_YEAR","STATION_ID","STRATUM_CODE","TOTAL_AREA")
    names(tbl)<-new.cols;
    
    qry<-"select distinct
            SURVEY_YEAR, STRATUM_CODE, TOTAL_AREA
          from tbl
          order by SURVEY_YEAR, STRATUM_CODE;"
    tbl_areas<-sqldf(qry);
    
    codes<-Codes.TrawlSurvey();
    if (!useOrigStrata) {strata<-codes[[paste("strata.",species,sep='')]];} else
    {strata<-codes[[paste("strata.orig.",species,sep='')]];}
    
    qry<-"select
            SURVEY_YEAR,
            STRATUM,
            sum(TOTAL_AREA) as STRATUM_AREA
          from
            tbl_areas as t,
            strata as s
          where
            t.STRATUM_CODE=s.code
          group by
            SURVEY_YEAR,STRATUM
          order by
            SURVEY_YEAR,STRATUM;"
    if (verbosity>1) cat("\nquery is:\n",qry,"\n");
    tbl_areas1<-sqldf(qry);
    
    qry<-"select
            t.SURVEY_YEAR as YEAR,
            s.STRATUM as STRATUM,
            t.STRATUM_CODE as STRATUM_CODE,
            a.STRATUM_AREA as STRATUM_AREA,
            t.STATION_ID as GIS_STATION
          from
            tbl as t,
            strata as s,
            tbl_areas1 as a
          where
            t.SURVEY_YEAR=a.SURVEY_YEAR and
            t.STRATUM_CODE=s.code and
            s.STRATUM=a.STRATUM
          order by
            t.SURVEY_YEAR,s.STRATUM,t.STATION_ID;"
    
    if (verbosity>1) cat("\nquery is:\n",qry,"\n");
    tbl<-sqldf(qry)
    
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
        write.csv(tbl,out.csv,na='',row.names=FALSE);
    }
    
    if (verbosity>1) cat("finished selectStations.TrawlSurvey.\n");
    return(tbl)
}

#tbl.BTC.stns<-selectStations.TrawlSurvey(species='BTC')
