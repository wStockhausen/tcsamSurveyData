#'
#'@title Function to extract NMFS crab survey hauls data from Excel csv file format by year.
#'
#'@param tbl_strata - data frame from call to selectStrata.TrawlSurvey(...) [required]
#'@param tbl        - trawl survey data table from previous call (ignored if NULL)
#'@param in.csv     - trawl survey data as csv file              (ignored if NULL)
#'@param export  - boolean flag to export results to csv file
#'@param out.csv - name of output csv file                    (ignored if NULL)
#'@param out.dir - base path for output csv file              (ignored if NULL)
#'@param Years      - vector of survey years to include in output             (ignored if NULL)
#'@param HaulTypes  - vector of haul types to include in output               (ignored if NULL)
#'@param YearRange  - vector of min, max survey years to include in output    (ignored if NULL)
#'@param DepthRange - vector of min, max haul depths to include in output     (ignored if NULL)
#'@param LatRange   - vector of min, max haul latitudes to include in output  (ignored if NULL)
#'@param LonRange   - vector of min, max haul longitudes to include in output (ignored if NULL)
#'@param verbosity : integer flag indicating level of printed output (0=off,1=minimal,2=full)
#'
#' @return a dataframe w/ columns \cr
#' YEAR,STRATUM,GIS_STATION,HAULJOIN,HAUL_TYPE, \cr
#' START_DATE,MID_LATITUDE,MID_LONGITUDE,BOTTOM_DEPTH, \cr
#' GEAR_TEMPERATURE,AREA_SWEPT_VARIABLE
#' 
#'@details If neither tbl or in.csv is given, the user will be prompted for a csv file via a file dialog box
#' 
#' @import sqldf
#' @importFrom tcltk tk_choose.files
#' @importFrom wtsUtilities addFilter
#' 
#' @export
#' 
#library("sqldf");
#library("tcltk")
#source("../Utilities/addFilter.R",chdir=TRUE)
selectHauls.TrawlSurvey<-function(tbl_strata,
                                  tbl=NULL,
                                  in.csv=NULL,
                                  export=FALSE,
                                  out.csv="SelectedSurveyHauls.csv",
                                  out.dir=NULL,
                                  Years=NULL,
                                  HaulTypes=NULL,
                                  YearRange=NULL,
                                  DepthRange=NULL,
                                  LatRange=NULL,
                                  LonRange=NULL,
                                  verbosity=1){
    if (verbosity>1) cat("starting selectHauls.TrawlSurvey.\n");
    
    if (is.null(tbl)){
        if (is.null(in.csv)) {
            Filters<-addFilter("csv","csv files (*.csv)","*.csv");
            in.csv<-tk_choose.files(caption=paste("Select AFSC crab trawl survey file"),
                                    multi=FALSE,filters=matrix(Filters[c("csv"),],1,2,byrow=TRUE));
            if (is.null(in.csv)|(in.csv=='')) return(NULL);
        }
        if (is.null(out.dir)) {
            out.dir<-dirname(file.path('.'));
            if (!is.null(in.csv)) {out.dir<-dirname(file.path(in.csv));}
        }
        if (verbosity>0) cat("Output directory for selectHauls.TrawlSurvey will be '",out.dir,"'\n",sep='');
        
        if (verbosity>1) cat("Reading AFSC crab trawl survey csv file for hauls info.\n",sep='')
        tbl<-read.csv(in.csv,stringsAsFactors=FALSE);
        if (verbosity>1) cat("Done reading input csv file.\n")
    }
    
    yrs<-tbl$START_DATE%%10000;
    tbl$YEAR<-yrs;
    uniq.yrs<-unique(yrs);
    if (verbosity>1) cat("survey years = {",paste(uniq.yrs,collapse=','),"}\n",sep='');
    
    cols<-c("HAUL_TYPE","START_DATE",
            "MID_LATITUDE","MID_LONGITUDE","BOTTOM_DEPTH",
            "GEAR_TEMPERATURE","AREA_SWEPT_VARIABLE");
    
    wc<-"and 1=1";#default where clause
    if (!is.null(Years))     {wc<-paste(wc,"and (t.YEAR in (",paste(Years,collapse=","),"))");}
    if (!is.null(HaulTypes)) {wc<-paste(wc,"and (t.HAUL_TYPE in (",paste(HaulTypes,collapse=","),"))");}
    if (!is.null(YearRange)) {wc<-paste(wc,"and (t.YEAR         between", YearRange[1],"and", YearRange[2],")");}
    if (!is.null(DepthRange)){wc<-paste(wc,"and (t.BOTTOM_DEPTH between",DepthRange[1],"and",DepthRange[2],")");}
    if (!is.null(LatRange))  {wc<-paste(wc,"and (t.MID_LATITUDE between",  LatRange[1],"and",  LatRange[2],")");}
    if (!is.null(LonRange))  {wc<-paste(wc,"and (t.MID_LONGITUDE between", LonRange[1],"and",  LonRange[2],")");}
    
    qry<-"select distinct 
            t.YEAR,
            s.STRATUM,
            t.GIS_STATION,
            t.HAULJOIN,
            &&cols 
          from 
            tbl as t,
            tbl_strata as s
          where
            t.YEAR=s.YEAR and t.GIS_STATION=s.GIS_STATION
            &&where
          order by t.YEAR,s.STRATUM,t.GIS_STATION,t.HAULJOIN;";
    qry<-gsub("&&cols",paste("t.",cols,sep='',collapse=","),qry);
    qry<-gsub("&&where",wc,qry);
    
    if (verbosity>1) cat("\nquery is:\n",qry,"\n");
    tbl1<-sqldf(qry);
        
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
        write.csv(tbl1,out.csv,na='',row.names=FALSE);
    }
    
    if (verbosity>1) cat("finished selectHauls.TrawlSurvey.\n");
    return(tbl1);
}
  
#tbl.hauls<-selectHauls.TrawlSurvey(tbl.strata,HaulTypes=3,export=TRUE,out.csv="SurveyHauls.BTC.StdHauls.csv");
