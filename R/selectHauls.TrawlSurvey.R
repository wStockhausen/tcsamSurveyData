#'
#'@title Function to extract NMFS crab survey hauls data from Excel csv file format by year.
#'
#'@description Function to extract NMFS crab survey hauls data from Excel csv file format by year.
#'
#'@param tbl_strata : dataframe from call to selectStrata.TrawlSurvey(...) \[required\]
#'@param tbl        : trawl survey dataframe from previous call (or name of trawl survey csv datafile, or NULL)              (ignored if NULL)
#'@param Years      : vector of survey years to include in output             (ignored if NULL)
#'@param HaulTypes  : vector of haul types to include in output               (ignored if NULL)
#'@param YearRange  : vector of min, max survey years to include in output    (ignored if NULL)
#'@param DepthRange : vector of min, max haul depths to include in output     (ignored if NULL)
#'@param LatRange   : vector of min, max haul latitudes to include in output  (ignored if NULL)
#'@param LonRange   : vector of min, max haul longitudes to include in output (ignored if NULL)
#'@param skip : number of rows to skip when reading crabhaul csv file (default=6 to match AKFIN crabhaul download)
#'@param export  : boolean flag to export results to csv file
#'@param out.csv : name of output csv file                    (ignored if NULL)
#'@param out.dir : base path for output csv file              (set to folder of input csv file or current working directory)
#'@param verbosity : integer flag indicating level of printed output (0=off,1=minimal,2=full)
#'
#' @return a dataframe w/ columns \cr
#' \itemize{
#' \item{YEAR}
#' \item{STRATUM}
#' \item{GIS_STATION}
#' \item{HAULJOIN}
#' \item{HAUL_TYPE}
#' \item{START_DATE}
#' \item{MID_LATITUDE}
#' \item{MID_LONGITUDE}
#' \item{BOTTOM_DEPTH}
#' \item{GEAR_TEMPERATURE}
#' \item{AREA_SWEPT_VARIABLE}
#' }
#'
#'@details If neither tbl or in.csv is given, the user will be prompted for a csv file via a file dialog box
#'
#' @importFrom readr read_csv
#' @importFrom sqldf sqldf
#' @importFrom wtsUtilities selectFile
#'
#' @export
#'
selectHauls.TrawlSurvey<-function(tbl_strata,
                                  tbl=NULL,
                                  Years=NULL,
                                  HaulTypes=NULL,
                                  YearRange=NULL,
                                  DepthRange=NULL,
                                  LatRange=NULL,
                                  LonRange=NULL,
                                  skip=6,
                                  export=FALSE,
                                  out.csv="SelectedSurveyHauls.csv",
                                  out.dir=NULL,
                                  verbosity=0){
    if (verbosity>1) cat("starting selectHauls.TrawlSurvey.\n");


    if (!is.data.frame(tbl_strata)) {
        cat("Error in selectHauls.TrawlSurvey:",
            "tbl_strata is NULL. Must supply tbl_strata.",
            "Aborting...",sep='\n');
        return(NULL);
    }

    in.csv<-NULL;
    if (!is.data.frame(tbl)){
        if (!is.character(tbl)) {
            in.csv<-selectFile(ext="csv",caption="Select AFSC crab trawl survey file");
            if (is.null(in.csv)|(in.csv=='')) return(NULL);
        } else {
            in.csv<-tbl;#tbl is a filename
        }
        if (verbosity>1) cat("Reading AFSC crab trawl survey csv file for hauls info, skipping first 5 lines.\n",sep='')
        tbl<-readr::read_csv(in.csv,skip=skip,guess_max=10000000);
        if (verbosity>1) cat("Done reading input csv file.\n")
    }

    #----check for possibly changed column names
    if (!('AREA_SWEPT_VARIABLE' %in% names(tbl))){
      idx = which(names(tbl)=="AREA_SWEPT");
      names(tbl)[idx] = 'AREA_SWEPT_VARIABLE';
    }

    if (is.null(out.dir)) {
        out.dir<-dirname(file.path('.'));
        if (!is.null(in.csv)) {out.dir<-dirname(file.path(in.csv));}
    }
    if (verbosity>0) cat("Output directory for selectHauls.TrawlSurvey will be '",out.dir,"'\n",sep='');

    req_cols<-c("HAUL_TYPE","START_DATE","GIS_STATION","HAULJOIN",
                "MID_LATITUDE","MID_LONGITUDE","BOTTOM_DEPTH",
                "GEAR_TEMPERATURE","AREA_SWEPT_VARIABLE");
    if (any(!(req_cols %in% names(tbl)))){
      msg<-paste0("\n#--ERROR in selectHauls.TrawlSurvey:\n",
                  "Required column(s) ",paste0("'",req_cols[!(req_cols %in% names(tbl))],"'",collapse=", "),"\n",
                  "were not found in the input crab haul dataframe or csv file (tbl).\n",
                  "It's column names were: ",paste0(names(tbl),collapse=", "),"\n");
      stop(msg);
    }

    if ("AKFIN_SURVEY_YEAR" %in% names(tbl)){
      yrs<-tbl$AKFIN_SURVEY_YEAR;
    } else {
      yrs = as.numeric(tbl$START_DATE) %% 10000;
    }
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
