#'
#'@title Extract crab strata and associated station information from a NMFS trawl survey strata file
#'
#'@description Function to extract crab strata and associated station information from a NMFS trawl survey strata file.
#'
#'@param tbl        : table (dataframe) of survey strata /stationdata (or name of survey strata/station csv file, or NULL)
#'@param species    : code ('BKC','BTC','RKC','OTC') indicating species
#'@param strataType : type of strata ('orig','revd','2015')
#'@param export     : boolean flag to export results to csv file
#'@param out.csv    : output file name
#'@param out.dir    : output file directory
#'@param verbosity  : integer flag indicating level of printed output (0=off,1=minimal,2=full)
#'
#'@details If tbl is NULL, the user will be prompted for a csv file via a file dialog box.
#'Returned dataframe has columns:
#'\itemize{\item {YEAR}
#'         \item {STRATUM}
#'         \item {STRATUM_CODE}
#'         \item {STRATUM_AREA}
#'         \item {GIS_STATION}
#'         \item {STATION_LONGITUDE}
#'         \item {STATION_LATITUDE}
#'         \item {STATION_AREA}
#'         }
#' Area is in square nautical miles.
#'
#' @return a dataframe with strata/stations info.
#'
#' @importFrom readr read_csv
#' @importFrom sqldf sqldf
#' @importFrom wtsUtilities selectFile
#'
#' @export
#'
selectStrata.TrawlSurvey<-function(tbl=NULL,
                                   species='BTC',
                                   strataType='2015',
                                   export=FALSE,
                                   out.csv=paste('SelectedStations',species,'csv',sep='.'),
                                   out.dir=NULL,
                                   verbosity=0){
    if (verbosity>0) cat("starting selectStrata.TrawlSurvey.\n");

    in.csv<-NULL;
    if (!is.data.frame(tbl)){
        if (!is.character(tbl)) {
            in.csv<-wtsUtilities::selectFile(ext="csv",caption=paste("Select AFSC crab survey strata file for strata type",strataType));
            if (is.null(in.csv)|(in.csv=='')) return(NULL);
        } else {
            in.csv<-tbl;#tbl is a filename
        }
        if (verbosity>1) cat("Reading AFSC crab survey strata file (csv) for station info.\n",sep='')
        tbl<-readr::read_csv(in.csv,guess_max=1000000);
        if (verbosity>1) cat("Done reading input csv file.\n")
    }

    if (is.null(out.dir)) {
        out.dir<-dirname(file.path('.'));
        if (!is.null(in.csv)) {out.dir<-dirname(file.path(in.csv));}
    }
    if (verbosity>0) cat("Output directory for selectStrata.TrawlSurvey will be '",out.dir,"'\n",sep='');

    #rearrange columns, drop some
    if ("TOTAL_AREA_SQ_NM" %in% names(tbl)){
      cols<-c("SURVEY_YEAR","STATION_ID","STRATUM","TOTAL_AREA_SQ_NM","LONGITUDE","LATITUDE");
      tbl = tbl |> dplyr::select(SURVEY_YEAR,STATION_ID,
                                 STRATUM_CODE=STRATUM,TOTAL_AREA=TOTAL_AREA_SQ_NM,
                                 LONGITUDE,LATITUDE);
    } else if ("TOTAL_AREA" %in% names(tbl)){
      cols<-c("SURVEY_YEAR","STATION_ID","STRATUM","TOTAL_AREA","LONGITUDE","LATITUDE");
      tbl = tbl |> dplyr::select(SURVEY_YEAR,STATION_ID,
                                 STRATUM_CODE=STRATUM,TOTAL_AREA,
                                 LONGITUDE,LATITUDE);
    }

    # qry<-"select distinct
    #         SURVEY_YEAR, STRATUM_CODE, TOTAL_AREA
    #       from tbl
    #       order by SURVEY_YEAR, STRATUM_CODE;"
    # tbl_areas<-sqldf::sqldf(qry);
    tbl_areas = tbl |> dplyr::distinct(SURVEY_YEAR, STRATUM_CODE, TOTAL_AREA);

    strata<-NULL;
    codes<-Codes.TrawlSurvey();
    if (tolower(strataType)=='orig'){strata<-codes[[paste("strata.orig",species,sep='.')]];} else
    if (tolower(strataType)=='revd'){strata<-codes[[paste("strata.revd",species,sep='.')]];} else
    if (tolower(strataType)=='2015'){strata<-codes[[paste("strata.2015",species,sep='.')]];} else
    {cat("strataType '",strataType,"' for species '",species,"' not recognized.\nAborting...\n");
     return(NULL);}

    if (is.null(strata)){
        cat('table strata is NULL for strataType = "',strataType='"\nAborting...\n');
        return(NULL);
    }

    #--substitute STRATUM name for STRATUM_CODE
    #----older strata definitions combine hot spot and multiple tow strata
    #----into single tow strata, hence grouping/summation
    # qry<-"select
    #         SURVEY_YEAR,
    #         STRATUM,
    #         sum(TOTAL_AREA) as STRATUM_AREA
    #       from
    #         tbl_areas as t,
    #         strata as s
    #       where
    #         t.STRATUM_CODE=s.code
    #       group by
    #         SURVEY_YEAR,STRATUM
    #       order by
    #         SURVEY_YEAR,STRATUM;"
    # if (verbosity>1) cat("\nquery is:\n",qry,"\n");
    # tbl_areas1<-sqldf::sqldf(qry);
    tbl_areas1 = tbl_areas |>
                   dplyr::inner_join(strata,by=c("STRATUM_CODE"="code")) |>
                   dplyr::group_by(SURVEY_YEAR,stratum) |>
                   dplyr::summarize(STRATUM_AREA=sum(TOTAL_AREA)) |>
                   dplyr::ungroup();

    # qry<-"select
    #         t.SURVEY_YEAR as YEAR,
    #         s.STRATUM as STRATUM,
    #         t.STRATUM_CODE as STRATUM_CODE,
    #         a.STRATUM_AREA as STRATUM_AREA,
    #         t.STATION_ID as GIS_STATION,
    #         t.LONGITUDE as STATION_LONGITUDE,
    #         t.LATITUDE as STATION_LATITUDE
    #       from
    #         tbl as t,
    #         strata as s,
    #         tbl_areas1 as a
    #       where
    #         t.SURVEY_YEAR=a.SURVEY_YEAR and
    #         t.STRATUM_CODE=s.code and
    #         s.STRATUM=a.STRATUM
    #       order by
    #         t.SURVEY_YEAR,s.STRATUM,t.STATION_ID;"
    #
    # if (verbosity>1) cat("\nquery is:\n",qry,"\n");
    # tbl1<-sqldf::sqldf(qry)

    tbl1 = tbl |>
              dplyr::inner_join(strata,by=c("STRATUM_CODE"="code")) |>
              dplyr::inner_join(tbl_areas1,by=c("SURVEY_YEAR","stratum")) |>
              dplyr::arrange(SURVEY_YEAR,stratum,STATION_ID) |>
              dplyr::select(YEAR=SURVEY_YEAR,
                            STRATUM=stratum,
                            STRATUM_CODE,
                            STRATUM_AREA,
                            GIS_STATION=STATION_ID,
                            STATION_LONGITUDE=LONGITUDE,
                            STATION_LATITUDE=LATITUDE);
    #--add STATION_AREA to table
    tbl2<-addStationAreasToStrataDataframe(tbl1);

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
        write.csv(tbl2,out.csv,na='',row.names=FALSE);
    }

    if (verbosity>1) cat("finished selectStrata.TrawlSurvey.\n");
    return(tbl2)
}

#tbl.BTC.stns<-selectStations.TrawlSurvey(species='BTC')
