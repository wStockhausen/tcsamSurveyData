#'
#' @title Convert BSFRF data to (almost) NMFS CRABHAUL_DATA format
#'
#' @description Function to convert BSFRF data to (almost) NMFS CRABHAUL_DATA format.
#'
#' @param tbl : filename for BSFRF  csv file to read or dataframe from reading csv file
#' @param types : BSFRF study types to select ("SBS", "IDX", or both)
#' @param verbosity : integer flag to print debug info
#'
#' @return dataframe "almost" in CRABHAUL_DATA format.
#'
#' @details returned dataframe has columns
#' \itemize{
#'   \item {AKFIN_SURVEY_YEAR}
#'   \item {HAULJOIN}
#'   \item {VESSEL}
#'   \item {CRUISE}
#'   \item {HAUL}
#'   \item {HAUL_TYPE}
#'   \item {START_DATE}
#'   \item {AKFIN_SURVEY_DATE}
#'   \item {START_HOUR}
#'   \item {MID_LATITUDE}
#'   \item {MID_LONGITUDE}
#'   \item {GIS_STATION}
#'   \item {BOTTOM_DEPTH (in meters)}
#'   \item {GEAR_TEMPERATURE (deg C)}
#'   \item {AREA_SWEPT_VARIABLE (nm^2)}
#'   \item {SPECIES_CODE}
#'   \item {SPECIES_NAME}
#'   \item {SEX}
#'   \item {WIDTH (mm)}
#'   \item {SHELL_CONDITION}
#'   \item {EGG_COLOR}
#'   \item {EGG_CONDITION}
#'   \item {CLUTCH_SIZE}
#'   \item {CHELA_HEIGHT}
#'   \item {CALCULATED_WEIGHT}
#'   \item {WEIGHT}
#'   \item {SAMPLING_FACTOR}
#'   \item {CPUE_NUM}
#' }
#'
#' @importFrom utils read.csv write.csv
#'
#' @export
#'
convertFormat.BSFRF2NMFS<-function(tbl,
                                   types=c("SBS","IDX"),
                                   verbosity=TRUE){
  in.csv<-NULL;
  if (!is.data.frame(tbl)){
    #read data file
      if (!is.character(tbl)) {
          in.csv<-wtsUtilities::selectFile(ext="csv",caption="Select BSFRF trawl survey file");
          if (is.null(in.csv)|(in.csv=='')) return(NULL);
      } else {
          in.csv<-tbl;#tbl is a filename
      }
      if (verbosity>1) cat("Reading BSFRF trawl survey csv.\n",sep='')
      tbl<-read.csv(in.csv,check.names=FALSE,stringsAsFactors=FALSE);
      if (verbosity>1) cat("Done reading input csv file.\n")
  }

  #Reformat input table
  ##-convert column names to lower case
  names(tbl)<-tolower(names(tbl));
  ##-select only "type" stations
  tbl<-tbl[tbl$study%in%types,];

  start_date<-as.numeric(format(as.Date(tbl$date,format="%m/%d/%y"),"%m%d%Y"));

  tbl$AKFIN_SURVEY_YEAR  <-tbl$year;
  tbl$HAULJOIN           <-paste(tbl$study,tbl$boat,start_date,tbl$tow,sep=";");
  tbl$VESSEL             <-tbl$boat;
  tbl$CRUISE             <--1;
  tbl$HAUL               <-tbl$tow;
  tbl$HAUL_TYPE          <-tbl$study;
  tbl$PERFORMANCE        <-0;
  tbl$START_DATE         <-start_date;
  tbl$AKFIN_SURVEY_DATE  <-format(as.Date(tbl$date,format="%m/%d/%y"),"%d-%m-%y");
  tbl$START_HOUR         <-tbl$time;
  tbl$MID_LATITUDE       <-as.numeric(tbl$midtowlatitude);
  tbl$MID_LONGITUDE      <-as.numeric(tbl$midtowlongitude);
  tbl$GIS_STATION        <-tbl$nmfs_stn;
  tbl$BOTTOM_DEPTH       <-as.numeric(tbl$depth_ftm*6*(0.3048));
  tbl$GEAR_TEMPERATURE   <-as.numeric(tbl$temp_c);
  tbl$AREA_SWEPT_VARIABLE<-as.numeric(tbl$aswept_nm2);
  tbl$SPECIES_CODE       <-68560;
  tbl$SPECIES_NAME       <-"Bairdi Tanner Crab";
  tbl$SEX                <-as.integer(tbl$sex);
  tbl$WIDTH              <-as.numeric(tbl$fixed_cl);
  tbl$SHELL_CONDITION    <-as.integer(tbl$shell);
  tbl$EGG_COLOR          <-as.integer(tbl$col);
  tbl$EGG_CONDITION      <-as.integer(tbl$con);
  tbl$CLUTCH_SIZE        <-as.integer(tbl$full);
  tbl$CHELA_HEIGHT       <-as.numeric(NA);
  tbl$CALCULATED_WEIGHT  <-as.numeric(NA);
  tbl$WEIGHT             <-as.numeric(NA);
  tbl$SAMPLING_FACTOR    <-as.numeric(tbl$sampfactor);
  tbl$CPUE_NUM           <-as.numeric(tbl$cpuenum);

  ##-keep only columns of interest
  cols<-c("AKFIN_SURVEY_YEAR","HAULJOIN","VESSEL","CRUISE","HAUL","HAUL_TYPE",
          "START_DATE","AKFIN_SURVEY_DATE","START_HOUR",
          "MID_LATITUDE","MID_LONGITUDE","GIS_STATION",
          "BOTTOM_DEPTH","GEAR_TEMPERATURE","AREA_SWEPT_VARIABLE",
          "SPECIES_CODE","SPECIES_NAME","SEX","WIDTH","SHELL_CONDITION",
          "EGG_COLOR","EGG_CONDITION","CLUTCH_SIZE",
          "CHELA_HEIGHT","CALCULATED_WEIGHT","WEIGHT","SAMPLING_FACTOR","CPUE_NUM");
  tbl<-tbl[,cols];

  #make sure that female variables for males are NA
  idx<-tbl$SEX==1;
  tbl$EGG_COLOR[idx]    <- NA;
  tbl$EGG_CONDITION[idx]<- NA;
  tbl$CLUTCH_SIZE[idx]  <- NA;

  #keep only tows with recorded lat/lons
  idx<-is.na(tbl$MID_LATITUDE)|is.na(tbl$MID_LATITUDE);
  tbl<-tbl[!idx,];

  return(tbl);
}
