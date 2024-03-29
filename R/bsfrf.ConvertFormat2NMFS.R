#'
#' @title Convert BSFRF data to (almost) NMFS CRABHAUL_DATA format
#'
#' @description Function to convert BSFRF data to (almost) NMFS CRABHAUL_DATA format.
#'
#' @param tbl : filename for BSFRF  csv file to read or dataframe from reading csv file with [bsfrf.ReadCSV()]
#' @param types : BSFRF study types to select ("SBS", "IDX", or both)
#' @param size_col : column name with crab sizes (default = 'fixed_cw')
#' @param bsfrf_species : BSFRF species to extract (default = "BRD")
#' @param nmfs_species_code : appropriate NMFS species code (default = 68560 for Bairdi)
#' @param nmfs_species_name : appropriate NMFS species name (default = 'Bairdi Tanner Crab')
#' @param verbosity : integer flag to print debug info
#'
#' @return dataframe "almost" in CRABHAUL_DATA format.
#'
#' @details BSFRF station ids are not necessarily correct. For example, the
#' so-called corner stations in the NMFS survey grid use the format 'aaxxxx' whereas
#' the BSFRF format is typically 'aa-xxxx' , where aa is a 2-letter code and
#' xxxx is a 4-digit code. Also, NMFS station 'H-21' has also been mis-coded 'H21' in
#' the BSFRF data in some years. Rather than deal with these instances piecemeal,
#' the NMFS survey grid layers ([gisGetSurveyGridLayers()]) are used to reassign NMFS station ids to the
#' BSFRF data based on the mid-tow coordinates for the BSFRF data. The hauls
#' that will be changed can be identified by running [bsfrf.CheckStation()] on the
#' BSFRF haul dataframe/csv file before making the conversion here.
#'
#' @details returned dataframe has columns
#' \itemize{
#'   \item {AKFIN_SURVEY_YEAR - year}
#'   \item {HAULJOIN - paste(toupper(study),boat,start_date,start_hour,tow,sep=";")}
#'   \item {VESSEL - boat}
#'   \item {CRUISE - -1}
#'   \item {HAUL - tow}
#'   \item {HAUL_TYPE - toupper(study)}
#'   \item {START_DATE - character: mmddyyyy}
#'   \item {AKFIN_SURVEY_DATE - character: dd-mm-yyyy}
#'   \item {START_HOUR - character: hhmm}
#'   \item {MID_LATITUDE - midtowlatitude (numeric)}
#'   \item {MID_LONGITUDE - midtowlongitude (numeric)}
#'   \item {GIS_STATION - nmfs_stn}
#'   \item {BOTTOM_DEPTH -  depth_ftm, converted to meters}
#'   \item {GEAR_TEMPERATURE -  temp_c (in deg C)}
#'   \item {AREA_SWEPT_VARIABLE -  aswept_nm2 (in nm^2)}
#'   \item {SPECIES_CODE - nmfs_species_code}
#'   \item {SPECIES_NAME - nmfs_species_name}
#'   \item {SEX - sex}
#'   \item {WIDTH - sizeCol (in mm)}
#'   \item {SHELL_CONDITION - shell/Shell}
#'   \item {EGG_COLOR - col}
#'   \item {EGG_CONDITION - con}
#'   \item {CLUTCH_SIZE - full}
#'   \item {CHELA_HEIGHT - NA}
#'   \item {CALCULATED_WEIGHT - NA}
#'   \item {WEIGHT - NA}
#'   \item {SAMPLING_FACTOR - sampfactor}
#'   \item {CPUE_NUM - cpuenum (expanded number/nm^2)}
#' }
#'
#' @importFrom dplyr filter mutate select
#' @importFrom stringr str_sub str_replace
#' @importFrom tidyselect all_of
#' @importFrom sf st_drop_geometry st_join st_within
#' @importFrom wtsGIS createSF_points
#' @importFrom wtsUtilities selectFile
#'
#' @export
#'
bsfrf.ConvertFormat2NMFS<-function(tbl,
                                   types=c("SBS","IDX"),
                                   sizeCol="fixed_size",
                                   bsfrf_species="BRD",
                                   nmfs_species_code=68560,
                                   nmfs_species_name="Bairdi Tanner Crab",
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
      tbl<-bsfrf.ReadCSV(in.csv);
      if (verbosity>1) cat("Done reading input csv file.\n")
  }

  #--get NMFS station grid
  grid = tcsamSurveyData::gisGetSurveyGridLayers();

  #--select only "type" stations and
  #----do spatial join to classify hauls by NMFS survey station based on mid-haul locations
  sfHD = tbl |>
          dplyr::filter(tolower(study)%in%tolower(types),species==bsfrf_species) |>
           wtsGIS::createSF_points(xCol="midtowlongitude",yCol="midtowlatitude",crs=wtsGIS::get_crs(4326)) |>
           sf::st_join(grid$grid,join=sf::st_within);  #--now has "nmfs_stn" and "STATION_ID" columns

  tbl = sfHD |> sf::st_drop_geometry() |>
          dplyr::mutate(nmfs_stn=STATION_ID) |>
          dplyr::select(year,study,boat,tow,date,time,
                        midtowlongitude,midtowlatitude,nmfs_stn,depth_ftm,temp_c,aswept_nm2,
                        sex,tidyselect::all_of(sizeCol),shell,col,con,full,sampfactor,cpuenum);

  start_date<-format(as.Date(tbl$date,format="%m/%d/%y"),"%m%d%Y");
  start_hour<-stringr::str_sub(tbl$time,1,5) |> stringr::str_replace(":","");
  start_hour<-paste0(wtsUtilities::formatZeros(stringr::str_split_fixed(tbl$time,":",3)[,1],width=2),
                     wtsUtilities::formatZeros(stringr::str_split_fixed(tbl$time,":",3)[,2],width=2));

  tbl$AKFIN_SURVEY_YEAR  <-tbl$year;
  tbl$HAULJOIN           <-paste(toupper(tbl$study),tbl$boat,start_date,start_hour,tbl$tow,sep=";");
  tbl$VESSEL             <-tbl$boat;
  tbl$CRUISE             <--1;
  tbl$HAUL               <-tbl$tow;
  tbl$HAUL_TYPE          <-toupper(tbl$study);
  tbl$PERFORMANCE        <-0;
  tbl$START_DATE         <-start_date;
  tbl$AKFIN_SURVEY_DATE  <-format(as.Date(tbl$date,format="%m/%d/%y"),"%d-%m-%y");
  tbl$START_HOUR         <-start_hour;
  tbl$MID_LATITUDE       <-as.numeric(tbl$midtowlatitude);
  tbl$MID_LONGITUDE      <-as.numeric(tbl$midtowlongitude);
  tbl$GIS_STATION        <-tbl$nmfs_stn;
  tbl$BOTTOM_DEPTH       <-as.numeric(tbl$depth_ftm*6*(0.3048));
  tbl$GEAR_TEMPERATURE   <-as.numeric(tbl$temp_c);
  tbl$AREA_SWEPT_VARIABLE<-as.numeric(tbl$aswept_nm2);
  tbl$SPECIES_CODE       <-nmfs_species_code;
  tbl$SPECIES_NAME       <-nmfs_species_name;
  tbl$SEX                <-as.integer(tbl$sex);
  tbl$WIDTH              <-as.numeric(tbl[[tolower(sizeCol)]]);
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
  tbl<-tbl |> dplyr::select(tidyselect::all_of(cols));

  #make sure that female variables for males are NA
  idx<-tbl$SEX==1;
  tbl$EGG_COLOR[idx]    <- NA;
  tbl$EGG_CONDITION[idx]<- NA;
  tbl$CLUTCH_SIZE[idx]  <- NA;

  #keep only tows with recorded lat/lons
  tbl<-tbl |> dplyr::filter(!is.na(MID_LATITUDE),!is.na(MID_LATITUDE));

  return(tbl);
}
