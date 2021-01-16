#'
#' @title Retrieve the EBS bottom trawl survey grid as pre-packaged polygon and point \pkg{sf} dataframes
#'
#' @description This function retrieves pre-packaged \pkg{sf} polygon and point dataframes for the
#' EBS bottom trawl survey.
#'
#' @return a 2-element list with the EBS trawl survey grid and station information as \pkg{sf} dataframes.
#'
#' @details None.
#'
#' @export
#'
gisGetSurveyGridLayers<-function(){
  load(system.file("extdata/surveyGridLayers.RData",package="tcsamSurveyData"));
  return(surveyGridLayers);
}
