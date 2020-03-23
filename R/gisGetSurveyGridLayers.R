#'
#' @title Retrieve the polygon and point geometry tmap layers from polygon and point shapefiles for a survey grid
#'
#' @description This function retrieves the polygon and point tmap layers for a survey grid.
#'
#' @return a 2-element list with the grid and stations map layers consistent with the tmap package
#'
#' @details None.
#'
#' @export
#'
gisGetSurveyGridLayers<-function(){
  load(system.file("extdata/surveyGridLayers.RData",package="tcsamSurveyData"));
  return(surveyGridLayers);
}
