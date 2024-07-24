#'
#' tcsamSurveyData: a package for processing NMFS and BSFRF survey data
#'
#' The tcsamSurveyData package provides functions to process NMFS and
#'   BSFRF survey data
#'
#' @section Rmd files:
#' This package provides several Rmd scripts to create pdf documents of certain
#' workflows:
#' \itemize{
#'   \item {calcCPUE.Surveys.NMFS.Rmd}       {- calculates survey CPUE by year and factor levels and writes results to cpue.csv}
#'   \item {calcEffectiveN.SurveyData.NMFS.Rmd} {- produces a report on effective N's for size compositions calculated using \code{calcResampledSizeComps}}
#'   \item {makeMaps.Surveys.Basemap.Rmd}    {- create a basemap for the NMFS EBS survey}
#'   \item {makeMaps.Surveys.CPUE.NMFS.Rmd}  {- creates a pdf/docx with maps of annual spatial CPUE (by factor levels) superimposed on spatial environmental data}
#'   \item {makeMaps1.Surveys.CPUE.NMFS.Rmd} {- deprecated: superseded by makeMaps.Surveys.CPUE.NMFS.Rmd}
#'   \item {makeMaps2.Surveys.CPUE.NMFS.Rmd} {- deprecated: superseded by makeMaps.Surveys.CPUE.NMFS.Rmd}
#'   \item {mapCPUE.Surveys.NMFS.Rmd}        {- deprecated: superseded by makeMaps.Surveys.CPUE.NMFS.Rmd}
#' }
#' These are available under inst/rmd and can be referenced using
#' \code{system.file("rmd/rmdFileName",package="tcsamSurveyData")},
#' where \code{rmdFileName} is the name of the Rmd file.
#'
#' @keywords internal
#' @name tcsamSurveyData
#'
"_PACKAGE"
