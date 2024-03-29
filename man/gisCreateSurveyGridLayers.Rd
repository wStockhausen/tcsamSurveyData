% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gisCreateSurveyGridLayers.R
\name{gisCreateSurveyGridLayers}
\alias{gisCreateSurveyGridLayers}
\title{Create \pkg{sf} dataframes for a survey grid from polygon and point shapefiles}
\usage{
gisCreateSurveyGridLayers(
  gisPath = NULL,
  shapeFiles = list(grid = NULL, stations = NULL),
  final_crs = wtsGIS::get_crs("WGS84")
)
}
\arguments{
\item{gisPath}{\itemize{
\item path to common toplevel folder for files
}}

\item{shapeFiles}{\itemize{
\item a list of shapefiles to read ("grid" and "stations", either can be NULL to skip creation of that layer)
}}

\item{final_crs}{\itemize{
\item a \code{\link[sf:st_crs]{sf::st_crs()}} object to be used as the final coordinate reference system for the spatial layers
}}
}
\value{
a 2-element list with the grid and stations spatial layers as \pkg{sf} dataframes.
}
\description{
This function reads shapefiles and creates \pkg{sf} polygon and point dataframes for a survey grid.
}
\details{
None.
}
\seealso{
\code{\link[wtsGIS:getPackagedLayer]{wtsGIS::getPackagedLayer()}} for pre-packaged \pkg{sf} datasets that will be
used in the event that input \code{grid} and \code{stations} shapefiles are NULL (the default).
}
