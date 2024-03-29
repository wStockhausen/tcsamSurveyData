% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcEnvData.ByStation.R
\name{calcEnvData.ByStation}
\alias{calcEnvData.ByStation}
\title{Function to calculate environmental data by survey station, averaging over values by haul.}
\usage{
calcEnvData.ByStation(
  tbl_strata = NULL,
  tbl_hauls = NULL,
  export = FALSE,
  out.csv = "envData.ByStation.csv",
  out.dir = NULL,
  verbosity = 0
)
}
\arguments{
\item{tbl_strata}{: dataframe w/ station info}

\item{tbl_hauls}{: data frame w/ haul data, or name of csv file w/ haul data, or NULL to choose file}

\item{export}{: boolean flag to write results to csv file}

\item{out.csv}{: output file name}

\item{out.dir}{: output file directory}

\item{verbosity}{: integer flag indicating level of printed output (0=off,1=minimal,2=full)}
}
\value{
A dataframe of environmental data (bottom depth, temperature) by year, stratum, survey station. See Details.
}
\description{
Function to calculate environmental data by survey station, averaging over values by haul.
}
\details{
Notes: \cr
\itemize{
\item   mean bottom depth in m
\item   mean bottom (gear) temperature in deg C
} \cr
The returned dataframe has the following columns: \cr
\itemize{
\item   YEAR
\item   STRATUM
\item   GIS_STATION
\item   LONGITUDE
\item   LATITUDE
\item   numHauls
\item   BOTTOM_DEPTH
\item   BOTTOM_TEMP
}
}
