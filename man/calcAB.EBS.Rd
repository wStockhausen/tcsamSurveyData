% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcAB.EBS.R
\name{calcAB.EBS}
\alias{calcAB.EBS}
\title{Calculate total abundance and biomass from a by-stratum data frame or csv file.}
\usage{
calcAB.EBS(
  tbl = NULL,
  in.csv = NULL,
  export = TRUE,
  out.csv = "SurveyBiomass.EBS.csv",
  out.dir = NULL,
  verbosity = 0
)
}
\arguments{
\item{tbl}{: data frame with abundance/biomass by stratum info from call to \code{\link{calcAB.ByStratum} or \link{calcAB.EW166}}, or a csv file from such a call, or NULL}

\item{in.csv}{: csv filename from which to read input dataframe}

\item{export}{: boolean flag to write results to csv file}

\item{out.csv}{: output file name}

\item{out.dir}{: output file directory}

\item{verbosity}{: integer flag indicating level of printed output (0=off,1=minimal,2=full)}
}
\value{
data frame with columns: \cr
\itemize{
\item  YEAR = survey year
\item  STRATUM         = 'EBS'
\item  STRATUM_AREA    = area of stratum
\item  other user-defined factors (e.g., sex, shell_condition)
\item  numStations     = number of stations included
\item  numHauls        = number of hauls included
\item  numNonZeroHauls = number of hauls included
\item  numIndivs       = number of individuals sampled
\item  totABUNDANCE = total abundance estimate
\item  stdABUNDANCE = std deviation of total abundance estimate
\item  cvABUNDANCE  = cv of total abundance estimate
\item  totBIOMASS = estimate of total biomass estimate
\item  stdBIOMASS = std deviation of total biomass estimate
\item  cvBIOMASS  = cv of total biomass estimate
}
}
\description{
This function calculates total EBS abundance and biomass from a by-stratum data frame or csv file.
}
\details{
If \code{tbl} and \code{in.csv} are both NULL, the user is prompted to enter a csv file with biomass by stratum info. \cr
\cr Notes: \cr
\itemize{
\item   Area is in square nautical miles
\item   Abundance is in 10^6 individuals
\item   Biomass   is in 10^3 mt
}
}
