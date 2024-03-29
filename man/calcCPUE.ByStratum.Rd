% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcCPUE.ByStratum.R
\name{calcCPUE.ByStratum}
\alias{calcCPUE.ByStratum}
\title{Calculate average cpue, numbers and biomass by stratum from AFSC trawl survey data.}
\usage{
calcCPUE.ByStratum(
  tbl_strata,
  tbl_cpue = NULL,
  export = FALSE,
  out.csv = "SurveyCPUE.ByStratum.csv",
  out.dir = NULL,
  verbosity = 0
)
}
\arguments{
\item{tbl_strata}{: data frame w/ stations/strata from call to \link{selectStrata.TrawlSurvey}}

\item{tbl_cpue}{: data frame w/ cpue by year, station, other factor levels (or csv filename or NULL)}

\item{export}{: boolean flag to write results to csv file}

\item{out.csv}{: output file name}

\item{out.dir}{: output file directory}

\item{verbosity}{: integer flag indicating level of printed output (0=off,1=minimal,2=full)}
}
\value{
data frame with average cpue (numbers, weight), abundance and biomass by stratum. Columns are \cr
\itemize{
\item  YEAR
\item  STRATUM
\item  STRATUM_AREA
\item  other user-defined factors
\item  numStations
\item  numHauls
\item  numNonZeroHauls
\item  numIndivs
\item  avgNumCPUE = average cpue (numbers) by stratum
\item  stdNumCPUE = std deviation of cpue (numbers) by stratum
\item  cvNumCPUE  = cv of cpue (numbers) by stratum
\item  avgWgtCPUE = average cpue (weight) by stratum
\item  stdWgtCPUE = std deviation of cpue (weight) by stratum
\item  cvWgtCPUE  = cv of estimated cpue (weight) by stratum
}
}
\description{
This function calculates average cpue, numbers and biomass by stratum from cpue (by survey station or by haul).
}
\details{
If tbl_cpue is NULL, the user is prompted to enter a csv file with cpue info. \cr
\cr Other notes: \cr
\itemize{
\item Area is in square nautical miles
\item CPUE in numbers is in no/(sq. nm.)
\item CPUE in weight  is in mt/(sq. nm.)
\item Abundance is in 10^6 indivs
\item Biomass   is in 10^3 mt
}
}
