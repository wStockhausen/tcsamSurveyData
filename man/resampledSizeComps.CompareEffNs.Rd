% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resampledSizeComps.CompareEffNs.R
\name{resampledSizeComps.CompareEffNs}
\alias{resampledSizeComps.CompareEffNs}
\title{Compare effective N's from several resampled size compositions "scenarios"}
\usage{
resampledSizeComps.CompareEffNs(
  dfrEffNs,
  byFacs = "",
  nDefault = 200,
  nStations = 375
)
}
\arguments{
\item{dfrEffNs}{\itemize{
\item a dataframe with "stacked" scenarios of effective Ns from \link{resampledSizeComps.calcEffN}
}}

\item{byFacs}{\itemize{
\item vector of column names for factors other than YEAR and STRATUM
}}

\item{nDefault}{\itemize{
\item default sample size (for plotting)
}}

\item{nStations}{\itemize{
\item typical number of survey stations (for plotting)
}}
}
\value{
2-element list of plots (with and without number of crab sampled)
}
\description{
Function to compare effective N's from several resampled size compositions "scenarios".
}
\details{
dfrEffNs should "stacked" dataframe of outputs from \link{resampledSizeComps.calc},
with values of an additional column, \code{type}, that indicates the scenario each row
is associated with.
}
