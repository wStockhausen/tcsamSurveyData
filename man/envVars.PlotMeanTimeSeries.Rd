% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/envVars.PlotMeanTimeSeries.R
\name{envVars.PlotMeanTimeSeries}
\alias{envVars.PlotMeanTimeSeries}
\title{Plot time series of mean environmental variable}
\usage{
envVars.PlotMeanTimeSeries(
  dfr,
  yearCol = "YEAR",
  envVarCol = "BOTTOM_TEMP",
  xlab = "year",
  ylab = "Bottom Temperature (degC)",
  showPlot = FALSE,
  verbose = FALSE
)
}
\arguments{
\item{dfr}{- dataframe from call to \code{\link{calcEnvData.ByStation}}}

\item{yearCol}{- dataframe column name for year}

\item{envVarCol}{- dataframe column name for the environmental variable}

\item{xlab}{- x axis label}

\item{ylab}{- y axis label}

\item{showPlot}{- flag (T/F) to show plot}

\item{verbose}{- flag (T/F) to print debugging info}
}
\value{
ggplot2 object
}
\description{
Function to plot time series of mean environmental variable
}
\details{
uses \code{\link{wtsUtilities::plotMDFR.XY}} and the \code{reshape2} package.
}
