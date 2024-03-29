% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resampledSizeComps.PlotStats.R
\name{resampledSizeComps.PlotStats}
\alias{resampledSizeComps.PlotStats}
\title{Create plots for resampled size comps statistics by year}
\usage{
resampledSizeComps.PlotStats(
  dfrStats,
  plotNorm = TRUE,
  nrow = 5,
  ncol = 5,
  showCIs = TRUE,
  showPlots = FALSE,
  savePlots = TRUE,
  fn = "fig_Stats",
  ext = "png",
  width = 6,
  height = 6,
  dpi = 200
)
}
\arguments{
\item{dfrStats}{: dataframe with "stacked" results from \link{resampledSizeComps.CalcStats}}

\item{plotNorm}{: flgg (T/F) to plot normalized (vs. un-normalized) size comps}

\item{nrow}{: number of rows per plot page}

\item{ncol}{: number of columns per plot page}

\item{showCIs}{: flag (T/F) to plot confidence intervals (as ribbons)}

\item{showPlots}{: flag (T/F) to}

\item{savePlots}{: flag (T/F) to}

\item{fn}{: base filename (with path to output folder; sex, maturity, shell condition and page number will be appended)}

\item{ext}{: extension for output plot files}

\item{width}{: page width (in inches)}

\item{height}{: page height (in inches)}

\item{dpi}{: dots-per-inch}
}
\value{
list of plots
}
\description{
Function to create plots for resampled size comps statistics by year.
}
\details{
This function makes plots for a single "scenario" of resampled size comps, showing
mean and median, in addition to original and (optionally) CIs. Use
\link{resampledSizeComps.CompareStats} to compare results from multiple scenarios.
}
