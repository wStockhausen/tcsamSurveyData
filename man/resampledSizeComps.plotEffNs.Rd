% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resampledSizeComps.plotEffNs.R
\name{resampledSizeComps.plotEffNs}
\alias{resampledSizeComps.plotEffNs}
\title{Plot effective Ns from resampled size compositions with original Ns}
\usage{
resampledSizeComps.plotEffNs(dfrEffNs, dfrOrgNs, byFacs = "")
}
\arguments{
\item{dfrEffNs}{: dataframe with effective Ns from resampled size compositions (output from \link{resampledSizeComps.calcEffN})}

\item{dfrOrgNs}{: dataframe with original Ns, for comparison}

\item{byFacs}{: vector of column names for factors other than YEAR and STRATUM}
}
\value{
a list with two ggplot2 plot objects comparing time series of the default sample sizes (default N's),
the mean effective N's, the harmonic mean effective N's, the number of crab sampled, and
the number of non-zero stations., The first plot is with, and the second without,
the actual number of crabs measured.
}
\description{
Function to plot effective Ns from resampled size compositions, together
with the original Ns that would have been used as input sample sizes.
}
\details{
dfrEffNs should be a dataframe output from \link{resampledSizeComps.calcEffN}.
dfrOrgNs should be a dataframe with columns
\itemize{
\item{stratum}
\item{year}
\item{sex}
\item{maturity}
\item{shell_condition}
\item{relSS - relative sample size}
}
}
