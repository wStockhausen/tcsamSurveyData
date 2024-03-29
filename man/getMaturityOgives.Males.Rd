% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getMaturityOgives.Males.R
\name{getMaturityOgives.Males}
\alias{getMaturityOgives.Males}
\title{Function to extract male maturity ogives from a dataframe}
\usage{
getMaturityOgives.Males(
  fn = NULL,
  tbl_strata = NULL,
  cutpts = seq(from = 25, to = 185, by = 5),
  byEBS = TRUE,
  showPlot = TRUE,
  export = FALSE,
  out.csv = "MaturityOgives.Males.csv",
  out.dir = NULL,
  verbosity = 0,
  ...
)
}
\arguments{
\item{fn}{: filename for maturity-classified chela height dataset}

\item{tbl_strata}{: data frame from call to \code{\link{selectStrata.TrawlSurvey}} [required]}

\item{cutpts}{: vector of cutpoints to create size bins from}

\item{byEBS}{: flag to aggregate across strata to EBS}

\item{showPlot}{: flag to show plot of ogives}

\item{export}{: boolean flag to export results to csv file}

\item{out.csv}{: name of output csv file                    (ignored if NULL)}

\item{out.dir}{: base path for output csv file              (set to folder of input csv file or current working directory)}

\item{verbosity}{: flags for intermediate output}

\item{...}{: parameters passed to plotMaturityOgives(...)}
}
\value{
dataframe with columns: \cr
\itemize{
\item  YEAR
\item  STRATUM
\item  SEX
\item  SIZE
\item  numIndivs
\item  ogive
}
}
\description{
Function to extract female maturity ogives from a size composition dataframe.
}
\details{
None.\cr
}
