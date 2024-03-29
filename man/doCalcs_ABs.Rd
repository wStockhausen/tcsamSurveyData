% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/doCalcs_ABs.R
\name{doCalcs_ABs}
\alias{doCalcs_ABs}
\title{Function to calculate CPUE, abundance and biomass at various levels of spatial aggregation from station, haul and individual crab info.}
\usage{
doCalcs_ABs(
  tbl_strata,
  tbl_hauls,
  tbl_indivs,
  averageHaulsByStation = TRUE,
  useStratumArea = TRUE,
  calcByEW166 = TRUE,
  aggBySex = FALSE,
  aggByMaturity = FALSE,
  aggByShellCondition = FALSE,
  aggBySize = FALSE,
  cutpts = seq(from = 0, to = 185, by = 5),
  truncate.low = TRUE,
  truncate.high = FALSE,
  dropLevels = list(SEX = c("MISSING", "HERMAPHRODITIC")),
  verbosity = 0
)
}
\arguments{
\item{tbl_strata}{\itemize{
\item dataframe with survey strata info (output from \code{\link{selectStrata.TrawlSurvey}})
}}

\item{tbl_hauls}{\itemize{
\item dataframe with hauls info (output from \code{\link{selectHauls.TrawlSurvey}})
}}

\item{tbl_indivs}{\itemize{
\item dataframe with individual crab info (output from \code{\link{selectIndivs.TrawlSurvey}})
}}

\item{averageHaulsByStation}{\itemize{
\item TRUE (average hauls by station prior to calculating stratum-level quantities)
}}

\item{useStratumArea}{: flag (T/F) to use STRATUM_AREA to expand average CPUE to stratum abundance/biomass (default is T)}

\item{calcByEW166}{: TRUE (calculate AB by EW166 prior to calculating total for EBS)}

\item{aggBySex}{\itemize{
\item flag to agregate CPUE over sexes
}}

\item{aggByMaturity}{\itemize{
\item flag to aggregate of maturity states
}}

\item{aggByShellCondition}{\itemize{
\item flag to aggregate over shell conditions
}}

\item{aggBySize}{\itemize{
\item flag to aggregate over sizes
}}

\item{cutpts}{\itemize{
\item seq(from=0,to=185,by=5)
}}

\item{truncate.low}{\itemize{
\item TRUE
}}

\item{truncate.high}{\itemize{
\item FALSE
}}

\item{dropLevels}{\itemize{
\item NULL, or list (by factor name) of vectors of factor values to drop (see help for \code{wtsUtilities::dropLevels})
}}

\item{verbosity}{\itemize{
\item integer flag indicating level of printed output (0=off,1=minimal,2=full)
}}
}
\value{
a nested list with elements cpue and ABs. \code{cpue} has the following elements: \cr
\itemize{
\item {byHaul}     {- data frame of cpue (numbers and weight) by year, haul and other factor levels}
\item {byStratum}     {- data frame of cpue (numbers and weight) by year, station and other factor levels (or NULL)}
} \cr
\code{ABs} has the following elements: \cr
\itemize{
\item {byStratum} {- data frame with abundance, biomass by year, stratum and other factor levels}
\item {EW166}     {- data frame with abundance, biomass by year, EW166 split and other factor levels (optional)}
\item {EBS}       {- data frame with abundance, biomass by year and other factor levels for the EBS}
}
}
\description{
Function to calculate CPUE, abundance and biomass from station, haul and individual crab info.
}
\details{
Notes: \itemize{
\item CPUE in numbers is in no/(sq. nm.)
\item CPUE in weight  is in mt/(sq. nm.)
\item Abundance is in 10^6 individuals
\item Biomass   is in 10^3 mt
}
}
