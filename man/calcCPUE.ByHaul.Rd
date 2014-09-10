\name{calcCPUE.ByHaul}
\alias{calcCPUE.ByHaul}
\title{Calculate cpue by survey haul and other factors from station, haul and individual crab info.}
\usage{
calcCPUE.ByHaul(tbl_hauls, tbl_indivs = NULL, bySex = FALSE,
  byShellCondition = FALSE, byMaturity = FALSE, bySize = FALSE,
  cutpts = seq(from = 25, to = 185, by = 5), truncate.low = TRUE,
  truncate.high = FALSE, export = FALSE, out.csv = "cpue.ByHaul.csv",
  out.dir = NULL, verbosity = 1)
}
\arguments{
  \item{tbl_hauls}{: dataframe from call to
  \code{\link{selectHauls.TrawlSurvey}} [required]}

  \item{tbl_indivs}{: dataframe from call to
  \code{\link{selectIndivs.TrawlSurvey}} (or crab survey
  filename, or NULL)}

  \item{bySex}{: flag (T/F) to calc by sex}

  \item{byShellCondition}{: flag (T/F) to calc by shell
  condition}

  \item{byMaturity}{: flag (T/F) to calc by maturity state}

  \item{bySize}{: flag (T/F) to calc by size}

  \item{cutpts}{: vector of cutpoints to create size bins
  from}

  \item{truncate.low}{: flag (T/F) to exclude individuals
  smaller than minSize}

  \item{truncate.high}{: flag (T/F) to exclude individuals
  larger than maxSize}

  \item{export}{: boolean flag to write results to csv
  file}

  \item{out.csv}{: output file name}

  \item{out.dir}{: output file directory}

  \item{verbosity}{: integer flag indicating level of
  printed output (0=off,1=minimal,2=full)}
}
\value{
A dataframe of cpue (numbers and weight) by haul. See
Details.
}
\description{
Function to calculate cpue by survey haul and other factors
(e.g., sex) from station, haul and individual crab info.
}
\details{


Note: tbl_hauls is required. if tbl_indivs is a dataframe,
it is used. If it is a character vector, it is interpreted
as a file name and it is read as a csv file. If it is NULL,
the user is prompted to choose a file to read. \cr The
returned dataframe has the following columns: \cr \itemize{
\item YEAR \item STRATUM \item GIS_STATION \item HAULJOIN
\item LONGITUDE \item LATITUDE \item requested factors, if
any \item numIndivs \item numCPUE \item wgtCPUE } \cr Other
notes: \cr \itemize{ \item CPUE in numbers is in no/(sq.
nm.) \item CPUE in weight is in mt/(sq. nm.) } \cr
}

