\name{calcCPUE.ByHaul}
\alias{calcCPUE.ByHaul}
\title{Calculate cpue by survey haul and other factors from station, haul and individual crab info.}
\usage{
calcCPUE.ByHaul(tbl_hauls, tbl_indivs = NULL, in.csv = NULL,
  bySex = FALSE, byShellCondition = FALSE, byMaturity = FALSE,
  bySize = FALSE, binSizes = FALSE, cutpts = seq(from = 25, to = 185, by =
  5), truncate.low = TRUE, truncate.high = FALSE, export = FALSE,
  out.csv = "cpue.ByHaul.csv", out.dir = NULL, verbosity = 1)
}
\arguments{
  \item{tbl_hauls}{: dataframe from call to
  \code{\link{selectHauls.TrawlSurvey}}}

  \item{tbl_indivs}{: dataframe from call to
  \code{\link{selectIndivs.TrawlSurvey}}}

  \item{in.csv}{: name of csv file w/ individual crab info
  to read (if tbl_indivs is not given)}

  \item{bySex=FALSE,}{}

  \item{byShellCondition=FALSE,}{}

  \item{byMaturity=FALSE,}{}

  \item{bySize=FALSE,}{}

  \item{binSizes=FALSE,}{}

  \item{cutpts=seq(from=25,to=185,by=5),}{}

  \item{truncate.low=TRUE,}{}

  \item{truncate.high=FALSE,}{}

  \item{export}{: boolean flag to write results to csv
  file}

  \item{out.csv}{: output file name}

  \item{out.dir}{: output file directory}

  \item{verbosity}{: integer flag indicating level of
  printed output (0=off,1=minimal,2=full)}
}
\value{
data frame of cpue (numbers and weight) by year, stratum,
station, haul and other factor levels
}
\description{
Function to calculate cpue by survey haul and other factors
(e.g., sex) from station, haul and individual crab info.
}
\details{


\cr \cr Note: if tbl and in.csv are both NULL, the user is
prompted to enter a csv file with biomass by stratum info.
\cr \cr Other notes: \cr \itemize{ \item CPUE in numbers is
in no/(sq. nm.) \item CPUE in weight is in mt/(sq. nm.) }
}

