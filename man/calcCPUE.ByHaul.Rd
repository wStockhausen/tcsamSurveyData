\name{calcCPUE.ByHaul}
\alias{calcCPUE.ByHaul}
\title{Function to calculate cpue by survey haul from station, haul and individual crab info.}
\usage{
calcCPUE.ByHaul(tbl_hauls, tbl_indivs = NULL, in.csv = NULL,
  bySex = FALSE, byShellCondition = FALSE, byMaturity = FALSE,
  bySize = FALSE, binSizes = FALSE, cutpts = seq(from = 25, to = 185, by =
  5), truncate.low = TRUE, truncate.high = FALSE, export = FALSE,
  out.csv = "cpue.ByHaul.csv", out.dir = NULL)
}
\arguments{
  \item{tbl_hauls}{:}

  \item{tbl_indivs}{:}

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
}
\value{
data frame of cpue (numbers and weight) by year, stratum,
station, haul and other factor levels
}
\description{

}
\details{
\cr \cr Note: if tbl and in.csv are both NULL, the user is
prompted to enter a csv file with biomass by stratum info.
\cr Notes: \cr CPUE in numbers is in no/(sq. nm.)  CPUE in
weight is in mt/(sq. nm.)
}

