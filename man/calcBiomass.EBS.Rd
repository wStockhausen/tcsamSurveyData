\name{calcBiomass.EBS}
\alias{calcBiomass.EBS}
\title{Function to calculate total biomass from a biomass-by-stratum data frame or csv file.}
\usage{
calcBiomass.EBS(tbl = NULL, in.csv = NULL, export = TRUE,
  out.csv = "SurveyBiomass.csv", out.dir = NULL)
}
\arguments{
  \item{tbl}{: data frame with biomass by stratum info from
  call to calcBiomass.ByStratum(...) or
  calcBiomass.EW166(...)}

  \item{in.csv}{: csv file with biomass by stratum info
  from call to calcBiomass.ByStratum(...) or
  calcBiomass.EW166(...)}

  \item{export}{: boolean flag to write results to csv
  file}

  \item{out.csv}{: output file name}

  \item{out.dir}{: output file directory}
}
\value{
data frame w/ abundance, biomass by year
}
\description{

}
\details{
\cr Note: if tbl and in.csv are both NULL, the user is
prompted to enter a csv file with biomass by stratum info
\cr Notes: \cr Abundance is in 10^6 individuals \cr Biomass
is in 10^3 mt \cr
}

