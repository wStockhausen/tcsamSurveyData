\name{calcBiomass.ByStratum}
\alias{calcBiomass.ByStratum}
\title{Function to calculate cpue by survey station from trawl survey files.}
\usage{
calcBiomass.ByStratum(tbl_strata, tbl_cpue = NULL, in.csv = NULL,
  export = FALSE, out.csv = "BiomassByStratum.csv", out.dir = NULL)
}
\arguments{
  \item{tbl_strata}{: data frame w/ stations/strata from
  call to selectStrata.TrawlSurvey(...)}

  \item{tbl_cpue}{: data frame w/ cpue by year, station,
  other factor levels}

  \item{in.csv}{: csv file w/ cpue by year, station, other
  factor levels}

  \item{export}{: boolean flag to write results to csv
  file}

  \item{out.csv}{: output file name}

  \item{out.dir}{: output file directory}
}
\value{
data frame with average cpue (numbers, weight), abundance
and biomass by stratum.
}
\description{
Note: if tbl_cpue and in.csv are both NULL, the user is
prompted to enter a csv file with cpue info. \cr Other
notes: \cr CPUE in numbers is in no/(sq. nm.) \cr CPUE in
weight is in mt/(sq. nm.) \cr Abundance is in 10^6 indivs
\cr Biomass is in 10^3 mt \cr
}
\details{

}

