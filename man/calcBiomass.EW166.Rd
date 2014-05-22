\name{calcBiomass.EW166}
\alias{calcBiomass.EW166}
\title{Calculates biomass/abundance for EW166 from a biomass by stratum data frame or csv file.}
\usage{
calcBiomass.EW166(tbl = NULL, in.csv = NULL,
  strata_revd = Codes.TrawlSurvey()[["strata.EW166"]], export = TRUE,
  out.csv = "SurveyBiomass.csv", out.dir = NULL)
}
\arguments{
  \item{tbl}{: data frame with biomass by stratum info from
  call to calcBiomass.ByStratum(...)}

  \item{in.csv:}{csv file with biomass by stratum info from
  call to calcBiomass.ByStratum(...)}

  \item{strata.revd}{: data frame w/ conversion from
  original strata to EW166 strata}

  \item{export}{: boolean flag to write results to csv
  file}

  \item{out.csv}{: output file name}

  \item{out.dir}{: output file directory}
}
\value{
data frame w/ abundance, biomass by year, EW166 strata,
factors
}
\description{
Calculates biomass/abundance for EW166 from a biomass by
stratum data frame or csv file.
}
\details{
\cr \cr Note: if tbl and in.csv are both NULL, the user is
prompted to enter a csv file with biomass by stratum info.
\cr Notes: \cr Abundance is in 10^6 individuals \cr Biomass
is in 10^3 mt \cr
}

