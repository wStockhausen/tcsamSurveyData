\name{doBiomassCalcs}
\alias{doBiomassCalcs}
\title{Function to calculate cpue by survey haul from station, haul and individual crab info.}
\usage{
doBiomassCalcs(tbl.strata, tbl.hauls, tbl.indivs, calcCPUE.byStation = TRUE,
  calcBiomass.byEW166 = TRUE, bySex = FALSE, byShellCondition = FALSE,
  byMaturity = FALSE, bySize = FALSE, binSizes = FALSE,
  cutpts = seq(from = 0, to = 185, by = 5), truncate.low = TRUE,
  truncate.high = FALSE, export.cpue.byH = FALSE, export.cpue.byS = FALSE,
  export.bio.byStrata = FALSE, export.bio.EW166 = FALSE,
  export.totBio = FALSE, out.csv.cpue.byH = "cpue.byH.csv",
  out.csv.cpue.byS = "cpue.byS.csv",
  out.csv.bio.byStrata = "bio.byStrata.csv",
  out.csv.bio.EW166 = "bio.EW166.csv", out.csv.totBio = "totBiomass.csv",
  out.dir = NULL, verbosity = 1)
}
\arguments{
  \item{tbl.hauls}{: dataframe with hauls info (output from
  \code{\link{selectHauls.TrawlSurvey}})}

  \item{tbl.indivs}{: dataframe with individual crab info
  (output from \code{\link{selectIndivs.TrawlSurvey}})}

  \item{tbl.strata}{: dataframe with survey strata info
  (output from \code{\link{selectStrata.TrawlSurvey}})}

  \item{calcCPUE.byStation}{= TRUE (otherwise calc cpue
  using calcCPUE.ByHaul)}

  \item{calcBiomass.byEW166}{= TRUE}

  \item{bySex}{= FALSE}

  \item{byShellCondition}{= FALSE}

  \item{byMaturity}{= FALSE}

  \item{bySize}{= FALSE}

  \item{binSizes}{= FALSE}

  \item{cutpts}{= seq(from=0,to=185,by=5)}

  \item{truncate.low}{= TRUE}

  \item{truncate.high}{= FALSE}

  \item{export.cpue.byH}{= FALSE}

  \item{export.cpue.byS}{= FALSE}

  \item{export.bio.byStrata}{= FALSE}

  \item{export.bio.EW166}{= FALSE}

  \item{export.totBio}{= FALSE}

  \item{out.csv.cpue.byH}{= 'cpue.byH.csv'}

  \item{out.csv.cpue.byS}{= 'cpue.byS.csv'}

  \item{out.csv.bio.byStrata}{= 'bio.byStrata.csv'}

  \item{out.csv.bio.EW166}{= 'bio.EW166.csv'}

  \item{out.csv.totBio}{= 'totBiomass.csv'}

  \item{out.dir}{= output directory}

  \item{verbosity}{: integer flag indicating level of
  printed output (0=off,1=minimal,2=full)}
}
\value{
a list with the following elements: \cr \itemize{ \item
cpue.byH \tab : data frame of cpue (numbers and weight) by
year, haul and other factor levels \item cpue.byS \tab :
data frame of cpue (numbers and weight) by year, station
and other factor levels (optional) \item bio.byStrata \tab
: data frame with abundance, biomass by year, stratum and
other factor levels \item bio.EW166 \tab : data frame with
abundance, biomass by year, EW166 split and other factor
levels (optional) \item bio.tot \tab : data frame with
abundance, biomass by year and other factor levels }
}
\description{

}
\details{
Notes: \itemize{ \item CPUE in numbers is in no/(sq. nm.)
\item CPUE in weight is in mt/(sq. nm.)  \item Abundance is
in 10^6 individuals \item Biomass is in 10^3 mt }
}

