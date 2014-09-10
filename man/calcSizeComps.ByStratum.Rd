\name{calcSizeComps.ByStratum}
\alias{calcSizeComps.ByStratum}
\title{Calculate size compositions by stratum from AFSC trawl survey data.}
\usage{
calcSizeComps.ByStratum(tbl_strata, tbl_cpue = NULL,
  avgHaulsByStation = FALSE, bySex = FALSE, byShellCondition = FALSE,
  byMaturity = FALSE, cutpts = seq(from = 25, to = 185, by = 5),
  truncate.low = TRUE, truncate.high = FALSE, export = FALSE,
  out.csv = "SizeCompsByStratum.csv", out.dir = NULL, tbl_hauls = NULL,
  Years = NULL, HaulTypes = NULL, YearRange = NULL, DepthRange = NULL,
  LatRange = NULL, LonRange = NULL, tbl_indivs = NULL,
  col.Size = "WIDTH", sex = c("MALE", "FEMALE", "ALL"),
  shell_condition = c("NEW_SHELL", "OLD_SHELL", "ALL"),
  maturity = c("IMMATURE", "MATURE", "ALL"), calcMaleMaturity = FALSE,
  minSize = -Inf, maxSize = Inf, verbosity = 1)
}
\arguments{
  \item{tbl_strata}{: data frame from call to
  \code{\link{selectStrata.TrawlSurvey}}}

  \item{tbl_cpue}{: data frame from call to
  \code{\link{calcCPUE.ByHaul}} or
  \code{\link{calcCPUE.ByStation}}, or filename for csv
  file, or NULL}

  \item{avgHaulsByStation}{: flag (T/F) to average hauls by
  station before calc'ing size comps}

  \item{bySex}{: flag (T/F) to calc by sex}

  \item{byShellCondition}{: flag (T/F) to calc by shell
  condition}

  \item{byMaturity}{: flag (T/F) to calc by maturity state}

  \item{cutpts}{: vector of cutpoints to create size bins
  from}

  \item{truncate.low}{: flag (T/F) to exclude individuals
  smaller than minSize}

  \item{truncate.high}{: flag (T/F) to exclude individuals
  larger than maxSize}

  \item{tbl_hauls}{- dataframe from call to
  \code{\link{selectHauls.TrawlSurvey}} [required]}

  \item{Years}{- vector of survey years to include in hauls
  (ignored if NULL)}

  \item{HaulTypes}{- vector of haul types to include in
  hauls (ignored if NULL)}

  \item{YearRange}{- vector of min, max survey years to
  include in hauls (ignored if NULL)}

  \item{DepthRange}{- vector of min, max haul depths to
  include in hauls (ignored if NULL)}

  \item{LatRange}{- vector of min, max haul latitudes to
  include in hauls (ignored if NULL)}

  \item{LonRange}{- vector of min, max haul longitudes to
  include in hauls (ignored if NULL)}

  \item{tbl_indivs}{: dataframe from call to
  \code{\link{selectIndivs.TrawlSurvey}} (or crab survey
  filename, or NULL)}

  \item{col.Size}{: name of tbl_indivs column containing
  size (CL or CW) information}

  \item{sex}{: one of 'MALE','FEMALE' or 'ALL' for
  narrowing selection of individuals}

  \item{shell_condition}{: one of 'NEW_SHELL','OLD_SHELL'
  or 'ALL' for narrowing selection of individuals}

  \item{maturity}{: one of 'IMMATURE','MATURE' or 'ALL' for
  narrowing selection of individuals}

  \item{calcMaleMaturity:}{flag (T/F) to calculate
  pr(mature|size) for males based on an ogive}

  \item{minSize}{: minimum size (width) of individuals to
  select}

  \item{maxSize}{: maximum size (width) of individuals to
  select}

  \item{export}{: boolean flag to write results to csv
  file}

  \item{out.csv}{: output file name}

  \item{out.dir}{: output file directory}

  \item{verbosity}{: integer flag indicating level of
  printed output (0=off,1=minimal,2=full)}
}
\value{
data frame with size comps by stratum. Columns are \cr
\itemize{ \item YEAR \item STRATUM \item STRATUM_AREA \item
other user-defined factors \item numStations \item numHauls
\item numIndivs \item totABUNDANCE = estimated abundance
(by stratum) \item stdABUNDANCE = std deviation of
estimated abundance (by stratum) \item cvABUNDANCE = cv of
estimated abundance (by stratum) \item totBIOMASS =
estimated biomass (by stratum) \item stdBIOMASS = std
deviation of estimated biomass (by stratum) \item cvBIOMASS
= cv of estimated biomass (by stratum) }
}
\description{
This function calculates size compositions by stratum from
cpue (by survey station or by haul).
}
\details{


If tbl_cpue is not a dataframe, it can be a cpue csv file
or NULL. If it is NULL, then tbl_hauls (tbl_indivs) must
either be: 1) a dataframe of hauls (indivs); 2) a filename
for a hauls (indivs), or 3) NULL (in which case the user
may select the csv file using a dialog box). \cr \cr Other
notes: \cr \itemize{ \item{Area is in square nautical
miles} \item{Abundance is in 10^6 indivs} \item{Biomass is
in 10^3 mt} }
}
