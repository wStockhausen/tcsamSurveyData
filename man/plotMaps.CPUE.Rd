\name{plotMaps.CPUE}
\alias{plotMaps.CPUE}
\title{Plot spatial maps of cpue by year, survey haul or station and other factors from cpue info.}
\usage{
plotMaps.CPUE(tbl_cpue, years = 2012, ztype = "numCPUE", scaleBy = 1,
  title = "Survey", zlab = "no. crab", zunits = "num/nm@+2@+",
  plotBySex = FALSE, plotByShellCondition = FALSE, plotByMaturity = FALSE,
  zscl = NULL, xyrng = "180/205/54/62", rotate = 170, elev = 70,
  delx = 0.5, dely = 0.25, blocktype = c("MEAN", "SUM"),
  plt_blocktype = c("SMOOTH", "COARSE"), logtr = FALSE, plt_title = FALSE,
  plt_bars = TRUE, plt_surface = FALSE, plt_blocklocations = plt_surface,
  plt_colorscale = plt_surface | plt_bars, plt_stations = TRUE,
  plt_reflines = TRUE, reflines = list(list(lon = -166 + 0 * seq(from = 50,
  to = 80, by = 1), lat = seq(from = 50, to = 80, by = 1))),
  bathymetryFile = "/Users/WilliamStockhausen/Programming/R/GitPackages/wtsGMT/data/depthcontour_200500.prn",
  cleanup = FALSE, verbosity = 1)
}
\arguments{
  \item{tbl_cpue}{: dataframe from call to
  \code{\link{calcCPUE.ByHaul} or
  \code{\link{calcCPUE.ByStation}}} (or crab cpue filename,
  or NULL)}

  \item{years}{: vector of survey years for which to
  produce maps}

  \item{ztype}{: the data type (numCPUE, wgtCPUE to map)}

  \item{scaleBy}{: factor to scale the ztype by BEFORE
  sending to plotMap.CSV}

  \item{zlab}{: label for z (cpue) axes on map}

  \item{zunits}{: label for z (cpue) units on maps}

  \item{zscl}{: max z (after applying scaleBy) for z axis
  (automatically scaled to max if zscl is NULL)}

  \item{plotBySex}{: flag (T/F) to plot by individual sexes
  or vector of sexes to plot}

  \item{plotByShellCondition}{: flag (T/F) to plot by
  indiviudvshell condition or vector of sc's to plot}

  \item{plotByMaturity}{: flag (T/F) to plot by maturity
  state or vector of ms's to plot}

  \item{psFile}{: base name for output files}

  \item{verbosity}{: integer flag indicating level of
  printed output (0=off,1=minimal,2=full)}
}
\value{
vector or list of zscales used for plots
}
\description{
Function to map cpue by survey haul or station and other
factors (e.g., sex) from cpue info.
}
\details{


Needs to filled in.
}

