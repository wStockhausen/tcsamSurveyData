\name{selectHauls.TrawlSurvey}
\alias{selectHauls.TrawlSurvey}
\title{Function to extract NMFS crab survey hauls data from Excel csv file format by year.}
\usage{
selectHauls.TrawlSurvey(tbl_strata, tbl = NULL, in.csv = NULL,
  export = FALSE, out.csv = "SelectedSurveyHauls.csv", out.dir = NULL,
  Years = NULL, HaulTypes = NULL, YearRange = NULL, DepthRange = NULL,
  LatRange = NULL, LonRange = NULL, verbosity = 1)
}
\arguments{
  \item{tbl_strata}{- data frame from call to
  selectStrata.TrawlSurvey(...) [required]}

  \item{tbl}{- trawl survey data table from previous call
  (ignored if NULL)}

  \item{in.csv}{- trawl survey data as csv file (ignored if
  NULL)}

  \item{export}{- boolean flag to export results to csv
  file}

  \item{out.csv}{- name of output csv file (ignored if
  NULL)}

  \item{out.dir}{- base path for output csv file (ignored
  if NULL)}

  \item{Years}{- vector of survey years to include in
  output (ignored if NULL)}

  \item{HaulTypes}{- vector of haul types to include in
  output (ignored if NULL)}

  \item{YearRange}{- vector of min, max survey years to
  include in output (ignored if NULL)}

  \item{DepthRange}{- vector of min, max haul depths to
  include in output (ignored if NULL)}

  \item{LatRange}{- vector of min, max haul latitudes to
  include in output (ignored if NULL)}

  \item{LonRange}{- vector of min, max haul longitudes to
  include in output (ignored if NULL)}

  \item{verbosity}{: integer flag indicating level of
  printed output (0=off,1=minimal,2=full)}
}
\value{
a dataframe w/ columns \cr
YEAR,STRATUM,GIS_STATION,HAULJOIN,HAUL_TYPE, \cr
START_DATE,MID_LATITUDE,MID_LONGITUDE,BOTTOM_DEPTH, \cr
GEAR_TEMPERATURE,AREA_SWEPT_VARIABLE
}
\description{

}
\details{
If neither tbl or in.csv is given, the user will be
prompted for a csv file via a file dialog box
}

