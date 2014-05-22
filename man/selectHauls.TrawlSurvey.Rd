\name{selectHauls.TrawlSurvey}
\alias{selectHauls.TrawlSurvey}
\title{Function to extract NMFS crab survey hauls data from Excel csv file format by year.}
\usage{
selectHauls.TrawlSurvey(tbl_strata, tbl = NULL, in.csv = NULL,
  export = FALSE, out.csv = "SelectedSurveyHauls.csv", out.dir = NULL,
  Years = NULL, HaulTypes = NULL, YearRange = NULL, DepthRange = NULL,
  LatRange = NULL, LonRange = NULL)
}
\value{
a dataframe w/ columns \cr
YEAR,STRATUM,GIS_STATION,HAULJOIN,HAUL_TYPE,
START_DATE,MID_LATITUDE,MID_LONGITUDE,BOTTOM_DEPTH,
GEAR_TEMPERATURE,AREA_SWEPT_VARIABLE
}
\description{

}

