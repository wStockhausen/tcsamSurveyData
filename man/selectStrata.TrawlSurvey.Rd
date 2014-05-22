\name{selectStrata.TrawlSurvey}
\alias{selectStrata.TrawlSurvey}
\title{Function to extract crab station data and strata information from a NMFS trawl survey strata file.}
\usage{
selectStrata.TrawlSurvey(tbl_hauls, tbl = NULL, in.csv = NULL,
  species = "BTC", useOrigStrata = FALSE, export = FALSE,
  out.csv = paste("SelectedStations", species, "csv", sep = "."),
  out.dir = NULL)

selectStrata.TrawlSurvey(tbl_hauls, tbl = NULL, in.csv = NULL,
  species = "BTC", useOrigStrata = FALSE, export = FALSE,
  out.csv = paste("SelectedStations", species, "csv", sep = "."),
  out.dir = NULL)
}
\arguments{
  \item{tbl_hauls}{: hauls table (dataframe) from call to
  selectHauls.TrawlSurvey(...) [required]}

  \item{tbl}{: table (dataframe) of survey station/strata
  data (from read.csv() called on survey station/strata csv
  file)}

  \item{in.csv}{: name of survey station/strata csv file to
  read (if tbl is not given)}

  \item{species}{: code ('BKC','BTC','RKC','OTC')
  indicating species}

  \item{useOrigStrata:}{boolean to use Bob Foy's "original"
  strata, rather than the revised strata}

  \item{export}{: boolean flag to export results to csv
  file}

  \item{tbl_hauls}{: hauls table (dataframe) from call to
  selectHauls.TrawlSurvey(...) [required]}

  \item{tbl}{: table (dataframe) of survey station/strata
  data (from read.csv() called on survey station/strata csv
  file)}

  \item{in.csv}{: name of survey station/strata csv file to
  read (if tbl is not given)}

  \item{species}{: code ('BKC','BTC','RKC','OTC')
  indicating species}

  \item{useOrigStrata:}{boolean to use Bob Foy's "original"
  strata, rather than the revised strata}

  \item{export}{: boolean flag to export results to csv
  file}
}
\value{
a dataframe with strata

a dataframe with strata
}
\description{

}
\details{
note: if neither tbl or in.csv is given, the user will be
prompted for a csv file via a file dialog box

note: if neither tbl or in.csv is given, the user will be
prompted for a csv file via a file dialog box
}

