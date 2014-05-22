\name{selectIndivs.TrawlSurvey}
\alias{selectIndivs.TrawlSurvey}
\title{Function to extract crab data on individuals w/ selected characteristics from NMFS trawl survey csv files.}
\usage{
selectIndivs.TrawlSurvey(tbl_hauls, tbl = NULL, col.Size = "WIDTH",
  in.csv = NULL, export = FALSE, out.csv = "SelectedIndivs.csv",
  out.dir = NULL, sex = c("MALE", "FEMALE", "ALL"),
  shell_condition = c("NEW_SHELL", "OLD_SHELL", "ALL"),
  maturity = c("IMMATURE", "MATURE", "ALL"), calcMaleMaturity = FALSE,
  minSize = -Inf, maxSize = Inf)
}
\arguments{
  \item{tbl_hauls}{: hauls table (dataframe) from call to
  selectHauls.TrawlSurvey(...) [required]}

  \item{tbl}{: table (dataframe) of survey data (from
  read.csv() called on survey csv file)}

  \item{in.csv}{: name of survey csv file to read (if tbl
  is not given)}

  \item{export}{: boolean flag to export results to csv
  file}

  \item{sex}{: one of 'MALE','FEMALE' or 'ALL' for
  narrowing selection of individuals}

  \item{shell_condition:}{one of 'NEW_SHELL','OLD_SHELL' or
  'ALL' for narrowing selection of individuals}

  \item{maturity}{: one of 'IMMATURE','MATURE' or 'ALL' for
  narrowing selection of individuals}

  \item{minWidth}{: minimum size (width) of individuals to
  select}

  \item{maxWidth}{: maximum size (width) of individuals to
  select}
}
\value{
dataframe
}
\description{

}
\details{
If neither tbl or in.csv is given, the user will be
prompted for a csv file via a file dialog box Weights are
in grams.
}

