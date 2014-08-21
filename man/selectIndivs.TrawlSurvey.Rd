\name{selectIndivs.TrawlSurvey}
\alias{selectIndivs.TrawlSurvey}
\title{Function to extract crab data on individuals w/ selected characteristics from NMFS trawl survey csv files.}
\usage{
selectIndivs.TrawlSurvey(tbl_hauls, tbl = NULL, col.Size = "WIDTH",
  export = FALSE, out.csv = "SelectedIndivs.csv", out.dir = NULL,
  sex = c("MALE", "FEMALE", "ALL"), shell_condition = c("NEW_SHELL",
  "OLD_SHELL", "ALL"), maturity = c("IMMATURE", "MATURE", "ALL"),
  calcMaleMaturity = FALSE, minSize = -Inf, maxSize = Inf,
  verbosity = 1)
}
\arguments{
  \item{tbl_hauls}{: hauls table (dataframe) from call to
  selectHauls.TrawlSurvey(...) [required]}

  \item{tbl}{: table (dataframe) of survey data (or csv
  filename or NULL)}

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

  \item{verbosity}{: integer flag indicating level of
  printed output (0=off,1=minimal,2=full)}
}
\value{
dataframe (see Details for coulmn names)
}
\description{

}
\details{
If neither tbl or in.csv is given, the user will be
prompted for a csv file via a file dialog box.\cr Returned
dataframe will have columns: \itemize{\item {HAULJOIN}
\item {numIndivs} \item {SEX} \item {SHELL_CONDITION} \item
{MATURITY} \item {SEX_CODE} \item {SHELL_CONDITION_CODE}
\item {SIZE} \item {EGG_COLOR} \item {EGG_CONDITION} \item
{CLUTCH_SIZE} \item {CHERLA_HEIGHT} \item {SAMPLING_FACTOR}
\item {WEIGHT} \item {CALCULATED_WEIGHT} } \cr Notes:
\itemize{\item Weights are in grams.}
}

