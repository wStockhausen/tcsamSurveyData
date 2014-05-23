\name{calcCPUE.ByStation}
\alias{calcCPUE.ByStation}
\title{Function to calculate cpue by survey station, averaging over cpue by haul.}
\usage{
calcCPUE.ByStation(tbl_cpue = NULL, in.csv = NULL, export = FALSE,
  out.csv = "cpue.ByStation.csv", out.dir = NULL, verbosity = 1)
}
\arguments{
  \item{tbl_cpue}{: data frame w/ cpue by haul from call to
  calcCPUE.ByHaul(...)}

  \item{in.csv}{: name of csv file w/ cpue by haul (if
  tbl_cpue is not given)}

  \item{export}{: boolean flag to write results to csv
  file}

  \item{out.csv}{: output file name}

  \item{out.dir}{: output file directory}

  \item{verbosity}{: integer flag indicating level of
  printed output (0=off,1=minimal,2=full)}
}
\value{
data frame of cpue (numbers and weight) by year, stratum,
station and other factor levels
}
\description{

}
\details{
\cr Notes: \cr \itemize{ \item CPUE in numbers is in
no/(sq. nm.) \cr \item CPUE in weight is in mt/(sq. nm.)
\cr \item CPUE is calculated only for stations at which at
least 1 haul was conducted }
}

