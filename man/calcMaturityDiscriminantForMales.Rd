% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcMaturityDiscriminantForMales.R
\name{calcMaturityDiscriminantForMales}
\alias{calcMaturityDiscriminantForMales}
\title{Calculate a discriminant value on CH/CW to classify immature vs mature males}
\usage{
calcMaturityDiscriminantForMales(
  dfr,
  col = 1,
  min = 0,
  max = max(dfr[[col]], na.rm = TRUE),
  binwidth = 0.005
)
}
\arguments{
\item{dfr}{\itemize{
\item dataframe with data to analyse for  discriminant
}}

\item{col}{\itemize{
\item number or name of column with data to analyse for discriminant
}}

\item{min}{\itemize{
\item minimum value of dfr[[col]] to consider
}}

\item{max}{\itemize{
\item maximum value of dfr[[col]] to consider
}}

\item{binwidth}{\itemize{
\item binwidth for histogram
}}
}
\value{
value to use as discriminant
}
\description{
Function to calculate a discriminant value on CH/CW to classify immature vs mature males.
}
\details{
None.
}
