% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addHaulInfoToIndivData.R
\name{addHaulInfoToIndivData}
\alias{addHaulInfoToIndivData}
\title{Add haul-associated info to an indiv data datframe}
\usage{
addHaulInfoToIndivData(tbl_hauls, tbl_indivs)
}
\arguments{
\item{tbl_hauls}{: hauls table (dataframe) from call to selectHauls.TrawlSurvey(...) [required]}

\item{tbl_indivs}{: indiv data table (dataframe) from call to selectIndivs.TrawlSurvey(...) [required]}
}
\value{
dataframe with haul-associated info dropped
}
\description{
Function to add haul-associated info to an indiv data datframe.
}
\details{
Individuals from hauls not in tbl_hauls will be dropped.
}
