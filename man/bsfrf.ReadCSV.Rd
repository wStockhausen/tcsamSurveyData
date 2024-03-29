% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bsfrf.ReadCSV.R
\name{bsfrf.ReadCSV}
\alias{bsfrf.ReadCSV}
\title{Read "raw" BSFRF csv file}
\usage{
bsfrf.ReadCSV(fn, cts = NULL)
}
\arguments{
\item{cts}{: input column definitions in format for \code{\link[readr:read_delim]{readr::read_csv()}}}

\item{tbl}{: filename for "raw" BSFRF csv file to read}
}
\value{
dataframe
}
\description{
Function to read "raw" BSFRF csv file.
}
\details{
Output dataframe will have lower case column names. If \code{cts} is given,
it should be in the format given by \code{\link[readr:cols]{readr::cols()}}. Default (\code{cts=NULL})
input csv file column names/types are:
\itemize{
\item{ScottSort = col_double()}
\item{Study = col_character()}
\item{Boat = col_character()}
\item{Tow = col_character()}
\item{NMFS_Stn = col_character()}
\item{Year = col_double()}
\item{Date = col_character()}
\item{Time = col_character()}
\item{Depth_ftm = col_double()}
\item{Temp_C = col_double()}
\item{Aswept_nm2 = col_double()}
\item{MidTowLatitude = col_double()}
\item{MidTowLongitude = col_double()}
\item{Basket = col_character()}
\item{Page = col_double()}
\item{LineNum = col_double()}
\item{Species = col_character()}
\item{Sex = col_character()}
\item{Shell = col_double()}
\item{Carapace = col_double(): possible size column}
\item{Fixed_CL = col_double(): possible size column}
\item{Size = col_double(): possible size column}
\item{Fixed_Size = col_double(): possible size column}
\item{Sub = col_character()}
\item{SampFactor = col_double()}
\item{col = col_character()}
\item{con = col_character()}
\item{full = col_character()}
\item{CPUEnum = col_double()}
\item{\verb{5mmSizes} = col_character()}
}
}
