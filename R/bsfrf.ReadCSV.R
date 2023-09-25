#'
#' @title Read "raw" BSFRF csv file
#'
#' @description Function to read "raw" BSFRF csv file.
#'
#' @param tbl : filename for "raw" BSFRF csv file to read
#' @param cts : input column definitions in format for [readr::read_csv()]
#'
#' @return dataframe
#'
#' @details Output dataframe will have lower case column names. If \code{cts} is given,
#' it should be in the format given by [readr::cols()]. Default (\code{cts=NULL})
#' input csv file column names/types are:
#' \itemize{
#'   \item{ScottSort = col_double()}
#'   \item{Study = col_character()}
#'   \item{Boat = col_character()}
#'   \item{Tow = col_character()}
#'   \item{NMFS_Stn = col_character()}
#'   \item{Year = col_double()}
#'   \item{Date = col_character()}
#'   \item{Time = col_time(format = "")}
#'   \item{Depth_ftm = col_double()}
#'   \item{Temp_C = col_double()}
#'   \item{Aswept_nm2 = col_double()}
#'   \item{MidTowLatitude = col_double()}
#'   \item{MidTowLongitude = col_double()}
#'   \item{Basket = col_character()}
#'   \item{Page = col_double()}
#'   \item{LineNum = col_double()}
#'   \item{Species = col_character()}
#'   \item{Sex = col_character()}
#'   \item{shell = col_double()}
#'   \item{Carapace = col_double()}
#'   \item{Fixed_CW = col_double()}
#'   \item{Sub = col_character()}
#'   \item{SampFactor = col_double()}
#'   \item{col = col_character()}
#'   \item{con = col_character()}
#'   \item{full = col_character()}
#'   \item{CPUEnum = col_double()}
#'   \item{`5mmSizes` = col_character()}
#' }
#'
#' @import readr
#'
#' @export
#'
bsfrf.ReadCSV<-function(fn,cts=NULL){
  if (is.null(cts)){
    #--use default column names/types
    cts = readr::cols(
                ScottSort = readr::col_double(),
                Study = readr::col_character(),
                Boat = readr::col_character(),
                Tow = readr::col_character(),
                NMFS_Stn = readr::col_character(),
                Year = readr::col_double(),
                Date = readr::col_character(),
                Time = readr::col_time(format = ""),
                Depth_ftm = readr::col_double(),
                Temp_C = readr::col_double(),
                Aswept_nm2 = readr::col_double(),
                MidTowLatitude = readr::col_double(),
                MidTowLongitude = readr::col_double(),
                Basket = readr::col_character(),
                Page = readr::col_double(),
                LineNum = readr::col_double(),
                Species = readr::col_character(),
                Sex = readr::col_character(),
                shell = readr::col_double(),
                Carapace = readr::col_double(),
                Fixed_CW = readr::col_double(),
                Sub = readr::col_character(),
                SampFactor = readr::col_double(),
                col = readr::col_character(),
                con = readr::col_character(),
                full = readr::col_character(),
                CPUEnum = readr::col_double(),
                `5mmSizes` = readr::col_character()
              );
  }
  dfr = readr::read_csv(fn,
                        col_types=cts);
  names(dfr) = tolower(names(dfr));
  return(dfr);
}
