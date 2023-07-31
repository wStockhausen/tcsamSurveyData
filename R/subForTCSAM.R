#'
#' @title Check and make substitutions for "undetermined", "unknown", or "all"
#'
#' @description Function to check and make substitutions for "undetermined", "unknown", or "all".
#'
#' @param x - character string to check if equal to "undetermined", "unknown", or "all"
#' @param str - character string to substitute
#'
#' @return character string with potential substitutions made
#'
#' @details If \code{x} is"undetermined", "unknown", or "all", the alternative character string
#' \code{str} is returned, otherwise the original is returned
#'
#' @export
#'
subForTCSAM<-function(x,str){
    xp<-x;
    xp <- ifelse(tolower(x)=="undetermined",str,xp);
    xp <- ifelse(tolower(x)=="unknown",str,xp);
    xp <- ifelse(tolower(x)=="all",str,xp);
    xp <- gsub(" ","_",xp,fixed=TRUE);
    return(xp);
}
