#'
#' @title Re-classify immature, old shell crab as immature, new shell crab
#'
#' @description Function to re-classify immature, old shell crab as immature, new shell crab.
#'
#' @param dfr - dataframe to run re-classification on
#' @param colMat - name of column with maturity designation
#' @param colSC - name of column with shell condition designation
#'
#' @return dataframe with same strucuture as input and shell condition converted to "NEW_SHELL" for
#' immature crab formerly classified as "OLD_SHELL" (irrespective of case).
#'
#' @details None.
#'
#' @export
#'
convertImmOS<-function(dfr,
                       colMat="MATURITY",
                       colSC="SHELL_CONDITION"
                       ){
  idx<-(toupper(dfr[[colMat]])=="IMMATURE")&(toupper(dfr[[colSC]])=="OLD_SHELL");
  dfr[[colSC]]<-"NEW_SHELL";
  return(dfr);
}
