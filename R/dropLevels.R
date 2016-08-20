#'
#'@title Drop factor levels from a dataframe
#'
#'@description Function to drop factor levels from a dataframe.
#'
#'@param tbl - dataframe to drop levels from
#'@param dropLevels - factor levels to drop 
#'@param verbosity - integer flag indicating level of printed output (0=off,1=minimal,2=full)
#'
#'@return dataframe
#'
#'@details Distinct levels of each factor can be dropped from the
#'exported dataframe by seting dropLevels to a list with names corresponding to 
#'factor columns and values being vectors of factor levels to drop.
#'
#'@export
#'
dropLevels<-function(tbl,
                     dropLevels=NULL){
    
    #drop requested factor levels
        dfacs<-names(dropLevels);
        for (dfac in dfacs){
            tbl<-tbl[!(tbl[[dfac]] %in% dropLevels[[dfac]]),];
        }


    return(tbl);
}