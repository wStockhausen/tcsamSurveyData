#'
#'@title Get map data objects associated with this package
#'
#'@description Function to return map data objects associated with this package.
#'
#'@param type - object type. one of ["ggmap","shape"]
#'@param name - object name. see details.
#'
#'@return ggmap or dataframe object from package data
#'
#'@details 'name' must be one of
#'\itemize{
#'  \item "EBS.stamen.toner.z06"
#'  \item "EBS.stamen.toner.z07"
#'  \item "EBS.polys.land"
#'  \item "EBS.polys.surveyblocks"
#'  \item "EBS.lines.bathy"
#'}
#'
#'@export
#'
getMapData<-function(type=c("ggmap","shape"),
                     name=c("EBS.stamen.toner.z06",
                            "EBS.stamen.toner.z07",
                            "EBS.polys.land",
                            "EBS.polys.surveyblocks",
                            "EBS.lines.bathy"),
                     verbosity=0){
    if (type=="ggmap"){
        if (name=="EBS.stamen.toner.z06") return(ggmap.EBS.stamen.toner.z06);
        if (name=="EBS.stamen.toner.z07") return(ggmap.EBS.stamen.toner.z07);
    } else if (type=="shape"){
        if (name=="EBS.polys.land") return(shp.EBS.polys.land);
        if (name=="EBS.polys.surveyblocks") return(shp.EBS.polys.surveyblocks);
        if (name=="EBS.lines.bathy") return(shp.EBS.lines.bathy);
    }
}