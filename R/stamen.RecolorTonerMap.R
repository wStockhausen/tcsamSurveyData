#'
#'@title Color land, water in a ggmap object derived from a Stamen toner map.
#'
#'@description Function to color land, water in a ggmap object derived from a Stamen toner map.
#'
#'@param map - ggmap object derived from a Stamen toner map
#'@param land - color specification for land pixels
#'@param water - color specification for water pixels
#'
#'@details Changes land ("#999999) and water ("#000000) pixels in a ggmap object
#'derived from a Stamen toner map to the specified colors. Attributes of the original
#'object are copied to the new object.
#'
#'@return a ggmap object
#'
#'@export
#'
stamen.RecolorTonerMap<-function(map,land="khaki",water="lightblue"){
    if (class(map)[1]!="ggmap"){
        
    }
    bbx  <- attr(map,"bb");
    src  <- attr(map, "source");
    mpt  <- attr(map, "maptype");
    zoom <-  attr(map, "zoom");
    
    idx <- map=="#000000"; #water pixels
    cmp <-map;
    cmp[idx]  <- water;
    cmp[!idx] <- land;
    
    class(cmp)<-c("ggmap","raster");
    attr(cmp, "bb")     <- bbx;
    attr(cmp, "source") <- src;
    attr(cmp, "maptype")<- mpt;
    attr(cmp, "zoom")   <- zoom;
    
    return(cmp);
}

