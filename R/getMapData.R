getMapData<-function(type=c("ggmap","shape"),
                     name=c("EBS.stamen.toner.z06",
                            "EBS.stamen.toner.z07"),
                     verbosity=0){
    attach
    if (type=="ggmap"){
        if (name=="EBS.stamen.toner.z06") return(ggmap.EBS.stamen.toner.z06);
        if (name=="EBS.stamen.toner.z07") return(ggmap.EBS.stamen.toner.z07);
    } else if (type=="shape"){
        if (name=="EBS.polys.land")  return(shp.EBS.polys.land);
        if (name=="EBS.lines.bathy") return(shp.EBS.lines.bathy);
    }
}