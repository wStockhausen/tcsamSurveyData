#'
#'@title test ggmap plotting.
#'
#'@description function to test ggmap plotting
#'
#'@details test
#'
#'@import ggmap
#'@import ggplot2
#'@import rgdal
#'
#'
testGGMaps<-function(){
    ##require("ggmap")
    x='MALE'
    m='MATURE'
    s='NEW_SHELL'
    year=2014
    cpue<-tbl.CPUE.ByStation;
    idx.x<-(cpue$SEX==x)
    idx.m<-(cpue$MATURITY==m)
    idx.s<-(cpue$SHELL_CONDITION==s)
    cpue<-cpue[idx.x&idx.m&idx.s,]
    
    bs<-c(left=-180,bottom=54,right=-155,top=63)
    #bsp<-make_bbox(LONGITUDE,LATITUDE,data=cpue)
    map<-get_stamenmap(bbox=bs,maptype="terrain",zoom=6,messaging=TRUE)
    p <- ggmap(map,extent='panel',maprange=FALSE) 
    p <- p + geom_point(mapping=aes(x=LONGITUDE,y=LATITUDE,size=wgtCPUE,color=wgtCPUE),data=cpue[cpue$YEAR==year,],alpha=0.5)
    #print(p)
    
    ##require('rgdal')
    depth<-readOGR(dsn=path.expand("~/Projects/MapData/Bathymetry"),layer="ShelfBathymetry")
    summary(depth)
    depth.WGS84<-spTransform(depth, CRS("+init=epsg:4326"))
    summary(depth.WGS84)
    depth.f<-fortify(depth.WGS84)
    
    pp <- p + geom_path(mapping=aes(x=long,y=lat,group=group),data=depth.f)
    #print(pp)
    
    land<-readOGR(dsn=path.expand("~/Projects/MapData/Land"),layer="Alaska")
    land.WGS84<-spTransform(land, CRS("+init=epsg:4326"))
    land.clip<-gClip(land.WGS84,bs)
    land.clip.f<-fortify(land.clip)
    ppp <- pp + geom_polygon(mapping=aes(x=long,y=lat,group=group),data=land.clip.f,alpha=0.5)
    print(ppp)
}
