#Scripts to create map data
#NOTE: make sure to set working directory to 'data-raw'

##load required libraries
require('ggmap');
#require('raster');
require('rgdal');
require('rgeos');

##-----------------------------------------------------
#'
#'@title Clip a set of polygons using a bounding box
#'
#'@description Function to clip a set of polygons using a bounding box
#'
#'@param shp - set of polygons (from call to readOGR)
#'@param bb  - bounding box (see details)
#'@param verbosity - integer flag (>0) to print intermediate info
#'
#'@return - spatial dataframe of clipped polygons
#'
#'@details The bounding box can be one of the following (see extent{raster}):\cr
#' 1. vector of coordinates given as xmin, xmax, ymin, ymax\cr
#' 2. vector of coordinates given as (with names): "left","bottom","top","right"\cr
#' 3. matrix with columns corresponding to "x","y" and rows to "lower left", "upper right"\cr
#' 4. dataframe with columns corresponding to "x","y" and rows to "lower left", "upper right"\cr
#' \cr
#' Also, the bounding box must be in the same coordinate system as th set of polygons.
#' 
#'@importFrom raster extent
#'@import rgeos
#'@import sp
#' 
#' @export
#' 
gClip <- function(shp, bb,verbosity=0){
#    require('raster')
#    require('rgeos')
    if (verbosity>0) print(bb);
    bp<-bb;
    if (class(bp)=="data.frame"){
        if (verbosity>1) cat("bp is a data.frame\n")
        b_poly <- as(raster::extent(as.vector((as.matrix(bp)))), "SpatialPolygons")
    } else if (class(bp) == "matrix") {
        if (verbosity>1) cat("bp is a matrix\n")
        b_poly <- as(raster::extent(as.vector(t(bp))), "SpatialPolygons")
    } else {
        #bp expected in order xmin,xmax,ymin,ymax
        if ('left'==tolower(names(bp)[1])){
            if (verbosity>1) cat("bp is a vector w/ order: left, bottom, right, top\n")
            #order is left, bottom, right, top (xmin, ymin, xmax, ymax)
            bp<-c(bb[1],bb[3],bb[2],bb[4]);
            names(bp)<-c('xmin','xmax','ymin','ymax');
            if (verbosity>1) print(bp);
        }
        b_poly <- as(raster::extent(bp), "SpatialPolygons")
    }
    if (verbosity>0) print(b_poly);
    
    proj4string(b_poly) <- proj4string(shp)
    gIntersection(shp, b_poly, byid = T)
}
##-----------------------------------------------------

##Define EBS bounding box
bbox<-c(left=-180,bottom=54,right=-155,top=63);

##Create Alaska land mask for EBS
landmask<-list(dsn=file.path(getwd(),'MapData/Land'),layer='Alaska');
land<-readOGR(dsn=landmask$dsn,layer=landmask$layer);
land.WGS84<-spTransform(land, CRS("+init=epsg:4326"));
land.clip<-gClip(land.WGS84,bbox);
shp.EBS.polys.land<-fortify(land.clip);
save(shp.EBS.polys.land,file="../data/shp.EBS.polys.land.RData")


##Create bathymetry for EBS
bathymetry<-list(dsn=file.path(getwd(),'MapData/Bathymetry'),layer='ShelfBathymetry');
depth<-readOGR(dsn=bathymetry$dsn,layer=bathymetry$layer);
depth.WGS84<-spTransform(depth, CRS("+init=epsg:4326"));
shp.EBS.lines.bathy<-fortify(depth.WGS84);
save(shp.EBS.lines.bathy,file="../data/shp.EBS.lines.bathy.RData");


##Create EBS survey blocks
surveyblocks<-list(dsn=file.path(getwd(),'MapData/Surveys'),layer='NMFS_EBSGroundfishSurveyBlocks');
blocks<-readOGR(dsn=surveyblocks$dsn,layer=surveyblocks$layer);
blocks.WGS84<-spTransform(blocks, CRS("+init=epsg:4326"));
blocks.clip<-gClip(blocks.WGS84,bbox);
shp.EBS.polys.surveyblocks<-fortify(blocks.clip);
save(shp.EBS.polys.surveyblocks,file="../data/shp.EBS.polys.surveyblocks.RData")

    
##Create EBS stamen toner ggmap object at zoom = 6
ggmap.EBS.stamen.toner.z06<-ggmap::get_stamenmap(bbox=bbox,maptype="toner-background",zoom=6,messaging=TRUE);
save(ggmap.EBS.stamen.toner.z06,file="../data/ggmap.EBS.stamen.toner.z06.RData")

    
##Create EBS stamen toner ggmap object at zoom = 7
ggmap.EBS.stamen.toner.z07<-ggmap::get_stamenmap(bbox=bbox,maptype="toner-background",zoom=7,messaging=TRUE);
save(ggmap.EBS.stamen.toner.z07,file="../data/ggmap.EBS.stamen.toner.z07.RData")
