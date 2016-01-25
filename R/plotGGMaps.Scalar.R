#'
#'@title Plot maps of survey temperature (or other scalar) by year using ggmap and ggplot2
#'
#'@description Function to plot maps of survey temperature (or other scalar) by year using ggmap and ggplot2
#'
#'@param dfr - dataframe with columns YEAR, LATITUDE, LONGITUDE, VALUE
#'@param scalar - name of scalar column in dfr to plot
#'@param min - min value to plot (or NULL)
#'@param max - max value to plot (or NULL)
#'@param truncateMin - flag to remove values below min
#'@param truncateMax - flag to remove values above max
#'@param map - ggmap raster object for base map background
#'@param bbox - coordinates of bounding box for map (left, bottom, right, top)
#'@param mapcolors - list of colors (land, water) for base map background
#'@param bathymetry - fortified spatial dataframe or list(dsn=,layer=) specifying bathymetry shapefile to include on base map
#'@param land - fortified spatial dataframe or list(dsn=,layer=) specifying land shapefile to include on base map
#'@param layers - list of ggplot layers to include in base map
#'@param label - page title
#'@param ggtheme - ggplot2 theme
#'@param returnLayerObjects - flag to return ggplot2 layer objects (instead of ggplot2 plot objects)
#'@param ncol - number of columns of plots per page
#'@param nrow - number of rows of plots per page
#'@param showPlot - flag to show plots immediately
#'@param verbosity - integer flag (>0) to print intermediate info
#'
#'@return list of ggplot2 plot or layer objects to print
#'
#'@details none.
#'
#'@importFrom plyr .
#'@importFrom reshape2 dcast
#'@importFrom reshape2 melt 
#'@importFrom scales squish
#'@importFrom sqldf sqldf
#'
#'@import ggmap
#'@import ggplot2
#'@import rgdal
#'
#'@export
#'
plotGGMaps.Scalar<-function(dfr,
                            scalar='TEMP',
                            min=NULL,
                            max=NULL,
                            truncateMin=TRUE,
                            truncateMax=TRUE,
                            map=getMapData("ggmap","EBS.stamen.toner.z06"),
                            bbox=c(left=-180,bottom=54,right=-155,top=63),
                            mapcolors=list(land='darkgreen',water='lightblue'),
                            bathymetry=getMapData("shape","EBS.lines.bathy"),
                            land=getMapData("shape","EBS.polys.land"),
                            layers=NULL,
                            label="Temperature (deg C)",
                            ggtheme=theme_grey(),
                            ncol=1,
                            nrow=4,
                            showPlot=FALSE,
                            verbosity=0){

    dfrp<-dfr[c('YEAR','LATITUDE','LONGITUDE',scalar),];
    
    #get background map
    if (!is.null(map)){
        if (verbosity>1) {cat("including raster on background map\n");}
        if (class(map)[1]==c('ggmap')){
            #do nothing: map in correct format
        } else {
            #TODO: need to do something here
            map<-ggmap::get_stamenmap(bbox=bbox,maptype="toner-background",zoom=map,messaging=TRUE);
        }
        map<-stamen.RecolorTonerMap(map,mapcolors$land,mapcolors$water);
        pMap <- ggmap(map,extent='panel',maprange=FALSE);
        if (verbosity>1) {
            cat('printing pMap\n')
            print(pMap);
        }
    } else {
        if (verbosity>1) {cat("excluding raster from background map\n");}
        pMap <- NULL;
    }
    
    #get or set up bathymetry layer
    if (!is.null(bathymetry)){
        if (verbosity>1) {cat("including bathymetry on background map\n");}
        if (is.data.frame(bathymetry)){
            #do nothing: bathymetry is in correct format (a fortified spatial dataframe)
            if (verbosity>1) {cat("bathymetry is a spatial dataframe\n");}
        } else if (is.list(bathymetry)){
            if (verbosity>1) {cat("reading bathymetry file\n");}
            depth<-readOGR(dsn=bathymetry$dsn,layer=bathymetry$layer);
            depth.WGS84<-spTransform(depth, CRS("+init=epsg:4326"));
            bathymetry<-fortify(depth.WGS84);
            rm(depth,depth.WGS84);
        }
        pDepth <- geom_path(mapping=aes(x=long,y=lat,group=group),data=bathymetry,color='grey50');
    } else {
        if (verbosity>1) {cat("excluding bathymetry from background map\n");}
        pDepth <- NULL;
    }
    
    #get or set up land mask layer
    if (!is.null(land)){
        if (verbosity>1) {cat("including land on background map\n");}
        if (is.data.frame(land)){
            #do nothing: land is in correct format (a fortified spatial dataframe)
            if (verbosity>1) {cat("land is a spatial dataframe\n");}
        } else if (is.list(land)){
            if (verbosity>1) {cat("reading land file\n");}
            land<-readOGR(dsn=land$dsn,layer=land$layer);
            land.WGS84<-spTransform(land, CRS("+init=epsg:4326"));
            land.clip<-gClip(land.WGS84,bbox);
            land<-fortify(land.clip);
        }
        pLand <- geom_polygon(mapping=aes(x=long,y=lat,group=group),data=land,alpha=0.25,size=0);
    } else {
        if (verbosity>1) {cat("excluding land on background map\n");}
        pLand <- NULL;
    }
    
    #base map for all plots
    pBase <- pMap + pDepth + pLand; 
    #add in other map layers
    if (!is.null(layers)){
        for (lyr in layers) pBase <- pBase + lyr;
    }
    if (!is.null(pBase)&&(verbosity>1)) print(pBase);
    
    #loop over factors, years to make plots
    mxp<-nrow*ncol;
    npg<-ceiling(length(uyrs)/mxp);#number of maps per page
    
    if (!is.null(max)) {
        idx<-dfrp[[scalar]]>max;
        if (truncateMax){
            dfrp[[scalar]][idx]<-NA;
        } else {
            dfrp[[scalar]][idx]<-max;
        }
    }
    for (pg in 1:npg){ #loop over pages
        if (verbosity>1) cat("Plotting years",paste(uyrs[(pg-1)*mxp+c(1,mxp)],collapse="-"),"\n")
        dfrp<-dfr1[(dfr1$YEAR %in% uyrs[(pg-1)*mxp+(1:mxp)]),];
        if (verbosity>1) cat("##plotting",nrow(dfrp),"values\n");
        
        pC <- geom_contour(mapping=aes(x=LONGITUDE,y=LATITUDE,z=scalar),
                         data=dfrp,colour='black',alpha=1,bins=1);
        pD <- geom_density_2d(mapping=aes(x=LONGITUDE,y=LATITUDE,z=scalar),
                         data=dfrp,colour='black',alpha=1,bins=1);
        pF <- scale_fill_gradient(low='blue',high='red');
        pC <- scale_color_gradient(low='blue',high='red');
        p <- pBase + pL + pP + pS + pF + pC + facet_wrap(~YEAR,ncol=ncol) + ggtheme;
        p <- p + guides(fill=guide_colorbar(fillTitle,order=1),
                        size=guide_legend('',order=2),
                        color=FALSE);
        p <- p + labs(list(x='Longitude',y='Latitude'));
        p <- p + ggtitle(label);
        if (showPlot) print(p);
        ctr<-ctr+1;
        ps[[ctr]]<-p;
    }#pg loop

    return(ps);
}
