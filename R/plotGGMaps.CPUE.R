#'
#'@title Plot maps of survey cpue by year and other factors using ggmap and ggplot2.
#'
#'@description Function to plot maps of survey cpue by year and other factors using ggmap and ggplot2.
#'
#'@param cpue - dataframe of cpue from call to calcCPUE.ByHaul or calcCPUE.ByStation
#'@param type - type of cpue to plot ('abundance' or 'biomass')
#'@param facs - vector of factor column names to plot by (e.g., c('SEX', 'MATURITY')) or c() to ignore.
#'@param keepLevels - list (by elements of facs) of factor levels to keep before plotting
#'@param dropLevels - list (by elements of facs) of factor levels to drop before plotting
#'@param max - max for ALL factor combinations (NULL -> scale by max w/in each factor level combination)
#'@param scale - value to scale cpue by before plotting
#'@param map - ggmap raster object for base map background
#'@param bbox - coordinates of bounding box for map (left, bottom, right, top)
#'@param mapcolors - list of colors (land, water) for base map background
#'@param bathymetry - fortified spatial dataframe or list(dsn=,layer=) specifying bathymetry shapefile to include on base map
#'@param land - fortified spatial dataframe or list(dsn=,layer=) specifying land shapefile to include on base map
#'@param layers - list of ggplot layers to include in base map
#'@param label - page title, if 'facs' is empty (length 0)
#'@param ggtheme - ggplot2 theme
#'@param ncol - number of columns of plots per page
#'@param nrow - number of rows of plots per page
#'@param showPlot - flag to show plots immediately
#'@param verbosity - integer flag (>0) to print intermediate info
#'
#'@return list of ggplot2 plot objects to print
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
plotGGMaps.CPUE<-function(cpue,
                          type=c('abundance','biomass'),
                          facs=c("SEX","MATURITY","SHELL_CONDITION"),
                          keepLevels=list(SEX=c("MALE","FEMALE"),
                                           MATURITY=c("IMMATURE","MATURE"),
                                             SHELL_CONDITION=c("NEW_SHELL","OLD_SHELL")),
                          dropLevels=list(SEX=c("HERMAPHRODITE","MISSING"),
                                             MATURITY=c("MISSING"),
                                             SHELL_CONDITION=c("MISSING")),
                          max=NULL,
                          scale=1,
                          map=getMapData("ggmap","EBS.stamen.toner.z06"),
                          bbox=c(left=-180,bottom=54,right=-155,top=63),
                          mapcolors=list(land='darkgreen',water='lightblue'),
                          bathymetry=getMapData("shape","EBS.lines.bathy"),
                          land=getMapData("shape","EBS.polys.land"),
                          layers=NULL,
                          label="",
                          ggtheme=theme_grey(),
                          ncol=1,
                          nrow=4,
                          showPlot=FALSE,
                          verbosity=0){
#     require('ggmap');
#     require('ggplot2');
#     require('plyr');
#     require('reshape2');
#     require('rgdal');
#     require('scales')
#     require('sqldf');
    
    #determine data type to plot
    if (type=='abundance'){
        dType <- paste('num','CPUE',sep='');
    } else {
        dType <- paste('wgt','CPUE',sep='');
    }

    
    #melt and cast cpue to get dataframe for plotting
    nf<-length(facs);
    id.vars<-c("YEAR","LONGITUDE","LATITUDE");
    if (nf>0){
        id.vars<-c(id.vars,facs);
    }
    measure.vars<-dType;
    
    #melt the input dataframe
    mdfr<-melt(cpue,id.vars,measure.vars,factorsAsStrings=TRUE,value.name='value');
    mdfr$value<-mdfr$value/scale;#scale value
    
    #keep requested factor levels
    if (is.list(keepLevels)){
        kfacs<-names(keepLevels);
        for (kfac in kfacs){
            #keep only requested levels for factors in 'facs'
            if (any(kfac %in% facs)) mdfr<-mdfr[(mdfr[[kfac]] %in% keepLevels[[kfac]]),];
        }
    }
    #drop requested factor levels
    if (is.list(dropLevels)){
        dfacs<-names(dropLevels);
        for (dfac in dfacs){
            #drop only requested levels for factors in 'facs'
            if (any(kfac %in% facs)) mdfr<-mdfr[!(mdfr[[dfac]] %in% dropLevels[[dfac]]),];
        }
    }
    
    #find unique factor levels
    qry<-"select distinct
            YEAR&&facs
          from
            mdfr;"
    if (nf>0){
        qry<-gsub('&&facs',paste(',',facs,sep='',collapse=''),qry)
    } else {
        qry<-gsub('&&facs','',qry)
    }
    ufacs<-sqldf(qry);
    
    #separate unique years, factor levels 
    uyrs<-unique(ufacs$YEAR);
    if (nf>0){
        qry<-"select distinct &&facs
              from ufacs
              order by &&facs;";
        qry<-gsub("&&facs",paste(facs,collapse=','),qry);
        ufacs<-sqldf(qry);#ufacs no longer has YEAR
    }
    if (verbosity>1) View(ufacs);
    
    #cast the melted dataframe to aggregate over ignored potential factors
    str<-"&&facsYEAR+LONGITUDE+LATITUDE~.";
    if (nf>0){
        str<-gsub("&&facs",paste(facs,'+',sep='',collapse=''),str);
    } else {
        str<-gsub("&&facs",'',str);
    }
    dfr<-dcast(mdfr,
               str,
               fun.aggregate=sum,
               value.var="value")
    nms<-names(dfr);
    nms[length(nms)]<-'value';
    names(dfr)<-nms;
    if (verbosity>1) View(dfr);
    
    
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
    
    nr<-1;
    if (nf>0) nr<-nrow(ufacs);
    if (verbosity>1) cat("number of factor comb.s is ",nr,"\n");
    
    ctr<-0;
    ps<-list();
    for (rw in 1:nr){
        if (verbosity>1) cat("Processing factor combination",ufacs[rw,],'\n');
        idx<-!vector(mode='logical',length=nrow(dfr));#vector of TRUEs
        if (nf>0){
            for (fac in names(ufacs)){
                idx<-idx & (dfr[[fac]]==ufacs[rw,fac]);
            }
            label<-paste(tolower(as.vector(ufacs[rw,])),sep='',collapse=', ');
        }
        if (sum(idx)>0){
            maxp<-max;
            dfr1<-dfr[idx,];
            if (verbosity>1) cat("will plot",nrow(dfr1),"points\n");
            if (is.null(max)) maxp<-max(dfr1[["value"]],na.rm=TRUE);
            for (pg in 1:npg){ #loop over pages
                if (verbosity>1) cat("Plotting years",paste(uyrs[(pg-1)*mxp+c(1,mxp)],collapse="-"),"\n")
                dfrp<-dfr1[(dfr1$YEAR %in% uyrs[(pg-1)*mxp+(1:mxp)]),];
                if (verbosity>1) cat("##plotting",nrow(dfrp),"values\n");
                
                pL <- geom_point(mapping=aes(x=LONGITUDE,y=LATITUDE),
                                 data=dfrp,shape='.',color='black',alpha=1);
                pP <- geom_point(mapping=aes(x=LONGITUDE,y=LATITUDE,size=value,fill=value,color=value),
                                 data=dfrp,alpha=0.8,shape=21);
                pS <- scale_size_area(oob=squish,limits=c(0,maxp));
                pF <- scale_fill_gradient(low='blue',high='red');
                pC <- scale_color_gradient(low='blue',high='red');
                p <- pBase + pL + pP + pS + pF + pC + facet_wrap(~YEAR,ncol=ncol) + ggtheme;
                p <- p + guides(fill=guide_colorbar(type,order=1),
                                size=guide_legend('',order=2),
                                color=FALSE);
                p <- p + labs(list(x='Longitude',y='Latitude'));
                p <- p + ggtitle(label);
                if (showPlot) print(p);
                ctr<-ctr+1;
                ps[[ctr]]<-p;
            }#pg loop
        }
    }#rw loop
    
    return(ps);
}
