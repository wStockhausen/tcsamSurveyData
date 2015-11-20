#'
#'@title Plot maps of survey cpue by year and other factors using ggmap and ggplot2.
#'
#'@description Function to plot maps of survey cpue by year and other factors using ggmap and ggplot2.
#'
#'@param cpue - dataframe of cpue from call to calcCPUE.ByHaul or calcCPUE.ByStation
#'@param type - type of cpue to plot ('abundance' or 'biomass')
#'@param facs - factors to plot (e.g., 'SEX', 'MATURITY')
#'@param keepLevels - list (by elements of facs) of factor levels to keep before plotting
#'@param dropLevels - list (by elements of facs) of factor levels to drop before plotting
#'@param max - max for ALL factor combinations (NULL -> scale by max w/in each factor level combination)
#'@param bbox - coordinates of bounding box for map (left, bottom, right, top)
#'@param bathymetry - list(dsn=,layer=) specifying bathymetry shapefile to include on map
#'@param land - list(dsn=,layer=) specifying land shapefile to include on map
#'@param layers - list of lists(dsn=,layer=) specifying other shapefiles to include on map
#'@param ggtheme - ggplot2 theme
#'@param ncol - number of columns of plots per page
#'@param nrow - number of rows of plots per page
#'@param showPlots - flag to show plots immediately
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
                          bbox=c(left=-180,bottom=54,right=-155,top=63),
                          bathymetry=list(dsn=path.expand('~/MapData/Bathymetry'),layer='ShelfBathymetry'),
                          land=list(dsn=path.expand('~/MapData/Land'),layer='Alaska'),
                          layers=NULL,
                          ggtheme=theme_grey(),
                          ncol=1,
                          nrow=4,
                          showPlots=FALSE){
#     require('ggmap');
#     require('ggplot2');
#     require('plyr');
#     require('reshape2');
#     require('rgdal');
#     require('scales')
#     require('sqldf');
    
    #determine data type to plot
    if (type=='abundance'){
        scale<-1;#
        dType <- paste('num','CPUE',sep='');
    } else {
        scale<-1;#
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
    mdfr<-reshape2::melt(cpue,id.vars,measure.vars,factorsAsStrings=TRUE,value.name='value');
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
    ufacs<-sqldf::sqldf(qry);
    
    #separate unique years, factor levels 
    uyrs<-unique(ufacs$YEAR);
    if (nf>0){
        qry<-"select distinct &&facs
              from ufacs
              order by &&facs;";
        qry<-gsub("&&facs",paste(facs,collapse=','),qry);
        ufacs<-sqldf::sqldf(qry);#ufacs no longer has YEAR
    }
    
    #cast the melted dataframe to aggregate over ignored potential factors
    str<-"&&facsYEAR+LONGITUDE+LATITUDE~.";
    if (nf>0){
        str<-gsub("&&facs",paste(facs,'+',sep='',collapse=''),str);
    } else {
        str<-gsub("&&facs",'',str);
    }
    dfr<-reshape2::dcast(mdfr,
                         str,
                         fun.aggregate=sum,
                         value.var="value")
    nms<-names(dfr);
    nms[length(nms)]<-'value';
    names(dfr)<-nms;
    
    
    #get map
    map<-get_stamenmap(bbox=bbox,maptype="terrain",zoom=6,messaging=TRUE)
    pMap <- ggmap(map,extent='panel',maprange=FALSE)
    
    #set up bathymetry layer
    depth<-readOGR(dsn=bathymetry$dsn,layer=bathymetry$layer)
    depth.WGS84<-spTransform(depth, CRS("+init=epsg:4326"))
    depth.f<-fortify(depth.WGS84)
    pDepth <- geom_path(mapping=aes(x=long,y=lat,group=group),data=depth.f)
    
    #set up land layer
    land<-readOGR(dsn=land$dsn,layer=land$layer)
    land.WGS84<-spTransform(land, CRS("+init=epsg:4326"))
    land.clip<-gClip(land.WGS84,bbox)
    land.clip.f<-fortify(land.clip)
    pLand <- geom_polygon(mapping=aes(x=long,y=lat,group=group),data=land.clip.f,alpha=0.5)
    
    #set up other layers
    #TODO
    
    #base map for all plots
    pBase <- pMap + pDepth + pLand; #TODO: add in pLayers
    
    #loop over factors, years to make plots
    mxp<-nrow*ncol;
    npg<-ceiling(length(uyrs)/mxp)
    
    nr<-1;
    if (nf>0) nr<-length(ufacs)
    ctr<-0;
    ps<-list();
    for (rw in 1:nr){
        idx<-!vector(mode='logical',length=nrow(dfr));#vector of TRUEs
        if (nf>0){
            for (fac in names(ufacs)){
                idx<-idx & (dfr[[fac]]==ufacs[rw,fac]);
            }
        }
        mxp<-max;
        dfr1<-dfr[idx,];
        if (is.null(max)) mxp<-max(dfr1[["."]],na.rm=TRUE);
        for (pg in 1:npg){ #loop over pages
            dfrp<-dfr1[(dfr$YEAR %in% uyrs[(pg-1)*mxp+1:mxp]),];
            
            pL <- geom_point(mapping=aes(x=LONGITUDE,y=LATITUDE),
                             data=dfrp,shape='.',color='black',alpha=1);
            pP <- geom_point(mapping=aes(x=LONGITUDE,y=LATITUDE,size=`.`,fill=`.`,color=`.`),
                             data=dfrp,alpha=0.8,shape=21);
            pS <- scale_size_area(oob=scales::squish,limits=c(0,mxp))
            pF <- scale_fill_gradient(low='blue',high='red')
            pC <- scale_color_gradient(low='blue',high='red')
            p <- pBase + pL + pP + pS + pF + pC + facet_wrap(~YEAR,ncol=ncol) + ggtheme
            p <- p + guides(size=guide_legend(''),fill=guide_colorbar(type),color=FALSE)
            p <- p + labs(list(x='Longitude',y='Latitude'))
            p <- p + ggtitle(paste(tolower(as.vector(ufacs[rw,])),sep='',collapse=' ,'))
            if (showPlot) print(p);
            ctr<-ctr+1;
            ps[[ctr]]<-p;
        }
    }
    
    return(ps);
}
