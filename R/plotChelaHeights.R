#'
#'@title Plot chela height data
#'
#'@description Function to plot chela height data
#'
#'@param dfrp - dataframe of chela height data from call to getChelaHeights(...)
#'@param ratio - default ratio 
#'@param discrim - optional function taking a vector of sizes (CW) and returning the associated chela height cutline values yielding a "mature" classification
#'@param useLnScales - flag (T/F) to plot data on ln-scales
#'@param aggYears - number of years over which to aggregate
#'@param rng - y-axis range 
#'@param ncol - number of coulmns to use for faceting
#'@param showPlot - flag to show plots immediately
#'
#'@return ggplot2 plot object to print
#'
#'@details none.
#'
#'@importFrom scales squish
#'
#'@import ggplot2
#'
#'@export
#'
plotChelaHeights<-function(dfrp,
                           ratio=0.18,
                           discrim=NULL,
                           useLnScales=FALSE,
                           aggYears=5,
                           rng=NULL,
                           ncol=2,
                           showPlot=FALSE){
    #make local copy
    dfr<-dfrp;
    
    #add in fixed ratio characterization
    if (is.null(discrim)) discrim<-function(z){return(ratio*z);};
    #cat(discrim(dfr$SIZE),"\n")
    #cat(dfr$CHELA_HEIGHT>=discrim(dfr$SIZE),"\n")
    idx<-dfr$CHELA_HEIGHT>=discrim(dfr$SIZE);
    
    dfr[["RATIO"]]<-'IMMATURE';#create new column 
    dfr$RATIO[idx]<-'MATURE';
    
    cutline <-discrim(dfr$SIZE);
    idx     <-order(dfr$SIZE);

    if (useLnScales){
        dfr$CHELA_HEIGHT<-log(dfr$CHELA_HEIGHT);
        dfr$SIZE        <-log(dfr$SIZE);
        dfrc            <-data.frame(x=dfr$SIZE[idx],y=log(cutline[idx]));
    } else {
        if (is.null(rng)) rng<-c(0,max(dfr$CHELA_HEIGHT,na.rm=TRUE));
        dfrc <-data.frame(x=dfr$SIZE[idx],y=cutline[idx]);
    }
        
    #aggregate years and convert to discrete scale
    dfr$YEAR<-as.character(aggYears*floor(dfr$YEAR/aggYears));
    dfri<-dfr[dfr$RATIO=="IMMATURE",]
    dfrm<-dfr[dfr$RATIO=="MATURE",]
    
    #make plot
    p <- ggplot(data=dfr);
    p <- p + geom_point(aes(x=SIZE,y=CHELA_HEIGHT,color=RATIO),size=2,position='jitter')
    p <- p + stat_smooth(data=dfri,mapping=aes(x=SIZE,y=CHELA_HEIGHT),method='lm',colour="green")
    p <- p + stat_smooth(data=dfrm,mapping=aes(x=SIZE,y=CHELA_HEIGHT),method='lm',colour="blue")
    if (is.null(discrim)){
        p <- p + geom_abline(intercept=0,slope=ratio,color='red',linetype=2)
    } else {
        p <- p + geom_line(data=dfrc,aes(x=x,y=y),color='red',linetype=2)
    }
    p <- p + scale_colour_brewer(type='div',palette=2)
    p <- p + scale_x_continuous() 
    p <- p + scale_y_continuous(breaks=pretty(rng),limits=rng,expand=c(0.01,0),oob=scales::squish)
    p <- p + facet_wrap(~YEAR,ncol=ncol) 
    if (!useLnScales){
        p <- p + ggplot2::labs(x="size (mm CW)",y="chela height (mm)");
    } else {
        p <- p + ggplot2::labs(x="ln[size (mm CW)]",y="ln[chela height (mm)]");
    }
    p <- p + guides(color=guide_legend(''))
    if (showPlot) print(p);
    
    return(p);
}
