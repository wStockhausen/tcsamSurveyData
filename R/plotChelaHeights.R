#'
#'@title Plot chela height data
#'
#'@description Function to plot chela height data.
#'
#'@param dfrp - dataframe of chela height data from call to getChelaHeights(...)
#'@param ratio - default ratio (if NULL, maturity status will not be characterized)
#'@param discrim - optional function taking a vector of sizes (CW) and returning the associated chela height cutline values yielding a "mature" classification
#'@param byShellCondition - flag (T/F) to color points by shell condition
#'@param facet_grid - formula to use for a faceted grid plot (default=NULL)
#'@param plotRatio - flag to plot CH/CW vs CW, rather than CH vs. CW (default=FALSE)
#'@param plotLMs - flag to plot linear model fits to immature/mature data (default=TRUE)
#'@param useLnScales - flag (T/F) to plot data on ln-scales default=FALSE)
#'@param aggYears - number of years over which to aggregate
#'@param rng - y-axis range
#'@param ncol - number of coulmns to use for faceting
#'@param title - title for plot
#'@param showPlot - flag to show plots immediately
#'
#'@return ggplot2 plot object to print
#'
#'@details If "ratio" is NULL (not the default), then maturity status (immature/mature) will not be determined
#'(even if "discrim" is given) and the maturity state-specific linear model fits (CH ~ CW or ln{CH}~ln{CW})
#'will not be shown. If "ratio" is not NULL (the default), then the discriminant function (if "discrim" is not NULL)
#'or the given ratio will be used to charaterize maturity state on the basis of CH vs. CW.
#'
#'If "facet_grid" is NULL, then plots are produced by facet_wrapping by YEAR. Otherwise, the faceting formula given by
#'"facet_grid" is used to create a faceted grid plot.
#'
#'@importFrom scales squish
#'@importFrom utils head packageVersion
#'
#'@import ggplot2
#'
#'@export
#'
plotChelaHeights<-function(dfrp,
                           ratio=0.18,
                           discrim=NULL,
                           byShellCondition=TRUE,
                           facet_grid=NULL,
                           plotRatio=FALSE,
                           plotLMs=TRUE,
                           useLnScales=FALSE,
                           aggYears=5,
                           rng=NULL,
                           ncol=2,
                           title=NULL,
                           showPlot=FALSE){
    #make local copy
    dfr<-dfrp;

    #add in fixed ratio characterization
    cutline<-NULL;
    if (!is.null(ratio)){
        if (is.null(discrim)) {
            discrim<-function(z){return(ratio*z);};
            if (plotRatio) discrim<-function(z){return(ratio+0*z);};
        }
        idx<-dfr$CHELA_HEIGHT >= discrim(dfr$SIZE);
        if (plotRatio) idx<-dfr$CHELA_HEIGHT/dfr$SIZE >= discrim(dfr$SIZE);
        dfr[["RATIO"]]<-'IMMATURE';#create new column
        dfr$RATIO[idx]<-'MATURE';

        cutline <-discrim(dfr$SIZE);
    } else {
        dfr[["RATIO"]]<-'UNDETERMINED';
    }

    xlab<-"size (mm CW)";
    ylab<-"chela height (mm)";

    if (plotRatio) {
        ylab<-"ratio (CH/CW)";
        dfr$CHELA_HEIGHT<-dfr$CHELA_HEIGHT/dfr$SIZE;
    }

    #set up scales and dataframe for cutline
    dfrc<-NULL;
    idx <-order(dfr$SIZE);
    if (useLnScales){
        xlab<-"ln[size (mm CW)]";
        if (plotRatio) {ylab<-"ln(CH/CW)";} else {ylab<-"ln[chela height (mm)]";}
        dfr$CHELA_HEIGHT<-log(dfr$CHELA_HEIGHT);
        dfr$SIZE        <-log(dfr$SIZE);
        if (!is.null(cutline)) dfrc<-data.frame(x=dfr$SIZE[idx],y=log(cutline[idx]));
    } else {
        if (is.null(rng)) rng<-c(0,max(dfr$CHELA_HEIGHT,na.rm=TRUE));
        if (!is.null(cutline)) dfrc <-data.frame(x=dfr$SIZE[idx],y=cutline[idx]);
    }

    #aggregate years and convert to discrete scale
    dfr$YEAR<-as.character(aggYears*floor(dfr$YEAR/aggYears));

    #set up dataframes with immature, mature separately
    if (!is.null(ratio)){
        dfri<-dfr[dfr$RATIO=="IMMATURE",]
        dfrm<-dfr[dfr$RATIO=="MATURE",]
    }

    #make plot
    p <- ggplot(data=dfr);
    p <- p + geom_point(aes(x=SIZE,y=CHELA_HEIGHT,color=RATIO),size=2,position='jitter')
    if (!is.null(ratio)&plotLMs){
        p <- p + stat_smooth(data=dfri,mapping=aes(x=SIZE,y=CHELA_HEIGHT),method='lm',colour="green")
        p <- p + stat_smooth(data=dfrm,mapping=aes(x=SIZE,y=CHELA_HEIGHT),method='lm',colour="blue")
    }
    if (byShellCondition) p <- p + geom_point(aes(x=SIZE,y=CHELA_HEIGHT,color=SHELL_CONDITION),size=1,position='jitter')
    if (!is.null(ratio)){
        cat("Plotting cutline!!\n")
        utils::head(dfrc);
        p <- p + geom_line(data=dfrc,aes(x=x,y=y),color='red',linetype=2)
        # if (is.null(discrim)){
        #     p <- p + geom_abline(intercept=0,slope=ratio,color='red',linetype=2)
        # } else {
        #     p <- p + geom_line(data=dfrc,aes(x=x,y=y),color='red',linetype=2)
        # }
    }
    p <- p + scale_colour_brewer(type='div',palette=2)
    p <- p + scale_x_continuous()
    p <- p + scale_y_continuous(breaks=pretty(rng),limits=rng,expand=c(0.01,0),oob=scales::squish)
    if (is.null(facet_grid)){
        p <- p + facet_wrap(~YEAR,ncol=ncol)
    } else {
        if (packageVersion("ggplot2")<="2.2.1") p <- p + facet_grid(facets=facet_grid);
        if (packageVersion("ggplot2")>="3.0.0") p <- p + facet_grid(rows=facet_grid);
    }
    p <- p + ggplot2::labs(x=xlab,y=ylab);
    if (!is.null(title)) p <- p + ggtitle(title);
    p <- p + guides(color=guide_legend(''))
    if (showPlot) print(p);

    return(p);
}
