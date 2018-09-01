#'
#'@title Plot maturity ogive data
#'
#'@description Function to plot maturity ogive data.
#'
#'@param dfrp - dataframe of maturity ogive data
#'@param facet_grid - formula to use for a faceted grid plot (default=NULL)
#'@param ncol - number of plot columns to use when facet_grid is NULL
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
#'
#'@import ggplot2
#'
#'@export
#'
plotMaturityOgives<-function(dfrp,
                             facet_grid="dcd~.",
                             ncol=4,
                             title=NULL,
                             showPlot=FALSE){
    #make local copy
    dfr<-dfrp;


    xlab<-"size (mm CW)";
    ylab<-"maturity ogive";

    #make plot
    clrCol<-"y";
    sizCol<-"numIndivs";
    shpCol<-"SEX";
    dfr$dcd<-floor(dfr$YEAR/10)*10;
    dfr$y  <-as.factor(dfr$YEAR-dfr$dcd);
    p <- ggplot2::ggplot(data=dfr);
    p <- p + ggplot2::geom_point(ggplot2::aes_string(x="SIZE",y="ogive",colour=clrCol,
                                                     size=sizCol,shape=shpCol),position='jitter');
    p <- p + ggplot2::scale_size_area();
    p <- p + ggplot2::scale_colour_brewer(type='div',palette=2)
    p <- p + ggplot2::scale_x_continuous();
    p <- p + ggplot2::scale_y_continuous(breaks=pretty(c(0,1)),limits=c(0,1),expand=c(0.01,0))
    if (!is.null(facet_grid)){
      if (packageVersion("ggplot2")<="2.2.1") p <- p + ggplot2::facet_grid(facets=facet_grid);
      if (packageVersion("ggplot2")>="3.0.0") p <- p + ggplot2::facet_grid(rows=facet_grid);
    }
    p <- p + ggplot2::labs(x=xlab,y=ylab,size="sample\n size",shape="sex");
    if (!is.null(title)) p <- p + ggplot2::ggtitle(title);
    p <- p + ggplot2::guides(color=guide_legend('year'))
    if (showPlot) print(p);

    return(p);
}
