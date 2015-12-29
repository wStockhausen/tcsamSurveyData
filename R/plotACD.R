#'
#'@title Plot time series of aggregated catch data.
#'
#'@description Function to plot time series of aggregated catch data.
#'
#'@param acd - dataframe from call to aggregateCatchData(...)
#'@param ylab - y-axis label
#'@param ci - confidence interval for error bars (e.g., 0.95)
#'@param ci.type - confidence interval type ('normal', 'lognormal')
#'@param xlims - x-axis limits (calculated internally if NULL)
#'@param ylims - y-axis limits (calculated internally if NULL)
#'@param ggtheme - ggplot2 theme
#'@param jitter - flag to jitter points
#'@param ncol - number of columns of plots per page
#'@param nrow - number of rows of plots per page
#'@param showPlots - flag to show plots immediately
#'@param verbosity - flag (>0) to print diagnostic output
#'
#'@return list of ggplot2 plot objects
#'
#'@details The time series for each factor level combination is plotted
#'separately. Distinct levels of each factor can be dropped from the
#'final plot by seting dropLevels to a list with names corresponding to 
#'factor columns and values being vectors of factor levels to drop.
#' 
#'One plot is created for each distinct level of 'STRATUM'.
#'
#'@import ggplot2
#'@importFrom plyr .
#'@importFrom reshape2 dcast
#'@importFrom reshape2 melt
#'@importFrom scales squish
#'
#'@export
#'
plotACD<-function(acd,
                  ylab="",
                  ci=0.8,
                  ci.type='normal',
                  xlims=NULL,
                  ylims=NULL,
                  ggtheme=theme_grey(),
                  jitter=TRUE,
                  ncol=2,
                  nrow=5,
                  showPlots=TRUE,
                  verbosity=0){

    nf<-ncol(acd)-4;
    
    dfr<-acd;
    if (tolower(ci.type)=='normal'){
        stdv<-dfr$cv*dfr$value;
        dfr$lci<-qnorm((1-ci)/2,mean=dfr$value,sd=stdv);
        dfr$uci<-qnorm(1-(1-ci)/2,mean=dfr$value,sd=stdv);
    } else if (tolower(ci.type)=='lognormal'){
        cv<-dfr$cv;
        sd<-sqrt(log(1+cv*cv));
        dfr$lci<-qlnorm((1-ci)/2,meanlog=log(dfr$value),sdlog=sd);
        dfr$uci<-qlnorm(1-(1-ci)/2,meanlog=log(dfr$value),sdlog=sd);
    }
    
    if (!is.null(xlims)){
        idx <- (xlims[1]<=dfr$year)&(dfr$year<=xlims[2]);
        dfr <- dfr[idx,];
    }
    
    mxp<-nrow*ncol;
    strata<-unique(dfr$stratum);    
    npg<-ceiling(length(strata)/mxp);
    
    if (is.null(ylims)) ylims<-c(min(0,min(dfr$uci,na.rm=TRUE)),max(dfr$uci,na.rm=TRUE));
    if (verbosity>0) cat("ylims = ",ylims,'\n')
    ##cat("jitter =",jitter,"\n")
    
    fax<-'black';
    dfr$yrp <- dfr$year;
    if (nf>0) {
        facs<-names(acd)[1+(1:nf)];
        fax<-do.call(paste,c(dfr[tolower(facs)],sep=', '))
        dfr$fax<-fax;
        ulevs<-unique(fax);
        nlevs<-length(ulevs);
        jit  <- 0.2*(-((nlevs-1)/2):((nlevs-1)/2))
        names(jit)<-ulevs;
        if (jitter) {
            dfr$yrp <- dfr$year + jit[dfr$fax];
        }
    }
    
    ctr<-0;
    ps<-list();
    for (pg in 1:npg){ #loop over pages
        dfrp<-dfr[(dfr$stratum %in% strata[(pg-1)*mxp+1:mxp]),];
        p <- ggplot(data=dfrp);
        p <- p + geom_line(aes(x=yrp,y=value,colour=fax),size=1);
        p <- p + geom_errorbar(aes(x=yrp,y=value,ymin=lci,ymax=uci,colour=fax));
        p <- p + geom_point(aes(x=yrp,y=value,color=fax,shape=fax),size=2);
        p <- p + scale_x_continuous();
        p <- p + scale_y_continuous(breaks=pretty(ylims),limits=ylims,expand=c(0.01,0),oob=squish);
#        p <- p + position_jitter()
        p <- p + geom_hline(yintercept=0,colour='black',size=0.5);
        p <- p + labs(x="Year",y=ylab)
        p <- p + facet_wrap(~stratum,ncol=ncol);
        p <- p + guides(fill=guide_legend(''),colour=guide_legend(''),shape=guide_legend(''));
        p <- p + ggtheme;
        if (showPlots) print(p);
        ctr<-ctr+1;
        ps[[ctr]]<-p;
    }#pg loop
    return(ps)
}
