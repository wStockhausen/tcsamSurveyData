#'
#'@title Plot aggregated size comps from survey data as bars.
#'
#'@description Function to plot aggregated size comps from survey data as bars.
#'
#'@param zcs - dataframe from call to aggregateSizeComps(...)
#'@param byStratum - flag to plot size comps separately by stratum
#'@param multipliers - list by factor of multipliers for factor levels
#'@param zlims - z-axis range (calculated internally if NULL)
#'@param xlims - size limits (calculated internally if NULL)
#'@param ylims - year limits (calculated internally if NULL)
#'@param zlab - 'z'-axis label
#'@param ggtheme - ggplot2 theme
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
#''multipliers' is a list of lists, with each sublist has two elements, 'factors' and 'value'.
#''factors' should be a list of factor=level pairs. 'value' should be the value to multiply by. 
#' 
#'One plot is created for each distinct level of 'STRATUM'.
#'
#'@import ggplot2
#'@importFrom plyr .
#'@importFrom reshape2 dcast
#'@importFrom reshape2 melt
#'
#'@export
#'
plotAZC<-function(zcs,
                  byStratum=TRUE,
                  multipliers=NULL,
                  zlab="Abundance (millions)",
                  zlims=NULL,
                  xlims=NULL,
                  ylims=NULL,
                  ggtheme=theme_grey(),
                  ncol=2,
                  nrow=5,
                  showPlots=TRUE,
                  verbosity=0){
    ##make copy of input dataframe
    dfr<-zcs;
    
    ##place limits on size and year to plot
    if (!is.null(xlims)){
        idx<-(xlims[1]<=dfr$size)&(dfr$size<=xlims[2]);
        dfr<-dfr[idx,];
    }
    if (!is.null(ylims)){
        idx<-(ylims[1]<=dfr$year)&(dfr$year<=ylims[2]);
        dfr<-dfr[idx,];
    }

    ##multiply by requested amounts
    if (is.list(multipliers)){
        nfp<-length(multipliers);
        for (f in 1:nfp){
            mlt <- multipliers[[f]]$value;  #value to multiply by
            lst <- multipliers[[f]]$factors;#list identifying factor combination
            ##identify rows corresponding to factor combination
            idx <- TRUE;
            sfacs<-names(lst);
            for (sfac in sfacs){
                idx<-idx & (dfr[[sfac]]==lst[[sfac]]);
            }
            if (verbosity>1){
                cat('Applying ',mlt,' to ',sum(idx),' values out of ',length(idx),' for ',sep='');
                for (sfac in sfacs) cat(sfac,"==",lst[[sfac]]," ");
                cat("\n");
            }
            ##apply multiplier to value
            dfr$value[idx] <- mlt * dfr$value[idx];
        }
    }

    yrs<-unique(dfr$year);
    uz<-unique(dfr$size);
    
    if (is.null(zlims)) {
        mn<-min(dfr$value,na.rm=TRUE);
        mx<-max(dfr$value,na.rm=TRUE);
        if (mn>=0){
            zlims<-c(0,mx);
        } else {
            zlims<-max(abs(mn),abs(mx))*c(-1,1);
        }
    }
    if (verbosity>0) cat("zlims = ",zlims,'\n')
    
    fax<-'black';
    if (byStratum){
        strata<-unique(dfr$stratum);
        ncr<-4;##number of non-factor columns (including strata column)
        ncp<-1;##offset for factor columns (skips strata column)
    } else {
        strata<-'';
        ncr<-3;##number of non-factor columns
        ncp<-0;##offset for factor columns
    }
    nf<-ncol(dfr)-ncr;##number of factors
    if (nf>0) {
        facs<-names(zcs)[ncp+(1:nf)];
        fax<-do.call(paste,c(dfr[tolower(facs)],sep=', '))
        dfr$fax<-fax;
    }
    
    mxp<-nrow*ncol;
    npg<-ceiling(length(yrs)/mxp);
    nyp<-mxp*npg;
    if (length(yrs)<nyp){
        ##add some blank years to fill out pages
        newyrs<-max(yrs)+1:(nyp-length(yrs));
        if (verbosity>1) cat('newyrs=',newyrs,'\n');
        dfrnewp<-dfr[1:2,];
        dfrnew<-NULL;
        for (newyr in newyrs){
            dfrnewp$year<-newyr;
            dfrnew<-rbind(dfrnew,dfrnewp);
        }
        dfrnew$value<-0;
        yrs<-c(yrs,newyrs);
        if (verbosity>1) View(dfrnew);
        if (verbosity>1) print(yrs);
    }
    
    ctr<-0;
    ps<-list();
    for (stratum in strata){
        if (byStratum){
            dfrpp<-dfr[(dfr$stratum==stratum),];
        } else {
            dfrpp<-dfr;
        }
        dfrpp<-rbind(dfrpp,dfrnew);
        for (pg in 1:npg){ #loop over pages
            dfrp<-dfrpp[(dfrpp$year %in% yrs[(pg-1)*mxp+1:mxp]),];
            p <- ggplot(data=dfrp);
            ##p <- p + geom_line(aes(x=size,y=value,colour=fax),size=1)
            p <- p + geom_step(aes(x=size,y=value,colour=fax),size=1,direction='hv')
            p <- p + scale_x_continuous(breaks=pretty(uz)) 
            p <- p + scale_y_continuous(breaks=pretty(zlims),limits=zlims,expand=c(0.01,0),oob=squish)
            p <- p + geom_hline(yintercept=0,colour='black',size=0.5)
            p <- p + labs(x="Size (mm)",y=zlab,title=stratum)
            p <- p + facet_wrap(~year,ncol=ncol) 
            p <- p + guides(fill=guide_legend(''),colour=guide_legend(''))
            p <- p + ggtheme
            if (showPlots) print(p);
            ctr<-ctr+1;
            ps[[ctr]]<-p
        }#pg loop
    }#stratum loop
    return(ps)
}