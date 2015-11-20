#'
#'@title Plot size comps from survey data as bars.
#'
#'@description Function to plot size comps from survey data as bars.
#'
#'@param zcs - dataframe from call to one of the calcSizeComps... functions
#'@param facs - factors to plot by 
#'@param dropLevels - list by factor of factor levels to drop from plots
#'@param multipliers - list by factor of multipliers for factor levels
#'@param var - variable type to plot (abundance or biomass)
#'@param rng - y-axis range (calculated internally if NULL)
#'@param ggtheme - ggplot2 theme
#'@param ncol - number of columns of plots per page
#'@param nrow - number of rows of plots per page
#'@param showPlots - flag to show plots immediately
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
plotSizeComps.ByStratum<-function(zcs,
                                 facs='',
                                 dropLevels=NULL,
                                 multipliers=NULL,
                                 var=c('ABUNDANCE','BIOMASS'),
                                 rng=NULL,
                                 ggtheme=theme_grey(),
                                 ncol=2,
                                 nrow=5,
                                 showPlots=TRUE){
    ##determine data type to plot
    if (toupper(var[1])=='ABUNDANCE'){
        ylab<-'abundance';
    } else if (toupper(var[1])=='BIOMASS'){
        ylab<-'biomass';
    } else {
        cat('Error in plotSizeComps.ByStratum.\n');
        cat("unrecognized var = '",var[1],"'.\n");
        cat("Exiting function\n");
        return(NULL);
    }
    
    ##determine id and measure variables for melting
    nf<-length(facs)
    if (nf>0){
        id.vars<-c("STRATUM",facs,"YEAR","SIZE");
        id.z<-3+nf;#index of SIZE column
    } else {
        id.vars<-c("STRATUM","YEAR","SIZE")
        id.z<-3;#index of SIZE column
    }
    measure.vars<-paste("tot",toupper(var[1]),sep='');
    
    ##melt the input dataframe
    mdfr<-reshape2::melt(zcs,id.vars,measure.vars,factorsAsStrings=TRUE,value.name='value');
    
    ##drop requested factor levels
    if (is.list(dropLevels)){
        dfacs<-names(dropLevels);
        for (dfac in dfacs){
            mdfr<-mdfr[!(mdfr[[dfac]] %in% dropLevels[[dfac]]),];
        }
    }
    
    ##multiply by requested amounts
    if (is.list(multipliers)){
        mfacs<-names(multipliers);
        for (mfac in mfacs){
            mlt <- multipliers[[mfac]]$value;  #value to multiply by
            lst <- multipliers[[mfac]]$factors;#list identifying factor combination
            ##identify rows corresponding to factor combination
            idx <- TRUE;
            sfacs<-names(lst);
            for (sfac in sfacs){
                idx<-idx & (mdfr[[sfac]]==lst[[sfac]]);
            }
            ##apply multiplier to variables
            for (vr in var) mdfr[[vr]][idx] <- mlt * mdfr[[vr]][idx];
        }
    }
    
    #cast the dataframe to aggregate over missing factors
    str<-"STRATUM&&facs+YEAR+SIZE~.";
    if (nf>0){
        str<-gsub("&&facs",paste('+',facs,sep='',collapse=''),str);
    } else {
        str<-gsub("&&facs",'',str);
    }
    dfr<-reshape2::dcast(mdfr,
                         str,
                         fun.aggregate=sum,
                         subset=plyr::`.`(variable==paste("tot",toupper(var[1]),sep='')),
                         value.var="value")
    nms<-names(dfr);
    nms<-tolower(nms);
    nms[length(nms)]<-'value';
    names(dfr)<-nms;
    
    strata<-unique(dfr$stratum);
    yrs<-unique(dfr$year);
    uz<-unique(dfr$size);
    
    mxp<-nrow*ncol;
    npg<-ceiling(length(yrs)/mxp)
    
    if (is.null(rng)) rng<-c(min(0,min(dfr$value,na.rm=TRUE)),max(dfr$value,na.rm=TRUE));
    cat("rng = ",rng,'\n')
    
    fax<-'black';
    if (nf>0) {
        fax<-do.call(paste,c(dfr[tolower(facs)],sep=', '))
        dfr$fax<-fax;
    }
    
    ctr<-0;
    ps<-list();
    for (stratum in strata){
        for (pg in 1:npg){ #loop over pages
            dfrp<-dfr[(dfr$year %in% yrs[(pg-1)*mxp+1:mxp])&(dfr$stratum==stratum),]
            p <- ggplot(data=dfrp)
            p <- p + geom_line(aes(x=size,y=value,colour=fax),size=1)
            p <- p + scale_x_continuous(breaks=pretty(uz)) 
            p <- p + scale_y_continuous(breaks=pretty(rng),limits=rng,expand=c(0.01,0),oob=scales::squish)
            p <- p + geom_hline(yintercept=0,colour='black',size=0.5)
            p <- p + labs(x="Size (mm)",y=ylab,title=stratum)
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