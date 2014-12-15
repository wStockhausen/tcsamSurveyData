#'
#'@title Plot time series of aggregated catch data.
#'
#'@description Function to plot time series of aggregated catch data.
#'
#'@param acd - dataframe from call to one of the calcBiomass... functions
#'@param facs - factors to plot by 
#'@param dropLevels - factor levels to drop from plots
#'@param var - variable type to plot (abundance or biomass)
#'@param ci - confidence interval for error bars (e.g., 0.95)
#'@param ci.type - confidence interval type ('normal', 'lognormal')
#'@param rng - y-axis range (calculated internally if NULL)
#'@param ggtheme - ggplot2 theme
#'@param ncol - number of columns of plots per page
#'@param nrow - number of rows of plots per page
#'@param showPlots - flag to show plots immediately
#'
#'@return list of ggplot2 objects
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
#'
#'@export
#'
plotAggregatedCatchData.ByStratum<-function(acd,
                                            facs='',
                                            dropLevels=NULL,
                                            var=c('ABUNDANCE','BIOMASS'),
                                            ci=0.8,
                                            ci.type='normal',
                                            rng=NULL,
                                            ggtheme=theme_grey(),
                                            ncol=2,
                                            nrow=5,
                                            showPlots=TRUE){
    #determine data type to plot
    if (toupper(var[1])=='ABUNDANCE'){
        ylab<-'abundance';
        acd$var<-acd$stdABUNDANCE*acd$stdABUNDANCE;
    } else if (toupper(var[1])=='BIOMASS'){
        ylab<-'biomass';
        acd$var<-acd$stdBIOMASS*acd$stdBIOMASS;
    } else {
        cat('Error in plotAggregatedCatchData.\n');
        cat("unrecognized var = '",var[1],"'.\n");
        cat("Exiting function\n");
        return(NULL);
    }
    vars<-c(paste("tot",toupper(var[1]),sep=''),'var');
    
    #determine id and measure variables for melting
    nf<-length(facs)
    if (nf>0){
        id.vars<-c("STRATUM",facs,"YEAR");
    } else {
        id.vars<-c("STRATUM","YEAR")
    }
    measure.vars<-vars;
    
    #melt the input dataframe
    mdfr<-reshape2::melt(acd,id.vars,measure.vars,factorsAsStrings=TRUE,value.name='value');
    
    #drop requested factor levels
    if (is.list(dropLevels)){
        dfacs<-names(dropLevels);
        for (dfac in dfacs){
            mdfr<-mdfr[!(mdfr[[dfac]] %in% dropLevels[[dfac]]),];
        }
    }
    
    #cast the dataframe to aggregate the value over unspecified factors
    str<-"STRATUM&&facs+YEAR~.";
    if (nf>0){
        str<-gsub("&&facs",paste('+',facs,sep='',collapse=''),str);
    } else {
        str<-gsub("&&facs",'',str);
    }
    dfr<-reshape2::dcast(mdfr,
                         str,
                         fun.aggregate=sum,
                         subset=plyr::`.`(variable==vars[1]),
                         value.var="value")
    nms<-names(dfr);
    nms<-tolower(nms);
    nms[length(nms)]<-'value';
    names(dfr)<-nms;
    
    #cast the dataframe to aggregate the associated variance over unspecified factors
    str<-"STRATUM&&facs+YEAR~.";
    if (nf>0){
        str<-gsub("&&facs",paste('+',facs,sep='',collapse=''),str);
    } else {
        str<-gsub("&&facs",'',str);
    }
    vdfr<-reshape2::dcast(mdfr,
                         str,
                         fun.aggregate=sum,
                         subset=plyr::`.`(variable==vars[2]),
                         value.var="value")
    nms<-names(vdfr);
    nms<-tolower(nms);
    nms[length(nms)]<-'var';
    names(vdfr)<-nms;
    
    dfr$stdv<-sqrt(vdfr$var);
    if (tolower(ci.type)=='normal'){
        dfr$lci<-qnorm((1-ci)/2,mean=dfr$value,sd=dfr$stdv);
        dfr$uci<-qnorm(1-(1-ci)/2,mean=dfr$value,sd=dfr$stdv);
    } else if (tolower(ci.type)=='lognormal'){
        cv<-dfr$stdv/dfr$value;
        sd<-sqrt(log(1+cv*cv));
        dfr$lci<-qlnorm((1-ci)/2,mean=log(dfr$value),sd=sd);
        dfr$uci<-qlnorm(1-(1-ci)/2,mean=log(dfr$value),sd=sd);
    }
    
    mxp<-nrow*ncol;
    strata<-unique(dfr$stratum);    
    npg<-ceiling(length(strata)/mxp)
    
    if (is.null(rng)) rng<-c(min(0,min(dfr$uci,na.rm=TRUE)),max(dfr$uci,na.rm=TRUE));
    cat("rng = ",rng,'\n')
    
    fax<-'black';
    if (nf>0) {
        fax<-do.call(paste,c(dfr[tolower(facs)],sep=', '))
        dfr$fax<-fax;
        ulevs<-unique(fax);
        nlevs<-length(ulevs);
        jit  <- 0.2*(-((nlevs-1)/2):((nlevs-1)/2))
        names(jit)<-ulevs;
        dfr$yrp <- dfr$year + jit[dfr$fax];
    }
    
    ctr<-0;
    ps<-list();
    for (pg in 1:npg){ #loop over pages
        dfrp<-dfr[(dfr$stratum %in% strata[(pg-1)*mxp+1:mxp]),]
        p <- ggplot(data=dfrp)
        p <- p + geom_line(aes(x=yrp,y=value,colour=fax),size=1)
        p <- p + geom_errorbar(aes(x=yrp,y=value,ymin=lci,ymax=uci,colour=fax))
        p <- p + geom_point(aes(x=yrp,y=value,color=fax,shape=fax),size=2)
        p <- p + scale_x_continuous() 
        p <- p + scale_y_continuous(breaks=pretty(rng),limits=rng,expand=c(0.01,0),oob=scales::squish)
#        p <- p + position_jitter()
        p <- p + geom_hline(yintercept=0,colour='black',size=0.5)
        p <- p + labs(x="Year",y=ylab)
        p <- p + facet_wrap(~stratum,ncol=ncol) 
        p <- p + guides(fill=guide_legend(''),colour=guide_legend(''),shape=guide_legend(''))
        p <- p + ggtheme
        if (showPlots) print(p);
        ctr<-ctr+1;
        ps[[ctr]]<-p
    }#pg loop
    return(ps)
}