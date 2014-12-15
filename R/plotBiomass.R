plotSizeComps.asLines<-function(zcs,
                                facs='',
                                dropLevels=NULL,
                                var=c('totABUNDANCE','totBIOMASS'),
                                rng=NULL,
                                ggtheme=theme_grey(),
                                ncol=2,
                                nrow=5,
                                showPlots=TRUE){
    #determine data type
    if (var[1]=='totABUNDANCE'){
        ylab<-'abundance';
    } else if (var[1]=='totABUNDANCE'){
        ylab<-'biomass';
    }
    
    #determine id and measure variables for melting
    nf<-length(facs)
    if (nf>0){
        id.vars<-c("STRATUM",facs,"YEAR","SIZE");
        id.z<-3+nf;#index of SIZE column
    } else {
        id.vars<-c("STRATUM","YEAR","SIZE")
        id.z<-3;#index of SIZE column
    }
    measure.vars<-var[1];
    
    #melt the input dataframe
    mdfr<-reshape2::melt(zcs,id.vars,measure.vars,factorsAsStrings=TRUE,value.name='value');
    
    #drop requested factor levels
    if (is.list(dropLevels)){
        dfacs<-names(dropLevels);
        for (dfac in dfacs){
            mdfr<-mdfr[!(mdfr[[dfac]] %in% dropLevels[[dfac]]),];
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
                         subset=plyr::`.`(variable==var),
                         value.var="value")
    nms<-names(dfr);
    nms<-tolower(nms);
    nms[length(nms)]<-'value';
    names(dfr)<-nms;
    
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
    for (pg in 1:npg){ #loop over pages
        dfrp<-dfr[(dfr$year %in% yrs[(pg-1)*mxp+1:mxp]),]
        p <- ggplot(data=dfrp)
        p <- p + geom_line(aes(x=size,y=value,colour=fax),size=1)
        p <- p + scale_x_continuous(breaks=pretty(uz)) 
        p <- p + scale_y_continuous(breaks=pretty(rng),limits=rng,expand=c(0.01,0),oob=scales::squish)
        p <- p + geom_hline(yintercept=0,colour='black',size=0.5)
        p <- p + labs(x="Size (mm)",y=ylab)
        p <- p + facet_wrap(~year,ncol=ncol) 
        p <- p + guides(fill=guide_legend(''),colour=guide_legend(''))
        p <- p + ggtheme
        if (showPlots) print(p);
        ctr<-ctr+1;
        ps[[ctr]]<-p
    }
    return(ps)
}