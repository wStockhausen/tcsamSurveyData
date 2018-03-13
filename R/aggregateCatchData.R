#'
#'@title Export aggregated catch data to stock assessment format
#'
#'@description Function to export aggregated catch data to stock assessment format.
#'
#'@param tbl - dataframe from call to one of the calcBiomass... functions
#'@param facs - factors to include in output
#'@param dropLevels - factor levels to drop
#'@param var - variable type to export (abundance or biomass)
#'@param export  - boolean flag to write results to csv file
#'@param out.csv - output file name
#'@param out.dir - output file directory
#'@param verbosity - integer flag indicating level of printed output (0=off,1=minimal,2=full)
#'
#'@return dataframe in stock assessment format
#'
#'@details Factors (e.g., 'SEX', 'MATURITY', 'SHELL_CONDITION') in the input dataframe that are
#'missing from the 'facs' vector will be aggregated over by summing. STRATUM is \bold{not} regarded as
#'a potential factor. The time series for each factor level combination is exported.
#'Distinct levels of each factor can be dropped from the
#'exported dataframe by seting dropLevels to a list with names corresponding to
#'factor columns and values being vectors of factor levels to drop.
#'
#'Output column order will be STRATUM, facs, YEAR, value, cv
#'
#'@importFrom plyr .
#'@importFrom reshape2 dcast
#'@importFrom reshape2 melt
#'
#'@export
#'
aggregateCatchData<-function(tbl,
                             facs='',
                             dropLevels=NULL,
                             var=c('ABUNDANCE','BIOMASS'),
                             export=FALSE,
                             out.csv=paste('SurveyACD',var[1],'csv',sep='.'),
                             out.dir=NULL,
                             verbosity=0){
    #determine data type to plot
    if (toupper(var[1])=='ABUNDANCE'){
        ylab<-'abundance';
        tbl$var<-tbl$stdABUNDANCE*tbl$stdABUNDANCE;
    } else if (toupper(var[1])=='BIOMASS'){
        ylab<-'biomass';
        tbl$var<-tbl$stdBIOMASS*tbl$stdBIOMASS;
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
        for (fac in facs){
            if (!(fac %in% names(tbl))){
                tbl[[fac]]<-"ALL";
            }
        }
        id.vars<-c("STRATUM",facs,"YEAR");
    } else {
        id.vars<-c("STRATUM","YEAR")
    }
    measure.vars<-vars;

    #melt the input dataframe
    mdfr<-melt(tbl,id.vars,measure.vars,factorsAsStrings=TRUE,value.name='value');

    #drop requested factor levels
    mdfr<-wtsUtilities::dropLevels(mdfr,dropLevels);
    # if (is.list(dropLevels)){
    #     dfacs<-names(dropLevels);
    #     for (dfac in dfacs){
    #         mdfr<-mdfr[!(mdfr[[dfac]] %in% dropLevels[[dfac]]),];
    #     }
    # }

    #cast the dataframe to aggregate the value over unspecified factors
    str<-"STRATUM&&facs+YEAR~.";
    if (nf>0){
        str<-gsub("&&facs",paste('+',facs,sep='',collapse=''),str);
    } else {
        str<-gsub("&&facs",'',str);
    }
    dfr<-dcast(mdfr,
               str,
               fun.aggregate=sum,
               subset=.(variable==vars[1]),
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
    vdfr<-dcast(mdfr,
                str,
                fun.aggregate=sum,
                subset=.(variable==vars[2]),
                value.var="value")
    nms<-names(vdfr);
    nms<-tolower(nms);
    nms[length(nms)]<-'var';
    names(vdfr)<-nms;

    dfr$cv<-sqrt(vdfr$var)/dfr$value;

    if (export){
        if (!is.null(out.dir)){
            if (verbosity>1) cat("\nTesting existence of folder '",out.dir,"'\n",sep='')
            if (!file.exists(out.dir)){
                if (verbosity>0) cat("Creating folder '",out.dir,"' for output.\n",sep='')
                dir.create(out.dir);
            } else {
                if (verbosity>0) cat("Using folder '",out.dir,"' for output.\n",sep='')
            }
            out.csv<-file.path(out.dir,out.csv)
        }
        write.csv(dfr,out.csv,na='',row.names=FALSE);
    }

    return(dfr);
}
