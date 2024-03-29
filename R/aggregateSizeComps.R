#'
#'@title Aggregate size comps from survey data over factors
#'
#'@description Function to aggregate size comps from survey data over factors.
#'
#'@param zcs  dataframe from call to one of the calcSizeComps... functions
#'@param facs  factors to include in output
#'@param dropLevels  list by factor of factor levels to drop
#'@param var  variable type to export (abundance or biomass)
#'@param export   boolean flag to write results to csv file
#'@param out.csv  output file name
#'@param out.dir  output file directory
#'@param verbosity  integer flag indicating level of printed output (0=off,1=minimal,2=full)
#'
#'@return a dataframe
#'
#'@details The time series for each factor level combination is plotted
#'separately. Distinct levels of each factor can be dropped from the
#'final plot by seting dropLevels to a list with names corresponding to
#'factor columns and values being vectors of factor levels to drop.
#'
#'@importFrom plyr .
#'@importFrom reshape2 dcast
#'@importFrom reshape2 melt
#'
#'@export
#'
aggregateSizeComps<-function(zcs,
                             facs='',
                             dropLevels=NULL,
                             var=c('ABUNDANCE','BIOMASS'),
                             export=FALSE,
                             out.csv=paste('SurveyACD',var[1],'csv',sep='.'),
                             out.dir=NULL,
                             verbosity=0){
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
    mdfr<-melt(zcs,id.vars,measure.vars,factorsAsStrings=TRUE,value.name='value');

    ##drop requested factor levels
    mdfr<-wtsUtilities::dropLevels(mdfr,dropLevels);
    # if (is.list(dropLevels)){
    #     dfacs<-names(dropLevels);
    #     for (dfac in dfacs){
    #         mdfr<-mdfr[!(mdfr[[dfac]] %in% dropLevels[[dfac]]),];
    #     }
    # }

    #cast the dataframe to aggregate over missing factors
    str<-"STRATUM&&facs+YEAR+SIZE~.";
    if (nf>0){
        str<-gsub("&&facs",paste('+',facs,sep='',collapse=''),str);
    } else {
        str<-gsub("&&facs",'',str);
    }
    dfr<-dcast(mdfr,
                 str,
                 fun.aggregate=sum,
                 subset=.(variable==paste("tot",toupper(var[1]),sep='')),
                 value.var="value")
    nms<-names(dfr);
    nms<-tolower(nms);
    nms[length(nms)]<-'value';
    names(dfr)<-nms;

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
