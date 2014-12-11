#'
#'@title Reshape survey size comps dataframe to wide format.
#'
#'@description Function to reshape a survey size comps dataframe to wide format
#'
#'@param dfr  - dataframe with survey size comps from call to \code{\link{calcSizeComps.ByStratum}} or  \code{\link{calcSizeComps.EW166}} or  \code{\link{calcSizeComps.EBS}}
#'@param facs - vector of 'factor' column names (e.g., 'SEX', 'MATURITY') for output
#'@param var  - name of variable to extract ("totABUNDANCE" or "totBIOMASS")
#'@param export  - boolean flag to write results to csv file
#'@param out.csv - output file name
#'@param out.dir - output file directory 
#'@param verbosity - integer flag indicating level of printed output (0=off,1=minimal,2=full)
#'
#'@return dataframe with size comps in 'wide' format.
#'
#'@details Factors (e.g., 'SEX', 'MATURITY', 'SHELL_CONDITION') in the input dataframe that are
#'missing from the 'facs' vector will be aggregated over by summing. STRATUM is \bold{not} regarded as 
#'a potential factor.
#'
#'Output column order will be STRATUM, facs, YEAR, numStations, numIndivs, size bins
#'
#'@importFrom plyr .
#'@importFrom reshape2 dcast
#'@importFrom reshape2 melt
#'
#'@export
#'
reshapeSizeComps.wide<-function(dfr,
                                facs='',
                                var=c('totABUNDANCE','totBIOMASS'),
                                export=FALSE,
                                out.csv='SurveySizeComps.wide.csv',
                                out.dir=NULL,
                                verbosity=1){
    #determine id and measure variables for melting
    nf<-length(facs)
    if (nf>0){
        id.vars<-c("STRATUM",facs,"SIZE","YEAR","numStations");
        id.z<-2+nf;#index of SIZE column
    } else {
        id.vars<-c("STRATUM","SIZE","YEAR","numStations")
        id.z<-2;#index of SIZE column
    }
    measure.vars<-c("numIndivs",var[1]);
    
    #melt the input dataframe
    mdfr<-reshape2::melt(dfr,id.vars,measure.vars,factorsAsStrings=TRUE);
    
    #calculate the number of individuals sampled, summing over aggregated factors
    str<-"STRATUM&&facs+YEAR+numStations~.";
    if (nf>0){
        str<-gsub("&&facs",paste('+',facs,sep='',collapse=''),str);
    } else {
        str<-gsub("&&facs",'',str);
    }
    zcs.ss.wide<-reshape2::dcast(mdfr,
                                 str,
                                 fun.aggregate=sum,
                                 subset=plyr::`.`(variable=="numIndivs"),
                                 value.var="value")
    ss<-zcs.ss.wide[["."]]
    
    #calculate the abundance or biomass, summing over aggregated factors
    str<-"STRATUM&&facs+YEAR~SIZE";
    if (nf>0){
        str<-gsub("&&facs",paste('+',facs,sep='',collapse=''),str);
    } else {
        str<-gsub("&&facs",'',str);
    }
    zcs.wide<-reshape2::dcast(mdfr,
                              str,
                              fun.aggregate=sum,
                              subset=plyr::`.`(variable==var[1]),
                              value.var="value");
    
    #add in sample size column
    zcs.wide<-cbind(list(numIndivs=ss),zcs.wide)
    
    #rearrange columns to standard order
    zcs.wide<-zcs.wide[,c(2:(2+nf+1),1,(2+nf+2):ncol(zcs.wide))];
            
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
        write.csv(zcs.wide,out.csv,na='',row.names=FALSE);
    }
    
    return(zcs.wide);
}