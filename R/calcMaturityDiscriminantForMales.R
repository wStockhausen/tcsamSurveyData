#'
#'@title Calculate a discriminant value on CH/CW to classify immature vs mature males
#'
#'@description Function to calculate a discriminant value on CH/CW to classify immature vs mature males.
#'
#'@param dfr - dataframe with data to analyse for  discriminant
#'@param col - number or name of column with data to analyse for discriminant
#'@param min - minimum value of dfr[[col]] to consider
#'@param max - maximum value of dfr[[col]] to consider
#'@param binwidth - binwidth for histogram
#'
#'@return value to use as discriminant
#'
#'@details None.
#'
#'@export
#'
calcMaturityDiscriminantForMales<-function(dfr,col=1,
                                           min=0,max=max(dfr[[col]],na.rm=TRUE),
                                           binwidth=0.005){
    vals<-dfr[[col]];
    vals<-vals[(vals>=min)&(vals<=max)];
    brks<-seq(from=min,to=max,by=binwidth);
    hst<-graphics::hist(vals,breaks=brks,freq=TRUE,include.lowest=FALSE,plot=FALSE);

    counts<-hst$counts;
    np<-length(counts);
    d1<-(counts[2:np]-counts[1:(np-1)]);
    d2<-c(0,(d1[2:(np-1)]-d1[1:(np-2)]),0);#2nd deriv.s
    d1<-c(0,(d1[2:(np-1)]+d1[1:(np-2)])/2,0);        #first deriv.s


    plot(lowess(hst$mids,hst$counts))
    plot(lowess(hst$mids,hst$counts,f=0.1))
    plot(lowess(hst$mids,hst$counts,f=0.2))
    plot(lowess(hst$mids,hst$counts,f=0.1))

    plot(spline(hst$mids,hst$counts))
    sp<-spline(hst$mids,hst$counts)
    counts<-sp$y
    np<-length(counts);
        d1<-(counts[2:np]-counts[1:(np-1)]);
        d2<-c(0,(d1[2:(np-1)]-d1[1:(np-2)]),0);   #2nd deriv.s
        d1<-c(0,(d1[2:(np-1)]+d1[1:(np-2)])/2,0); #first deriv.s
    plot(d1,d2)
    text(d1,d2,as.character(brks))
    idx<-(d2<0)&(abs(d1)<0.05*max(abs(d1)))
    text(d1[idx],d2[idx],as.character(brks[idx]),col="red")
    plot(d1[idx],d2[idx])
    text(d1[idx],d2[idx],as.character(brks[idx]),col="red")
    plot(d1[idx],d2[idx])
    text(d1[idx],d2[idx],as.character(sp$x[idx]),col="red")
    text(d1[idx],d2[idx],as.character(round(1000*sp$x[idx])/1000),col="red")
    plot(d1[idx],d2[idx])
    text(d1[idx],d2[idx],as.character(round(1000*sp$x[idx])/1000),col="red")

    return(dsc);
}
