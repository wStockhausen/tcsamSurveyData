#'
#'@title Plot spatial maps of cpue by year, survey haul or station and other factors from cpue info.
#'
#'@description Function to map cpue by survey haul or station and other factors (e.g., sex) from cpue info.
#'
#'@param tbl_cpue  : dataframe from call to \code{\link{calcCPUE.ByHaul} or \code{\link{calcCPUE.ByStation}}} (or crab cpue filename, or NULL)
#'@param years  : vector of survey years for which to produce maps
#'@param ztype  : the data type (numCPUE, wgtCPUE to map)
#'@param scaleBy : factor to scale the ztype by BEFORE sending to plotMap.CSV
#'@param zlab    : label for z (cpue) axes on map
#'@param zunits  : label for z (cpue) units on maps
#'@param zscl    : see Details
#'@param basename  : base name for output files (year and factor level combinations will be added)
#'@param verbosity : integer flag indicating level of printed output (0=off,1=minimal,2=full)
#'
#'@return the max zscale, or a named list (names corresponding to individual factor combinations)
#'of max zscales that were used for plots. This value can be used to scale all maps 
#'(if a single numeric value) or maps corresponding to a given factor combination (if
#'a named list) when running the function on the same cpue values a second time.
#'
#'@details Needs to filled in. \cr
#'Note: Multiple values at the same location will be summed prior to being mapped.
#'\cr
#'Note: zscl can be NULL, a numeric value, or a named list with numeric values as 
#'list elements. If NULL, then the max z value for the data plotted on an individual
#'map will be used to scale that map. If a numeric value, then it is used as the 
#'z-axis scale for all maps produced. If a named list, the element corresponding to 
#'each combination of factors will be used as the z-axis scale for that set of maps.
#'
#' @import sqldf
#' @importFrom wtsGMT plotMap.CSV
#' @importFrom wtsUtilities selectFile
#'      
#'@export
#'
plotMaps.CPUE<-function(tbl_cpue,
                        years=NULL,
                        ztype='numCPUE',
                        scaleBy=1,
                        title='Survey',
                        zlab='no. crab',
                        zunits='num/nm@+2@+',
                        zscl=NULL,
                        xyrng='180/205/54/62',
                        rotate=170,
                        elev=45,
                        delx=0.5,
                        dely=0.25,
                        blocktype=c('MEAN','SUM'),
                        plt_blocktype=c('SMOOTH','COARSE'),
                        logtr=FALSE,
                        plt_title=FALSE,
                        plt_bars=TRUE,
                        plt_surface=FALSE,
                        plt_blocklocations=plt_surface,
                        plt_colorscale=plt_surface|plt_bars,
                        plt_stations=TRUE,
                        plt_reflines=TRUE,
                        reflines=list(list(lon=-166+0*seq(from=50,to=80,by=1),lat=seq(from=50,to=80,by=1))),
                        bathymetryFile='/Users/WilliamStockhausen/Programming/R/GitPackages/wtsGMT/data/depthcontour_200500.prn',
                        basename='SurveyMaps',
                        cleanup=TRUE,
                        verbosity=1){

    #figure out whether tbl_cpue is by haul or by station
    cols<-names(tbl_cpue); #column names
    nc<-length(cols);      #total number of columns in tbl_cpue
    type.cpue<-'STATION';  #default type is STATION
    ns<-5; #number of standard columns
    nd<-5; #number of data columns
    if (any(cols=='HAULJOIN')) {
        type.cpue<-'HAUL';
        ns<-6; #number of standard columns
        nd<-3; #number of data columns
    }
    nf<-nc-(ns+nd);#number of factor columns for cpue by station
    
    #determine years, if necessary
    if (is.null(years)){
        qry<-"select distinct YEAR from tbl_cpue order by YEAR;"
        tbl_years<-sqldf(qry);
        years<-tbl_years$YEARS;
    }
    
    #determine factor levels
    if (nf>0){
        facs<-cols[(ns+1):(nc-nd)];
        qry<-"select distinct &&facs from tbl_cpue order by &&facs";
        qry<-gsub("&&facs",paste(facs,sep='',collapse=','),qry)
        if (verbosity>1) cat("\nquery is:\n",qry,"\n");
        tbl_ufacs<-sqldf(qry);
        cat("Unique factor level combinations that could be plotted:\n")
        print(tbl_ufacs);
    }
    
    #plot the maps
    if (nf==0){
        #no factor levels to plot by
        base.ps<-basename;
        psFiles<-vector(mode='character',length=0);
        zscls<-vector(mode='numeric',length=0);
        for (y in years){
            yrstr<-as.character(y);
            idx<-tbl_cpue$YEAR==y;
            tblp<-tbl_cpue[idx,c('LONGITUDE','LATITUDE',ztype)];
            cat("Plotting map for ",yrstr,'\n')
            print(tblp);
            if (nrow(tblp)>0){
                psFile<-paste(base.ps,yrstr,sep='.');
                zsclp<-wtsGMT::plotMap.CSV(dfr=tblp,
                                            lat='LATITUDE',
                                            lon='LONGITUDE',
                                            col=ztype,zunits=zunits,zlab=zlab,
                                            zscl=zscl,
                                            title=title,year=yrstr,
                                            rotate=rotate,elev=elev,
                                            plt_bars=plt_bars,
                                            plt_surface=plt_surface,delx=delx,dely=dely,
                                            blocktype=blocktype,plt_blocktype=plt_blocktype,
                                            plt_blocklocations=plt_blocklocations,
                                            plt_stations=plt_stations,
                                            plt_reflines=plt_reflines,
                                            bathymetryFile=bathymetryFile,
                                            psFile=psFile,
                                            cleanup=cleanup);
                zscls<-c(zscls,zsclp);
                psFiles<-c(psFiles,paste(psFile,'.ps',sep=''));
            }
        }
        wtsGMT::createPDF.fromPS(base.ps,psFiles=psFiles)
        if (cleanup){file.remove(psFiles);}
        return(max(zscls,na.rm=TRUE));
    } else {
        nrw<-nrow(tbl_ufacs);
        lst.zscls<-vector(mode='list',length=0)
        for (rw in 1:nrw){
            ufac<-tbl_ufacs[rw,];
            if (!is.data.frame(ufac)){
                cat("ufac is not a dataframe!\n");
                ufac<-data.frame(ufac,stringsAsFactors=FALSE);
                names(ufac)<-facs[1];
            }
            cat("ufac = \n"); print(ufac);
            cat("plotting maps for factor levels\n")
            facstr<-paste(facs[1],'=',ufac[[facs[1]]],sep='');
            if (nf>1) {for (i in 2:nf) {facstr<-paste(facstr,paste(facs[i],'=',ufac[[facs[i]]],sep=''),sep=',');}}
            cat("\t",facstr,"\n")
            qry<-"select 
                    &&cols
                  from 
                    tbl_cpue c, ufac u
                  where
                    &&whr;"
            qry<-gsub("&&cols",paste('c.',cols,sep='',collapse=','),qry)
            qry<-gsub("&&whr",paste('c.',facs,'=','u.',facs,sep='',collapse=' AND '),qry)
            tbl<-sqldf(qry);
            if (nrow(tbl)>0){
                base.ps<-basename;
                for (fac in facs) base.ps<-paste(base.ps,paste(fac,'=',ufac[[fac]],sep=''),sep='.');
                psFiles<-vector(mode='character',length=0);
                zscls<-vector(mode='numeric',length=0);
                for (y in years){
                    yrstr<-as.character(y);
                    idx<-tbl$YEAR==y;
                    tblp<-tbl[idx,c('LONGITUDE','LATITUDE',ztype)];
                    if ((nrow(tblp)>0)&&sum(abs(tblp[[ztype]])>0)){
                        cat("Plotting map for ",yrstr,'\n')
                        #sum ztype data at each location, in case of multiple values
                        qry<-"select 
                                LONGITUDE,LATITUDE,
                                sum(&&ztype) as ZDATA
                              from tblp
                              group by LONGITUDE, LATITUDE;"
                        qry<-gsub("&&ztype",ztype,qry);
                        tblp<-sqldf(qry)
                        print(tblp);
                        psFile<-paste(base.ps,yrstr,sep='.');
                        zsclv<-zscl;
                        if (is.list(zscl)){zsclv<-zscl[[facstr]];}
                        cat("Using zscl = ",'\n'); print(zsclv);
                        zsclp<-wtsGMT::plotMap.CSV(dfr=tblp,
                                                    lat='LATITUDE',
                                                    lon='LONGITUDE',
                                                    col='ZDATA',zunits=zunits,zlab=zlab,zscl=zsclv,
                                                    title=title,year=yrstr,
                                                    rotate=rotate,elev=elev,
                                                    plt_bars=plt_bars,
                                                    plt_surface=plt_surface,delx=delx,dely=dely,
                                                    blocktype=blocktype,plt_blocktype=plt_blocktype,
                                                    plt_blocklocations=plt_blocklocations,
                                                    plt_stations=plt_stations,
                                                    plt_reflines=plt_reflines,
                                                    bathymetryFile=bathymetryFile,
                                                    psFile=psFile,
                                                    cleanup=cleanup);
                        zscls<-c(zscls,zsclp);
                        psFiles<-c(psFiles,paste(psFile,'.ps',sep=''));
                    } #nrow(tblp)>0
                } #loop over years
                if (length(psFiles)>0){
                    wtsGMT::createPDF.fromPS(base.ps,psFiles=psFiles)
                    if (cleanup){file.remove(psFiles);}
                    lst.zscls[[facstr]]<-max(zscls,na.rm=TRUE);
                }
            } #nrow(tbl)>0
        } #loop over ufacs
        return(lst.zscls);
    }
}
