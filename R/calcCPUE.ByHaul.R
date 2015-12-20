#'
#'@title Calculate cpue by survey haul and other factors from station, haul and individual crab info.
#'
#'@description Function to calculate cpue by survey haul and other factors (e.g., sex) from station, haul and individual crab info.
#'
#'@param tbl_hauls   : dataframe from call to \code{\link{selectHauls.TrawlSurvey}} [required]
#'@param tbl_indivs  : dataframe from call to \code{\link{selectIndivs.TrawlSurvey}} (or crab survey filename, or NULL)
#'@param  bySex            : flag (T/F) to calc by sex
#'@param  byShellCondition : flag (T/F) to calc by shell condition
#'@param  byMaturity       : flag (T/F) to calc by maturity state
#'@param  bySize        : flag (T/F) to calc by size
#'@param  cutpts        : vector of cutpoints to create size bins from
#'@param  truncate.low  : flag (T/F) to exclude individuals smaller than minSize
#'@param  truncate.high : flag (T/F) to exclude individuals larger than maxSize
#'@param export  : boolean flag to write results to csv file
#'@param out.csv : output file name
#'@param out.dir : output file directory 
#'@param verbosity : integer flag indicating level of printed output (0=off,1=minimal,2=full)
#'
#'@details
#'Note: tbl_hauls is required. if tbl_indivs is a dataframe, it is used. If it is a character vector, it is
#'interpreted as a file name and it is read as a csv file. If it is NULL, the user is prompted to choose a file
#'to read. \cr
#'The returned dataframe has the following columns: \cr
#'\itemize{
#'\item   YEAR
#'\item   STRATUM
#'\item   GIS_STATION
#'\item   HAULJOIN
#'\item   LONGITUDE
#'\item   LATITUDE
#'\item   requested factors, if any
#'\item   numIndivs
#'\item   numCPUE
#'\item   wgtCPUE
#'}
#'\cr Other notes: \cr
#'\itemize{
#'\item   CPUE in numbers is in no/(sq. nm.)
#'\item   CPUE in weight  is in mt/(sq. nm.)
#'} \cr
#'
#'@return A dataframe of cpue (numbers and weight) by haul. See Details.
#'
#' @importFrom sqldf sqldf
#' @importFrom wtsUtilities selectFile
#'      
#'@export
#'
calcCPUE.ByHaul<-function(tbl_hauls,
                          tbl_indivs=NULL,
                          bySex=FALSE,
                          byShellCondition=FALSE,
                          byMaturity=FALSE,
                          bySize=FALSE,
                          cutpts=seq(from=25,to=185,by=5),
                          truncate.low=TRUE,
                          truncate.high=FALSE,
                          export=FALSE,
                          out.csv='cpue.ByHaul.csv',
                          out.dir=NULL,
                          verbosity=1){
    if (verbosity>0) cat("starting calcCPUE.ByHaul\n");
    
    
    if (!is.data.frame(tbl_hauls)) {
        cat("Error in calcCPUE.ByHaul:",
            "tbl_hauls is NULL. Must supply tbl_hauls.",
            "Aborting...",sep='\n');
        return(NULL);
    }
    
    in.csv<-NULL;
    if (!is.data.frame(tbl_indivs)){
        if (!is.character(tbl_indivs)) {
            in.csv<-selectFile(ext="csv",caption="Select AFSC crab trawl survey file");
            if (is.null(in.csv)|(in.csv=='')) return(NULL);
        } else {
            in.csv<-tbl_indivs;#tbl is a filename
        }
        if (verbosity>1) cat("Reading AFSC crab trawl survey csv file for individual crab info.\n",sep='')
        tbl<-read.csv(in.csv,stringsAsFactors=FALSE);
        if (verbosity>1) cat("Done reading input csv file.\n")
    }
    
    if (is.null(out.dir)) {
        out.dir<-dirname(file.path('.'));
        if (!is.null(in.csv)) {out.dir<-dirname(file.path(in.csv));}
    }
    if (verbosity>0) cat("Output directory for calcCPUE.ByHaul will be '",out.dir,"'\n",sep='');
    
    #make some shorter variables
    bySx<-bySex;bySC<-byShellCondition;byMt<-byMaturity;bySz<-bySize;
    
    #define a function for substituting query conditions
    subst.cond<-function(qry,byCond,col,lbl){
        if (byCond) {
            qry<-gsub(paste("&&",lbl,sep=''),paste(col,",",sep=''),qry);    
            qry<-gsub(paste("&&by",lbl,sep=''),paste(",",col,sep=''),qry);    
        } else {
            qry<-gsub(paste("&&",lbl,sep=''),'',qry);    
            qry<-gsub(paste("&&by",lbl,sep=''),'',qry);    
        }
        return(qry);
    }
    
    if (bySize){
      #expand cutpts to truncate or not
      nCtPts<-length(cutpts);
      ctpts.tmp<-cutpts;
      if (!truncate.low ) ctpts.tmp[1]<-0;
      if (!truncate.high) ctpts.tmp[nCtPts]<-Inf;
      #apply cutpts to sizes
      cuts<-cut(tbl_indivs$SIZE,ctpts.tmp,right=FALSE,labels=FALSE)
      tbl_indivs$SIZE<-cutpts[cuts];
    }
    
    #Calculate and sum cpue by year, haul, station and factor levels (e.g., sex, shell condition)
    #over individuals for hauls w/ nonzero catches.
    #Note that this DOES NOT average over hauls, as calcCPUE.ByStation(...) does.
    qry<-"select
            HAULJOIN,
            &&Sx&&SC&&Mt&&Sz
            sum(numIndivs) as numIndivs,
            sum(SAMPLING_FACTOR) as expNUM,
            sum(SAMPLING_FACTOR*CALCULATED_WEIGHT) as expWGT
          from
            tbl_indivs
          group by
            HAULJOIN&&bySx&&bySC&&byMt&&bySz
          order by
            HAULJOIN&&bySx&&bySC&&byMt&&bySz;";
    qry<-subst.cond(qry,bySx,"SEX",            'Sx');
    qry<-subst.cond(qry,bySC,"SHELL_CONDITION",'SC');
    qry<-subst.cond(qry,byMt,"MATURITY",       'Mt');
    qry<-subst.cond(qry,bySz,"SIZE",           'Sz');
    if (verbosity>1) cat("\nquery is:\n",qry,"\n");
    tbl_sums<-sqldf(qry);
    
    #create table of years,strata,stations,hauls x uniq "factors" (e.g., sex, shell condition,...)
    if (bySx|bySC|byMt|bySz){
        cols<-paste(subst.cond("&&col",bySx,"SEX","col"),
                    subst.cond("&&col",bySC,"SHELL_CONDITION","col"),
                    subst.cond("&&col",byMt,"MATURITY","col"),
                    subst.cond("&&col",bySz,"SIZE","col"),
                    sep='');
        qry<-"select distinct
                &&cols
                1 as DUMMY
              from
                tbl_sums as s
              order by
                &&colsDUMMY;";
        qry<-gsub("&&cols",cols,qry);
        if (verbosity>1) cat("\nquery is:\n",qry,"\n");
        tbl_ufctrs<-sqldf(qry);
        
        qry<-"select *
              from 
                (select 
                    YEAR,STRATUM,GIS_STATION,HAULJOIN,
                    1*MID_LONGITUDE as LONGITUDE,
                    1*MID_LATITUDE as LATITUDE,
                    AREA_SWEPT_VARIABLE from tbl_hauls),
                tbl_ufctrs;"
        tbl_uhfs<-sqldf(qry);
    } else {
        qry<-"select 
                 YEAR,STRATUM,GIS_STATION,HAULJOIN,
                 1*MID_LONGITUDE as LONGITUDE,
                 1*MID_LATITUDE as LATITUDE,
                 AREA_SWEPT_VARIABLE from tbl_hauls;"
        tbl_uhfs<-sqldf(qry);
    }
    
    #expand sums to hauls w/ zero catches and calculate cpue
    cols<-'u.YEAR,
           u.STRATUM,
           u.GIS_STATION,
           u.HAULJOIN,
           u.LONGITUDE,
           u.LATITUDE';
    if (bySx) cols<-paste(cols,',u.SEX',            sep='')
    if (bySC) cols<-paste(cols,',u.SHELL_CONDITION',sep='')
    if (byMt) cols<-paste(cols,',u.MATURITY',       sep='')
    if (bySz) cols<-paste(cols,',u.SIZE',           sep='')
    qry<-"select
            &&cols,
            numIndivs,
            expNum/AREA_SWEPT_VARIABLE as numCPUE,
            expWgt/AREA_SWEPT_VARIABLE as wgtCPUE
          from
            tbl_uhfs as u left join
            tbl_sums as s
          on
            u.HAULJOIN=s.HAULJOIN
            &&bySx
            &&bySC
            &&byMt
            &&bySz
          order by
            &&cols;";
    if (!bySx){qry<-gsub('&&bySx','',qry);} else {qry<-gsub('&&bySx','and u.SEX            =s.SEX',qry);}
    if (!bySC){qry<-gsub('&&bySC','',qry);} else {qry<-gsub('&&bySC','and u.SHELL_CONDITION=s.SHELL_CONDITION',qry);}
    if (!byMt){qry<-gsub('&&byMt','',qry);} else {qry<-gsub('&&byMt','and u.MATURITY       =s.MATURITY',qry);}
    if (!bySz){qry<-gsub('&&bySz','',qry);} else {qry<-gsub('&&bySz','and u.SIZE           =s.SIZE',qry);}
    qry<-gsub("&&cols",cols,qry)
    if (verbosity>1) cat("\nquery is:\n",qry,"\n");
    tbl_cpue<-sqldf(qry);
    
    #replace NA's with 0's. Have to convert to numeric as NA in first row converts remainder to character.
    idx<-which(is.na(tbl_cpue$numIndivs));
    tbl_cpue$numIndivs<-as.numeric(tbl_cpue$numIndivs); tbl_cpue$numIndivs[idx]<-0; 
    tbl_cpue$numCPUE  <-as.numeric(tbl_cpue$numCPUE);   tbl_cpue$numCPUE[idx]  <-0; 
    tbl_cpue$wgtCPUE  <-as.numeric(tbl_cpue$wgtCPUE);   tbl_cpue$wgtCPUE[idx]  <-0; 
    
    tbl_cpue$wgtCPUE<-tbl_cpue$wgtCPUE/1.0E6;#convert cpue in weight from g/(sq nm) to t/(sq nm)
    
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
        write.csv(tbl_cpue,out.csv,na='',row.names=FALSE);
    }
    
    if (verbosity>1) cat("finished calcCPUE.ByHaul\n");
    return(tbl_cpue)
}

#tbl.cpue<-calcCPUE.ByHaul(tbl.hauls,tbl.BTC.MAA.indivs,export=FALSE)
