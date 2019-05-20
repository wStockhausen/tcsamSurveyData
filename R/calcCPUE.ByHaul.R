#'
#'@title Calculate cpue by survey haul and other factors from station, haul and individual crab info
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
#'@param checksSFs : flag to check consistency of scale factors
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
#'\item   SEX
#'\item   MATURITY
#'\item   SHELL_CONDITION
#'\item   SIZE
#'\item   numIndivs
#'\item   SAMPLING_FACTOR
#'\item   AREA_SWEPT_VARIABLE
#'\item   numCPUE
#'\item   wgtCPUE
#'}
#'\cr Other notes: \cr
#'\itemize{
#'\item   AREA_SWEPT_VARIABLE is in sq. nm.
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
                          checkSFs=FALSE,
                          export=FALSE,
                          out.csv='cpue.ByHaul.csv',
                          out.dir=NULL,
                          verbosity=0){
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
    byX<-bySex;byS<-byShellCondition;byM<-byMaturity;byZ<-bySize;

    if (!byX){tbl_indivs$SEX            <-"ALL";}
    if (!byS){tbl_indivs$SHELL_CONDITION<-"ALL";}
    if (!byM){tbl_indivs$MATURITY       <-"ALL";}
    if (!byZ){tbl_indivs$SIZE           <-"ALL";tbl_uzs<-data.frame(SIZE="ALL");} else {
      #expand cutpts to truncate or not
      nCtPts<-length(cutpts);
      ctpts.tmp<-cutpts;
      if (!truncate.low ) ctpts.tmp[1]<-0;
      if (!truncate.high) ctpts.tmp[nCtPts]<-Inf;
      #apply cutpts to sizes
      cuts<-cut(tbl_indivs$SIZE,ctpts.tmp,right=FALSE,labels=FALSE)
      tbl_indivs$SIZE<-cutpts[cuts];
      tbl_indivs<-tbl_indivs[!is.na(tbl_indivs$SIZE),];
      tbl_uzs<-data.frame(SIZE=cutpts[1:(nCtPts-1)]);
    }

    #Calculate numbers and weights expanded by sampling factors and sum by
    #hauljoin and sex/maturity/shell condition/size categories
    #over individuals for hauls w/ nonzero catches.
    #Note that this DOES NOT average over hauls, as calcCPUE.ByStation(...) does.
    qry<-"select
            HAULJOIN,
            SEX,
            MATURITY,
            SHELL_CONDITION,
            SIZE,
            sum(numIndivs) as numIndivs,
            sum(SAMPLING_FACTOR*numIndivs)/sum(numIndivs) as SAMPLING_FACTOR,
            sum(SAMPLING_FACTOR*numIndivs) as expNUM,
            sum(SAMPLING_FACTOR*CALCULATED_WEIGHT) as expWGT
          from
            tbl_indivs
          group by
            HAULJOIN,SEX,MATURITY,SHELL_CONDITION,SIZE
          order by
            HAULJOIN,SEX,MATURITY,SHELL_CONDITION,SIZE;";
    # if (verbosity>1) cat("\nquery is:\n",qry,"\n");
    tbl_sums<-sqldf::sqldf(qry);

    #create table of the sampling factor for each haul by sex averaged over maturity, shell condition and size
    #to fill in sampling factors for categories with no observed crab
    qry<-"select
            HAULJOIN, SEX, avg(SAMPLING_FACTOR) as SAMPLING_FACTOR
          from tbl_sums
          group by HAULJOIN, SEX;";
    tbl_sfs<-sqldf::sqldf(qry);

    #create table of years,strata,stations,hauls x uniq "factors" (i.e., sex, maturity, shell condition, size)
        qry<-"select *
              from
               (select distinct
                  SEX,MATURITY,SHELL_CONDITION
                from tbl_sums as s), tbl_uzs
              order by
                SEX,MATURITY,SHELL_CONDITION,SIZE;";
        # if (verbosity>1) cat("\nquery is:\n",qry,"\n");
        tbl_ufctrs<-sqldf::sqldf(qry);

        qry<-"select *
              from
                (select
                    YEAR,STRATUM,GIS_STATION,HAULJOIN,
                    1*MID_LONGITUDE as LONGITUDE,
                    1*MID_LATITUDE as LATITUDE,
                    AREA_SWEPT_VARIABLE
                 from tbl_hauls),
                tbl_ufctrs;"
        tbl_uhfs<-sqldf::sqldf(qry);

    #expand sums to hauls w/ zero catches and calculate cpue
    qry<-"select
            u.YEAR,u.STRATUM,u.GIS_STATION,u.HAULJOIN,u.LONGITUDE,u.LATITUDE,
            u.SEX,u.MATURITY,u.SHELL_CONDITION,u.SIZE,
            s.numIndivs,
            s.SAMPLING_FACTOR,
            u.AREA_SWEPT_VARIABLE,
            s.expNum/u.AREA_SWEPT_VARIABLE as numCPUE,
            s.expWgt/u.AREA_SWEPT_VARIABLE as wgtCPUE
          from
            tbl_uhfs as u left join
            tbl_sums as s
          on
            u.HAULJOIN       =s.HAULJOIN and
            u.SEX            =s.SEX and
            u.MATURITY       =s.MATURITY and
            u.SHELL_CONDITION=s.SHELL_CONDITION and
            u.SIZE           =s.SIZE
          order by
            u.YEAR,u.STRATUM,u.GIS_STATION,u.HAULJOIN,u.LONGITUDE,u.LATITUDE,
            u.SEX,u.MATURITY,u.SHELL_CONDITION,u.SIZE;";
    # if (verbosity>1) cat("\nquery is:\n",qry,"\n");
    tbl_cpue<-sqldf::sqldf(qry);

    #update rows with no observed individuals
    #--identify rows based on missing numIndivs
    idx<-which(is.na(tbl_cpue$numIndivs));
    #--replace NA's with 0's for numIndivs, numCPUE, wgtCPUE.
    #--Have to make sure column is numeric as NA in first row converts remainder to character.
    tbl_cpue$numIndivs<-as.numeric(tbl_cpue$numIndivs); tbl_cpue$numIndivs[idx]<-0;
    tbl_cpue$numCPUE  <-as.numeric(tbl_cpue$numCPUE);   tbl_cpue$numCPUE[idx]  <-0;
    tbl_cpue$wgtCPUE  <-as.numeric(tbl_cpue$wgtCPUE);   tbl_cpue$wgtCPUE[idx]  <-0;
    #--replace NA's in SAMPLING_FACTOR with average by sex in haul
    #--Have to make sure column is numeric as NA in first row converts remainder to character.
    tbl_cpue$SAMPLING_FACTOR<-as.numeric(tbl_cpue$SAMPLING_FACTOR);
    qry<-"select
            c.YEAR,c.STRATUM,c.GIS_STATION,c.HAULJOIN,c.LONGITUDE,c.LATITUDE,
            c.SEX,c.MATURITY,c.SHELL_CONDITION,c.SIZE,
            c.numIndivs,
            &&checkSFs
            CASE WHEN (c.SAMPLING_FACTOR IS NOT NULL) THEN c.SAMPLING_FACTOR ELSE s.SAMPLING_FACTOR END as SAMPLING_FACTOR,
            c.AREA_SWEPT_VARIABLE,
            c.numCPUE,
            c.wgtCPUE
          from
            tbl_cpue as c left join tbl_sfs as s
          on
             c.HAULJOIN = s.HAULJOIN and
             c.SEX      = s.SEX;";
    str<-"";
    if (checkSFs) str<-"CASE WHEN (c.SAMPLING_FACTOR IS NOT NULL) THEN c.SAMPLING_FACTOR ELSE -1 END                as origSAMPLING_FACTOR,";
    qry<-gsub("&&checkSFs",str,qry,fixed=TRUE);
    tbl_cpue<-sqldf::sqldf(qry);
    #--might have haul/sex combinations that had no observed crab, so SAMPLING_FACTOR would still be missing
    #----For these, use sex-averaged SAMPLING_FACTOR by haul from tbl_sfs to fill in tbl_cpue
    qry<-"select HAULJOIN, avg(SAMPLING_FACTOR) as SAMPLING_FACTOR from tbl_sfs group by HAULJOIN;";
    tbl_sfs1<-sqldf::sqldf(qry);
    qry<-"select
            c.YEAR,c.STRATUM,c.GIS_STATION,c.HAULJOIN,c.LONGITUDE,c.LATITUDE,
            c.SEX,c.MATURITY,c.SHELL_CONDITION,c.SIZE,
            c.numIndivs,
            CASE WHEN (c.SAMPLING_FACTOR IS NOT NULL) THEN c.SAMPLING_FACTOR ELSE s.SAMPLING_FACTOR END as SAMPLING_FACTOR,
            c.AREA_SWEPT_VARIABLE,
            c.numCPUE,
            c.wgtCPUE
          from
            tbl_cpue as c left join tbl_sfs1 as s
          on
             c.HAULJOIN = s.HAULJOIN;";
    tbl_cpue<-sqldf::sqldf(qry);



    #convert cpue in weight from g/(sq nm) to t/(sq nm)
    tbl_cpue$wgtCPUE<-tbl_cpue$wgtCPUE/1.0E6;

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
