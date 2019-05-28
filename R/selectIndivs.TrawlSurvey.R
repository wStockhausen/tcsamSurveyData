#'
#'@title Function to extract crab data on individuals w/ selected characteristics from trawl survey data in NMFS csv format
#'
#'@description Function to extract crab data on individuals w/ selected characteristics from trawl survey data in NMFS csv format.
#'
#'@param tbl_hauls : hauls table (dataframe) from call to selectHauls.TrawlSurvey(...) [required]
#'@param tbl       : table (dataframe) of survey data (or csv filename or NULL)
#'@param col.Size  : column name for size information
#'@param sex             : one of 'MALE','FEMALE','MISSING', 'HERMAPHRODITE', or 'ALL' for narrowing selection of individuals
#'@param shell_condition : one of 'NEW_SHELL','OLD_SHELL' or 'ALL' for narrowing selection of individuals
#'@param maturity        : one of 'IMMATURE','MATURE' or 'ALL' for narrowing selection of individuals
#'@param calcMaleMaturity : flag (T/F) to calculate pr(mature|size) for males based on an ogive (default=F)
#'@param noImmOldShell : flag (T/F) to reclassify all immature, old shell crab to immature, new shell crab (default=F)
#'@param dropLevels : NULL (default) or list of levels to drop, by factor
#'@param minSize : minimum size (width) of individuals to select
#'@param maxSize : maximum size (width) of individuals to select
#'@param export  - boolean flag to export results to csv file
#'@param out.csv - name of output csv file                    (ignored if NULL)
#'@param out.dir - base path for output csv file              (set to folder of input csv file or current working directory)
#'@param verbosity : integer flag indicating level of printed output (0=off,1=minimal,2=full)
#'
#'@return dataframe (see \link{details} for coulmn names)
#'
#'@details If neither tbl or in.csv is given, the user will be prompted for a survey data csv file via a file dialog box.\cr
#' Returned dataframe will have columns:
#' \itemize{\item {HAULJOIN}
#'          \item {numIndivs}         {number of "effective" individuals}
#'          \item {SEX}
#'          \item {SHELL_CONDITION}
#'          \item {MATURITY}
#'          \item {SEX_CODE}
#'          \item {SHELL_CONDITION_CODE}
#'          \item {SIZE}
#'          \item {EGG_COLOR}
#'          \item {EGG_CONDITION}
#'          \item {CLUTCH_SIZE}
#'          \item {CHELA_HEIGHT}
#'          \item {SAMPLING_FACTOR}   {sub-sampling ratio (>= 1)}
#'          \item {WEIGHT}            {measured weight of an individual crab, in grams}
#'          \item {CALCULATED_WEIGHT} {weight of an individual crab, in grams}
#'         } \cr
#' Notes:
#' \itemize{
#'   \item "effective" number of individuals is < 1 when male maturity
#'   \item Weights are in grams.
#' }
#'
#' @import tcsamFunctions
#'
#' @importFrom sqldf sqldf
#' @importFrom wtsUtilities selectFile
#'
#' @export
#'
#-----------------------------------------------------------
#Select individuals from crab trawl survey data file.
#-----------------------------------------------------------
selectIndivs.TrawlSurvey<-function(tbl_hauls,
                                   tbl=NULL,
                                   col.Size='WIDTH',
                                   sex=c('MALE','FEMALE','ALL','MISSING', 'HERMAPHRODITE'),
                                   shell_condition=c('NEW_SHELL','OLD_SHELL','ALL'),
                                   maturity=c('IMMATURE','MATURE','ALL'),
                                   calcMaleMaturity=FALSE,
                                   noImmOldShell=TRUE,
                                   dropLevels=NULL,
                                   minSize=-Inf,
                                   maxSize=Inf,
                                   export=FALSE,
                                   out.csv="SelectedIndivs.csv",
                                   out.dir=NULL,
                                   verbosity=0){
    if (verbosity>0) message("starting selectIndivs.TrawlSurvey.\n");

    if (!is.data.frame(tbl_hauls)) {
        cat("Error in selectIndivs.TrawlSurvey:",
            "tbl_hauls is NULL. Must supply tbl_hauls.",
            "Aborting...",sep='\n');
        return(NULL);
    }

    in.csv<-NULL;
    if (!is.data.frame(tbl)){
        if (!is.character(tbl)) {
            in.csv<-selectFile(ext="csv",caption="Select AFSC crab trawl survey file");
            if (is.null(in.csv)|(in.csv=='')) return(NULL);
        } else {
            in.csv<-tbl;#tbl is a filename
        }
        if (verbosity>1) message("Reading AFSC crab trawl survey csv file for individual crab info.");
        tbl<-read.csv(in.csv,stringsAsFactors=FALSE);
        if (verbosity>1) message("Done reading input csv file.")
    }

    if (is.null(out.dir)) {
        out.dir<-dirname(file.path('.'));
        if (!is.null(in.csv)) {out.dir<-dirname(file.path(in.csv));}
    }
    if (verbosity>0) message(paste0("Output directory for selectIndivs.TrawlSurvey will be '",out.dir,"'."));

    #identify size column (WIDTH, LENGTH) and standardize name to SIZE
    nms<-names(tbl);
    idx<-which(nms==col.Size);
    nms[idx]<-'SIZE';
    #make all names upper case
    names(tbl)<-toupper(nms);


    #extract columns of interest
    cols<-c("HAULJOIN","SEX","SIZE","SHELL_CONDITION",
            "EGG_COLOR","EGG_CONDITION","CLUTCH_SIZE","CHELA_HEIGHT",
            "WEIGHT","CALCULATED_WEIGHT","SAMPLING_FACTOR");
    tbl<-tbl[,cols];

    #extract data for hauls of interest
    qry<-"select
            &&cols
          from
            tbl as t,
            tbl_hauls as h
          where
            t.HAULJOIN=h.HAULJOIN;";
    qry<-gsub("&&cols",paste("t.",cols,sep='',collapse=","),qry);
    if (verbosity>1) message(paste0("\nquery is:\n",qry,"\n"));
    tbl<-sqldf::sqldf(qry);

    #assign -1 to NA's in column CLUTCH_SIZE (i.e., males) to simplify SQL code
    idx<-is.na(tbl$CLUTCH_SIZE);
    tbl$CLUTCH_SIZE[idx]<- -1;

    #2nd pass: convert codes
    codes<-Codes.TrawlSurvey();
    #sex
    sex<-sex[1];
    sex_codes<-codes[["sex"]];
    sq.sex<-"(select * from sex_codes &&sex.cri) as x";
    if (sex=='MALE')          {sq.sex<-gsub("&&sex.cri",'where value="MALE"',  sq.sex)} else
    if (sex=='FEMALE')        {sq.sex<-gsub("&&sex.cri",'where value="FEMALE"',sq.sex)} else
    if (sex=='MISSING')       {sq.sex<-gsub("&&sex.cri",'where value="MISSING"',sq.sex)} else
    if (sex=='HERMAPHRODITE') {sq.sex<-gsub("&&sex.cri",'where value="HERMAPHRODITE"',sq.sex)} else
                       {sq.sex<-gsub("&&sex.cri",'',sq.sex)}
    #shell condition
    sc<-shell_condition[1];
    sc_codes<-codes[["shell_condition"]];
    sq.sc<-"(select * from sc_codes &&sc.cri) as s"
    if (sc=='NEW_SHELL') {sq.sc<-gsub("&&sc.cri",'where value="NEW_SHELL"',sq.sc)} else
    if (sc=='OLD_SHELL') {sq.sc<-gsub("&&sc.cri",'where value="OLD_SHELL"',sq.sc)} else
                         {sq.sc<-gsub("&&sc.cri",'',sq.sc)}
    #maturity, based on clutch size for females, undetermined for males
    mat<-maturity[1];
    mat_codes<-codes[["clutch_size"]];
    sq.mat<-"(select * from mat_codes &&mat.cri) as m"
    if (mat=='ALL')      {sq.mat<-gsub("&&mat.cri",'',sq.mat)} else
    if (mat=='IMMATURE') {sq.mat<-gsub("&&mat.cri",'where value in ("IMMATURE","UNDETERMINED")',sq.mat)} else
                         {sq.mat<-gsub("&&mat.cri",'where value in (  "MATURE","UNDETERMINED")',sq.mat)}
    #note: need "UNDETERMINED" above to get males, whose maturity is not determined by clutch size

    qry<-"select
            t.HAULJOIN as HAULJOIN,
            1 as numIndivs,
            x.value as SEX_VALUE,
            s.value as SC_VALUE,
            m.value as MATURITY,
            t.SEX as SEX_CODE,
            t.SHELL_CONDITION as SHELL_CONDITION_CODE,
            t.SIZE as SIZE,
            t.EGG_COLOR as EGG_COLOR,
            t.EGG_CONDITION as EGG_CONDITION,
            t.CLUTCH_SIZE as CLUTCH_SIZE,
            t.CHELA_HEIGHT as CHELA_HEIGHT,
            t.SAMPLING_FACTOR as SAMPLING_FACTOR,
            t.WEIGHT as WEIGHT,
            t.CALCULATED_WEIGHT as CALCULATED_WEIGHT
          from
            tbl as t,
            &&sq.sex,
            &&sq.sc,
            &&sq.mat
          where
            &&minSizeCri and
            &&maxSizeCri and
            t.SIZE<999 and
            t.SEX=x.survey_code and
            t.SHELL_CONDITION=s.survey_code and
            t.CLUTCH_SIZE=m.survey_code
          order by
            t.HAULJOIN,
            SEX_VALUE,SC_VALUE,SHELL_CONDITION_CODE,MATURITY,CLUTCH_SIZE,SIZE;"

    minSizeCri<-"(1==1)"
    if (is.finite(minSize)) {minSizeCri<-paste(minSize,"<=t.SIZE")}
    qry<-gsub("&&minSizeCri",minSizeCri,qry)
    maxSizeCri<-"(1==1)"
    if (is.finite(maxSize)) {maxSizeCri<-paste("t.SIZE<=",maxSize)}
    qry<-gsub("&&maxSizeCri",maxSizeCri,qry)

    qry<-gsub("&&sq.sex",sq.sex,qry)
    qry<-gsub("&&sq.sc", sq.sc, qry)
    qry<-gsub("&&sq.mat",sq.mat,qry)
    if (verbosity>1) message(paste0("\nquery is:\n",qry,"\n"));

    tbl<-sqldf::sqldf(qry);

    #Change back to NA's from -1's in column CLUTCH_SIZE (i.e., males)
    idx<-tbl$CLUTCH_SIZE==-1;
    tbl$CLUTCH_SIZE[idx]<-NA;

    #rename some columns
    cols<-names(tbl);
    cols[3]<-'SEX';
    cols[4]<-'SHELL_CONDITION';
    names(tbl)<-cols;

    if(calcMaleMaturity){
        idx<-tbl$SEX=='MALE';
        tbl.M<-tbl[idx,];
        tbl.NM<-tbl[!idx,];
        frac.mat<-calc.prMat.Males(tbl.M$SIZE,tbl.M$SHELL_CONDITION);
        tbl.ImmM<-tbl.M;
        tbl.ImmM$numIndivs      <-(1-frac.mat)*tbl.ImmM$numIndivs;#"effective" number of immmature males
        #tbl.ImmM$SAMPLING_FACTOR<-(1-frac.mat)*tbl.ImmM$SAMPLING_FACTOR; <-NOTE: no need to adjust sampling factor
        tbl.ImmM$MATURITY<-'IMMATURE';
        tbl.MatM<-tbl.M;
        tbl.MatM$numIndivs      <-frac.mat*tbl.MatM$numIndivs;#"effective" number of mmature males
        #tbl.MatM$SAMPLING_FACTOR<-frac.mat*tbl.MatM$SAMPLING_FACTOR; <-NOTE: no need to adjust sampling factor
        tbl.MatM$MATURITY<-'MATURE';

        tbl<-rbind(tbl.NM,tbl.ImmM,tbl.MatM);
    }

    #calculate weight, if not already done so
    idx_ncw<-is.na(tbl$CALCULATED_WEIGHT);#TRUE where weight not calculated
    if (any(idx_ncw)){
      cw <- tcsamFunctions::calc.WatZ(tbl$SIZE[idx_ncw],tbl$SEX[idx_ncw],tbl$MATURITY[idx_ncw]);
      tbl$CALCULATED_WEIGHT[idx_ncw] <- cw;
    }

    if (noImmOldShell){
      #reclassify all immature, old shell crab as immature, new shell
      idx<-(tbl$MATURITY=="IMMATURE");
      tbl$SHELL_CONDITION[idx]<-"NEW_SHELL";
    }

    if (!is.null(dropLevels)) tbl<-wtsUtilities::dropLevels(tbl,dropLevels);

    #re-order for consistency
    qry<-"select * from tbl
          order by
            HAULJOIN,SEX,MATURITY,SHELL_CONDITION,SIZE,CLUTCH_SIZE;"
    tbl<-sqldf::sqldf(qry);

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
        write.csv(tbl,out.csv,na='',row.names=FALSE);
    }

    if (verbosity>1) message("finished selectIndivs.TrawlSurvey.");
    return(tbl)
}

#tbl.indivs<-selectIndivs.TrawlSurvey(tbl.hauls,sex='MALE',shell_condition='ALL',maturity='ALL',export=FALSE);

