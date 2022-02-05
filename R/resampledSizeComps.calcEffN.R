#'
#' @title Calculate and plot effective N from resampled size compositions
#'
#' @description Function to calculate and plot effective N from resampled size compositions.
#'
#' @param dfr - dataframe with resampled size compositions
#' @param byFacs - vector of column names for factors other than YEAR and STRATUM
#' @param nDefault - assumed effective sample size (for plotting)
#' @param nStations - typical number of survey stations (for plotting)
#'
#' @details dfr should be an output from \code{\link{resampledSizeComps.calc}}.
#'
#' @import ggplot2
#'
#' @export
#'
resampledSizeComps.calcEffN<-function(dfr,
                                      byFacs="",
                                      nDefault=200,
                                      nStations=375){

  #--convert to lower case and replace "_"'s with spaces
  dfr$SEX            <-tolower(dfr$SEX);
  dfr$MATURITY       <-tolower(dfr$MATURITY);
  dfr$SHELL_CONDITION<-tolower(dfr$SHELL_CONDITION);
  dfr$SHELL_CONDITION<-gsub("_"," ",dfr$SHELL_CONDITION,fixed=TRUE);

  #--define factor-related stuff
  nFacs<-0;
  frmlaFacs<-"";
  if (any(byFacs!="")) {
    nFacs<-length(byFacs);
    frmlaFacs<-paste0("+",paste(byFacs,collapse="+"));
  }

  #--get original sample sizes
  dfr0<-dfr[dfr$i==0,];
  frmla<-paste0("STRATUM+YEAR",frmlaFacs,"~.");
  origN<-reshape2::dcast(dfr0,frmla,fun.aggregate=sum,value.var="numIndivs");
  names(origN)[3+nFacs]<-"numIndivs";
  tempH<-reshape2::dcast(dfr0,frmla,fun.aggregate=mean,value.var="numNonZeroHauls");
  origN$numNonZeroHauls<-tempH[[3+nFacs]];

  #--normalize the original and bootstrapped size comps by year across sex, maturity, and shell condition
  frmla<-paste0("i+STRATUM+YEAR",frmlaFacs,"~.");
  totN<-reshape2::dcast(dfr,frmla,fun.aggregate=sum,value.var="totABUNDANCE");
  names(totN)[4+nFacs]<-"totN";
  strWhr<-"";
  if (nFacs>0) {
    str <- paste(paste0("d.",byFacs),"=",paste0("t.",byFacs),collapse=" and \n")
    strWhr<-paste("and\n",str,"\n");
  }
  qry<-"select
          d.i,d.STRATUM,d.YEAR,d.SEX,d.MATURITY,d.SHELL_CONDITION,d.SIZE,
          d.totAbundance/t.totN as p
        from
          dfr as d, totN as t
        where
          d.i = t.i and
          d.YEAR = t.YEAR and
          d.STRATUM = t.STRATUM &&strWhr
        order by d.i,d.STRATUM,d.YEAR,d.SEX,d.MATURITY,d.SHELL_CONDITION,d.SIZE;";
  qry<-gsub("&&strWhr",strWhr,qry,fixed=TRUE);
  dfrNZCs<-sqldf::sqldf(qry);

  #--extract normalized observed size comps
  dfrNZC0<-dfrNZCs[dfrNZCs$i==0,];
  #--extract normalized bootstrapped size comps
  dfrNZCs<-dfrNZCs[dfrNZCs$i>0,];

  #--calculate mean size comps
  dfrp<-dfr[dfr$i>0,];#remove observed size comp
  dfrMnZCs<-reshape2::dcast(dfrp,YEAR+STRATUM+SEX+SHELL_CONDITION+MATURITY+SIZE~.,fun.aggregate=mean,value.var="totABUNDANCE");
  names(dfrMnZCs)[7]<-"totABUNDANCE";
  #--and normalize them
  frmla<-paste0("STRATUM+YEAR",frmlaFacs,"~.");
  totNMn<-reshape2::dcast(dfrMnZCs,frmla,fun.aggregate=sum,value.var="totABUNDANCE");
  names(totNMn)[3+nFacs]<-"totN";
  qry<-"select
          d.STRATUM,d.YEAR,d.SEX,d.MATURITY,d.SHELL_CONDITION,d.SIZE,
          d.totAbundance/t.totN as p
        from
          dfrMnZCs as d, totNMn as t
        where
          d.YEAR = t.YEAR and
          d.STRATUM = t.STRATUM &&strWhr
        order by d.STRATUM,d.YEAR,d.SEX,d.MATURITY,d.SHELL_CONDITION,d.SIZE;";
  qry<-gsub("&&strWhr",strWhr,qry,fixed=TRUE);
  dfrMnNZCs<-sqldf::sqldf(qry);

  #--calculate effective Ns
  qry<-"select
          d.i,d.YEAR,d.STRATUM,d.SEX,d.MATURITY,d.SHELL_CONDITION,d.SIZE,
          d.p*(1-d.p) as pq,
          (d.p-m.p)*(d.p-m.p) as vp
        from
          dfrNZCs as d, dfrMnNZCs as m
        where
          d.YEAR            = m.YEAR            and
          d.STRATUM         = m.STRATUM         and
          d.SEX             = m.SEX             and
          d.MATURITY        = m.MATURITY        and
          d.SHELL_CONDITION = m.SHELL_CONDITION and
          d.SIZE            = m.SIZE
        order by d.i,d.YEAR,d.STRATUM,d.SEX,d.MATURITY,d.SHELL_CONDITION,d.SIZE;";
  tmp1 <- sqldf::sqldf(qry);
  strCols<-"";
  if (nFacs>0) strCols<-paste(",",paste(byFacs,collapse=", "))
  qry<-"select
          i, YEAR, STRATUM&&strCols,
          sum(pq)/sum(vp) as effN
        from tmp1
        group by i, YEAR, STRATUM&&strCols
        order by i, YEAR, STRATUM&&strCols;";
  qry<-gsub("&&strCols",strCols,qry,fixed=TRUE);
  tmp2 <- sqldf::sqldf(qry);
  qry<-"select
          STRATUM, YEAR&&strCols,
          avg(effN) as avgEffN,
          1.0/avg(1.0/effN) as hmnEffN
        from tmp2
        group by STRATUM, YEAR&&strCols
        order by STRATUM, YEAR&&strCols;";
  qry<-gsub("&&strCols",strCols,qry,fixed=TRUE);
  dfrEffNs <- sqldf::sqldf(qry);
  colNames<-c("stratum","year");
  if (nFacs>0) colNames<-c(colNames,tolower(byFacs));
  names(dfrEffNs)<-c(colNames,"avg(N)","har(N)");
  dfrEffNs[["num. crab"]]       <-origN$numIndivs;
  dfrEffNs[["num. non-0 hauls"]]<-origN$numNonZeroHauls;

  #--plot results
  require(ggplot2);
  ps<-list();
  measure.vars<-c("avg(N)","har(N)","num. crab","num. non-0 hauls");
  tmp<-reshape2::melt(dfrEffNs,measure.vars=measure.vars,variable.name="type",value.name="N");
  frmla<-".";
  if (nFacs>0) frmla<-tolower(paste0(byFacs,collapse="+"));
  frmla<-paste0(frmla,"~stratum")
  p <- ggplot(tmp,aes_string(x="year",y="N",colour="type",linetype="stratum",shape="stratum"));
  p <- p + geom_hline(yintercept=nDefault,linetype=2);
  p <- p + geom_hline(yintercept=nStations,linetype=3);
  p <- p + geom_line();
  p <- p + geom_point();
  p <- p + facet_grid(rows=as.formula(frmla));
  p <- p + scale_y_continuous(limits=c(0,NA));
  p <- p + labs(x="year",y="effective N");
  ps[[paste0("Figure &&figno. Input N (dotted line), effective N, number of crab measured, and number of non-zero hauls.")]]<-p;

  p <- p %+% tmp[tmp$type!="num. crab",];
  ps[[paste0("Figure &&figno. Input N (dotted line), effective N, and number of non-zero hauls..")]]<-p;

  return(list(plots=ps,dfrEffNs=dfrEffNs));
}

#lst=resampledSizeComps.calcEffN(dfrRZCs,byFacs="SEX");
