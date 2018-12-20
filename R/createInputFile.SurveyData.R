#'
#' Creates a survey input file for TCSAM02
#'
#' This function that creates a survey input file for TCSAM02.
#'
#' @details This function that creates a survey input file for TCSAM02.
#'
#' @param fn - output file name
#' @param survey_name - name to use for survey
#' @param acdInfo - list with sublists for abundance and biomass specifying fitType, likeType, and likeWgt
#' @param acdInfo - list for size compositions specifying fitType, likeType, and likeWgt
#' @param cutpts - vector of cutpoints for the input size compositions
#' @param dfrACD - dataframe of aggregated catch data
#' @param dfrZCs - dataframe of size comps
#' @param inpSS - nominal sample size for scaling size compositions
#' @param verbose - flag to print debugging info
#'
#' @export
#'
createInputFile.SurveyData<-function(fn,
                                      survey_name="NMFS_trawl_survey",
                                      acdInfo=list(abundance=list(fitType="BY_XM",
                                                                  likeType="LOGNORMAL",
                                                                  likeWgt=0.0),
                                                   biomass=list(fitType="BY_X_MAT_ONLY",
                                                                  likeType="LOGNORMAL",
                                                                  likeWgt=1.0)),
                                      zcsInfo=list(fitType="BY_XME",
                                                   likeType="MULTINOMIAL",
                                                   likeWgt=1.0),
                                      cutpts,
                                      dfrACD,
                                      dfrZCs,
                                      inpSS=200,
                                      verbose=FALSE){
  #--define some character constants
  yr  <- "YEAR"
  sx  <- "SEX";
  mt  <- "MATURITY";
  sc  <- "SHELL_CONDITION";
  sz  <- "SIZE";
  abd <- "totABUNDANCE";
  cvA <- "cvABUNDANCE";
  bio <- "totBIOMASS";
  cvB <- "cvBIOMASS";
  ss  <- "ss";

  #--write output to assessment data file
    if (!file.exists(fn)) {
      res<-file.create(fn);
      if (!res) stop(paste0("Could not create file '",fn,"'.\nAborting...\n"));
    }
    con<-file(fn);
    open(con,open="w");

    cat("#--------------------------------------------------------------------\n",file=con);
    cat("#TCSAM02 model file for survey data\n",file=con);
    cat("#--------------------------------------------------------------------\n",file=con);
    cat("SURVEY    #required keyword\n",file=con);
    cat(survey_name,"\t#survey name\n",file=con,sep='');
    cat("TRUE      #has index catch data?\n",file=con);
    cat("FALSE     #has retained catch data?\n",file=con);
    cat("FALSE     #has observed discard catch data\n",file=con);
    cat("FALSE     #has observed total catch data\n",file=con);
    cat("FALSE     #has effort data?\n",file=con);
    cat("#------------INDEX CATCH DATA------------	\n",file=con);
    cat("CATCH_DATA     #required keyword\n",file=con);
    cat("TRUE           #has aggregate catch abundance (numbers)\n",file=con);
    cat("TRUE           #has aggregate catch biomass (weight)\n",file=con);
    cat("TRUE           #has size frequency data\n",file=con);

    tmp<-dfrACD;
    uYs<-sort(unique(dfrACD$YEAR));
    uFCs<-unique(dfrACD[,c(sx,mt,sc)])
    if (verbose){
      cat("uFCs:\n")
      print(uFCs);
    }
    #--survey numbers
    cat("#------------AGGREGATE CATCH ABUNDANCE (NUMBERS)------------#\n",file=con);
    cat("AGGREGATE_ABUNDANCE     #required keyword\n",file=con);
    cat(acdInfo$abundance$fitType, "               #objective function fitting option\n",file=con);
    cat(acdInfo$abundance$likeType,"               #likelihood type\n",file=con);
    cat(acdInfo$abundance$likeWgt, "               #likelihood weight\n",file=con);
    cat(length(uYs),"          #number of years\n",file=con,sep='');
    cat("MILLIONS  # units, catch abundance\n",file=con);
    cat(nrow(uFCs),"         #number of factor combinations\n",file=con);
    for (iFC in 1:nrow(uFCs)){
      fc<-uFCs[iFC,];
      cat(toupper(subForTCSAM(as.character(fc[[sx]]),"ALL_SEX")),
          toupper(subForTCSAM(as.character(fc[[mt]]),"ALL_MATURITY")),
          toupper(subForTCSAM(as.character(fc[[sc]]),"ALL_SHELL")),"\n",file=con);
      cat("#year    abundance    cv\n",file=con);
      for (y in uYs){
        ida<-(tmp[[yr]]==y)&
              (tmp[[sx]]==fc[[sx]])&
              (tmp[[mt]]==fc[[mt]])&
              (tmp[[sc]]==fc[[sc]]);
        cat(y,tmp[ida,abd],tmp[ida,cvA],"\n",sep="    ",file=con);
      }#--y
    }#--fc
    rm(fc,y,ida);

    #--survey biomass
    cat("#------------AGGREGATE CATCH ABUNDANCE (BIOMASS)------------#\n",file=con);
    cat("AGGREGATE_BIOMASS       #required keyword\n",file=con);
    cat(acdInfo$biomass$fitType, "               #objective function fitting option\n",file=con);
    cat(acdInfo$biomass$likeType,"               #likelihood type\n",file=con);
    cat(acdInfo$biomass$likeWgt, "               #likelihood weight\n",file=con);
    cat(length(uYs),"                      #number of years\n",file=con,sep='');
    cat("THOUSANDS_MT            # units, catch biomass\n",file=con);
    cat(nrow(uFCs),"		#number of factor combinations\n",file=con);
    for (iFC in 1:nrow(uFCs)){
      fc<-uFCs[iFC,];
      cat(toupper(subForTCSAM(as.character(fc[[sx]]),"ALL_SEX")),
          toupper(subForTCSAM(as.character(fc[[mt]]),"ALL_MATURITY")),
          toupper(subForTCSAM(as.character(fc[[sc]]),"ALL_SHELL")),"\n",file=con);
      cat("#year    biomass    cv\n",file=con);
      for (y in uYs){
        ida<-(tmp[[yr]]==y)&
              (tmp[[sx]]==fc[[sx]])&
              (tmp[[mt]]==fc[[mt]])&
              (tmp[[sc]]==fc[[sc]]);
        cat(y,tmp[ida,bio],tmp[ida,cvB],"\n",sep="    ",file=con);
      }#--y
    }#--fc
    rm(fc,y,ida);

    #--survey size compositions
    bins<-(cutpts[2:length(cutpts)]+cutpts[1:(length(cutpts)-1)])/2;
    tmp<-dfrZCs;
    tmp[[sx]][tmp[[sx]]=="ALL"]<-"UNDETERMINED";
    tmp[[mt]][tmp[[mt]]=="ALL"]<-"UNDETERMINED";
    tmp[[sc]][tmp[[sc]]=="ALL"]<-"UNDETERMINED";
    uYs<-sort(unique(tmp[[yr]]));
    uFCs<-unique(tmp[,c(sx,mt,sc)]);
    if (verbose){
      cat("uFCs:\n")
      print(uFCs);
    }
    dfrSS<-reshape2::dcast(tmp,
                           paste0(paste(yr,sx,mt,sc,sep="+"),"~."),
                           fun.aggregate=wtsUtilities::Sum,value.var="numIndivs");
    names(dfrSS)[5]<-"ss";#numbers of crab measured by factor combination
    ssT <- reshape2::dcast(dfrZCs,
                           paste0(yr,"~."),
                           fun.aggregate=wtsUtilities::Sum,value.var="numIndivs");
    names(ssT)[2]<-"ss";#total number of crab measured
    cat("#------------NUMBERS-AT-SIZE DATA-----------\n",file=con);
    cat("SIZE_FREQUENCY_DATA  #required keyword\n",file=con);
    cat(zcsInfo$fitType, "               #objective function fitting option\n",file=con);
    cat(zcsInfo$likeType,"               #likelihood type\n",file=con);
    cat(zcsInfo$likeWgt, "               #likelihood weight\n",file=con);
    cat(length(uYs),"       #number of years of data\n",file=con);
    cat("MILLIONS             #units\n",file=con);
    cat(length(cutpts)," #number of size bin cutpoints\n",file=con);
    cat("#size bin cutpts (mm CW)\n",file=con);
    cat(cutpts,"\n",file=con);
    cat("#--------------\n",file=con);
    cat(nrow(uFCs),"    #number of factor combinations\n",file=con);
    for (iFC in 1:nrow(uFCs)){
      fc<-uFCs[iFC,];
      #cat("uFC[",iFC,",]:\n");
      #print(fc);
      cat(toupper(subForTCSAM(fc[[sx]],"ALL_SEX")),
          toupper(subForTCSAM(fc[[mt]],"ALL_MATURITY")),
          toupper(subForTCSAM(fc[[sc]],"ALL_SHELL")),"\n",file=con);
          cat("#year    ss    ",bins,"\n",file=con);
          for (y in uYs){
            ids<-(dfrSS[[yr]]==y)&
                  (dfrSS[[sx]]==fc[[sx]])&
                  (dfrSS[[mt]]==fc[[mt]])&
                  (dfrSS[[sc]]==fc[[sc]]);
            idz<-(tmp[[yr]]==y)&
                  (tmp[[sx]]==fc[[sx]])&
                  (tmp[[mt]]==fc[[mt]])&
                  (tmp[[sc]]==fc[[sc]]);
            rw<-paste(tmp[idz,abd],collapse=" ");
            cat(y,inpSS*dfrSS[ids,"ss"]/ssT[ssT[[yr]]==y,"ss"],rw,"\n",sep="    ",file=con);
          }#--y
    }#--iFC

    cat("#------------RETAINED CATCH DATA------------#\n",file=con);
    cat("#---none\n",file=con);
    cat("#------------DISCARD CATCH DATA------------#\n",file=con);
    cat("#---none\n",file=con);
    cat("#------------TOTAL CATCH DATA------------#\n",file=con);
    cat("#---none\n",file=con);

    close(con);
}
