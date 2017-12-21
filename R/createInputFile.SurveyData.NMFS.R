#'
#' Creates a survey input file for TCSAM02
#'
#' This function that creates a survey input file for TCSAM02.
#'
#' @details This function that creates a survey input file for TCSAM02.
#'
#' @param fn - output file name
#' @param cutpts - vector of cutpoints for the input size compositions
#' @param dfrACD - dataframe of aggregated catch data
#' @param dfrSS - dataframe of input sample sizes corresponding to the size comps
#' @param dfrZCs - dataframe of size comps
#' @param verbose - flag to print debugging info
#'
#' @export
#'
createInputFile.SurveyData.NMFS<-function(fn,
                                          cutpts,
                                          dfrACD,
                                          dfrSS,
                                          dfrZCs,
                                          verbose=FALSE){
  #write output to assessment data files
    years<-sort(unique(dfrACD$YEAR));
    if (!file.exists(fn)) {
      res<-file.create(fn);
      if (!res) stop(paste0("Could not create file '",fn,"'.\nAborting...\n"));
    }
    con<-file(fn);
    open(con,open="w");

    cat("#--------------------------------------------------------------------\n",file=con);
    cat("#TCSAM02 model file for NMFS survey data\n",file=con);
    cat("#--------------------------------------------------------------------\n",file=con);
    cat("SURVEY             #required keyword\n",file=con);
    cat("NMFS_trawl_survey  #survey name\n",file=con,sep='');
    cat("TRUE    	        #has index catch data?\n",file=con);
    cat("FALSE    	        #has retained catch data?\n",file=con);
    cat("FALSE    	        #has observed discard catch data\n",file=con);
    cat("FALSE   		    #has observed total catch data\n",file=con);
    cat("FALSE    	        #has effort data?\n",file=con);
    cat("#------------INDEX CATCH DATA------------	\n",file=con);
    cat("CATCH_DATA     #required keyword\n",file=con);
    cat("TRUE           #has aggregate catch abundance (numbers)\n",file=con);
    cat("TRUE           #has aggregate catch biomass (weight)\n",file=con);
    cat("TRUE           #has size frequency data\n",file=con);

    #--survey numbers
    cat("#------------AGGREGATE CATCH ABUNDANCE (NUMBERS)------------#\n",file=con);
    cat("AGGREGATE_ABUNDANCE     #required keyword\n",file=con);
    cat("BY_XM                   #objective function fitting option\n",file=con);
    cat("LOGNORMAL               #likelihood type\n",file=con);
    cat("0.0                     #likelihood weight\n",file=con);
    cat(length(years),"          #number of years\n",file=con,sep='');
    cat("MILLIONS  # units, catch abundance\n",file=con);
    cat("4         #number of factor combinations\n",file=con);
    for (x in c("MALE","FEMALE")){
      for (m in c("IMMATURE","MATURE")){
        cat(paste(x,m,"ALL_SHELL\n"),file=con);
        cat("#year    number    cv\n",file=con);
        for (y in years){
          id<-(dfrACD$YEAR==y)&(dfrACD$SEX==x)&(dfrACD$MATURITY==m);
          cat(y,dfrACD$totABUNDANCE[id],dfrACD$cvABUNDANCE[id],"\n",sep="    ",file=con);
        }#--y
      }#--x
    }#--y

    #--survey biomass
    cat("#------------AGGREGATE CATCH ABUNDANCE (BIOMASS)------------#\n",file=con);
    cat("AGGREGATE_BIOMASS       #required keyword\n",file=con);
    cat("BY_X_MATONLY            #objective function fitting option\n",file=con);
    cat("LOGNORMAL               #likelihood type\n",file=con);
    cat("1.0                     #likelihood weight\n",file=con);
    cat(length(years),"                      #number of years\n",file=con,sep='');
    cat("THOUSANDS_MT  # units, catch biomass\n",file=con);
    cat("4             #number of factor combinations\n",file=con);
    for (x in c("MALE","FEMALE")){
      for (m in c("IMMATURE","MATURE")){
        cat(paste(x,m,"ALL_SHELL\n"),file=con);
        cat("#year    number    cv\n",file=con);
        for (y in years){
          id<-(dfrACD$YEAR==y)&(dfrACD$SEX==x)&(dfrACD$MATURITY==m);
          cat(y,dfrACD$totBIOMASS[id],dfrACD$cvBIOMASS[id],"\n",sep="    ",file=con);
        }#--y
      }#--m
    }#--x

    #--survey size compositions
    bins<-(cutpts[2:length(cutpts)]+cutpts[1:(length(cutpts)-1)])/2;
    cat("#------------NUMBERS-AT-SIZE DATA-----------\n",file=con);
    cat("SIZE_FREQUENCY_DATA  #required keyword\n",file=con);
    cat("BY_XME		            #objective function fitting option\n",file=con);
    cat("MULTINOMIAL 		      #likelihood type\n",file=con);
    cat("1.0                  #likelihood weight\n",file=con);
    cat(length(years),"	#number of years of data\n",file=con);
    cat("MILLIONS		#units\n",file=con);
    cat(length(cutpts),"	#number of size bin cutpoints\n",file=con);
    cat("#size bin cutpts (mm CW)\n",file=con);
    cat(cutpts,"\n",file=con);
    cat("#--------------\n",file=con);
    cat("6		#number of factor combinations\n",file=con);
    for (x in c("MALE","FEMALE")){
      for (m in c("IMMATURE","MATURE")){
        if (m=="IMMATURE"){scs<-"NEW_SHELL";} else {scs<-c("NEW_SHELL","OLD_SHELL");}
        for (s in scs){
          cat(paste(x,m,s,"\n"),file=con);
          cat("#year    number    cv\n",file=con);
          for (y in years){
            idss<-(dfrSS$SEX==x)&(dfrSS$MATURITY==m)&
                  (dfrSS$SHELL_CONDITION==s)&(dfrSS$YEAR==y);
            ss<-dfrSS$ss[idss];
            idzc<-(dfrZCs$SEX==x)&(dfrZCs$MATURITY==m)&
                  (dfrZCs$SHELL_CONDITION==s)&(dfrZCs$YEAR==y);
            rw<-paste(dfrZCs$totABUNDANCE[idzc],collapse=" ");
            cat(y,ss,rw,"\n",sep="    ",file=con);
          }#--y
        }#--s
      }#--m
    }#--x

    cat("#------------RETAINED CATCH DATA------------#\n",file=con);
    cat("#---none\n",file=con);
    cat("#------------DISCARD CATCH DATA------------#\n",file=con);
    cat("#---none\n",file=con);
    cat("#------------TOTAL CATCH DATA------------#\n",file=con);
    cat("#---none\n",file=con);

    close(con);
}
