#'
#'@title Provides a list of dataframes suitable forconverting from survey codes to
#'assessment codes and descriptions.
#'
#'@description This function provides a list of dataframes suitable for converting 
#'from survey codes to assessment codes and descriptions
#'
#'@return a list with the following elements: \cr
#'\itemize{
#'\item sex             : table to convert between survey and assessment sex codes
#'\item shell_condition : table to convert between survey and assessment shell condition codes
#'\item clutch_size     : table to convert between survey and assessment clutch size codes
#'\item strata.BTC      : revised strata for Bairdi Tanner crab
#'\item strata.orig.BTC : original strata for Bairdi Tanner crab
#'\item strata.EW166    : E/W 166W strata
#'}
#'
#'@export
#'
Codes.TrawlSurvey<-function(){
    lst<-list();#output list of codes
    
    #sex
    sex<-list(survey_code=c(1,2,3,4),
              assessment_code=c(1,2,-1,-1),
              value=c("MALE","FEMALE","MISSING","HERMAPHRODITIC"));
    lst[["sex"]]<-as.data.frame(sex);
    
    #shell condition
    sc<-list(survey_code=c(0,1,2,3,4,5,9),
             assessment_code=c(1,1,1,2,2,2,-1),
             value=c("NEW_SHELL","NEW_SHELL","NEW_SHELL","OLD_SHELL","OLD_SHELL","OLD_SHELL","MISSING"))
    lst[["shell_condition"]]<-as.data.frame(sc);
    
    #maturity, based on clutch size
    mat<-list(survey_code    =c(         0,       1,       2,       3,       4,       5,       6,       7, 9,-1),
              assessment_code=c(         1,       2,       2,       2,       2,       2,       2,       2,-2,-1),
              value          =c("IMMATURE","MATURE","MATURE","MATURE","MATURE","MATURE","MATURE","MATURE","MISSING","UNDETERMINED"));
    lst[["clutch_size"]]<-as.data.frame(mat);
    
    #Revised strata for Bairdi Tanner crab
    strata.BTC<-as.data.frame(list(
                              code    =c(               10,                 11,                 98,             14,                      99,               15,                 16,                17),
                              district=c("East 166 Single","East 166 Multiple","East 166 Hot Spot","Pribilof MTCA","Pribilof MTCA Hot Spot","West 166 Single","West 166 Multiple","St. Matthew MTCA"),
                              stratum =c(       "East 166",         "East 166",         "East 166","Pribilof MTCA",         "Pribilof MTCA",       "West 166",         "West 166","St. Matthew MTCA")
                              ));
    lst[["strata.BTC"]]<-strata.BTC;
    
    #Original strata for Bairdi Tanner crab
    strata.BTC<-as.data.frame(list(
                              code    =c(               10,                 11,                 98,             14,                      99,               15,                 16,                17),
                              district=c("East 166 Single","East 166 Multiple","East 166 Hot Spot","Pribilof MTCA","Pribilof MTCA Hot Spot","West 166 Single","West 166 Multiple","St. Matthew MTCA"),
                              stratum =c("East 166 Single","East 166 Multiple","East 166 Hot Spot","Pribilof MTCA","Pribilof MTCA Hot Spot","West 166 Single","West 166 Multiple","St. Matthew MTCA")
                              ));
    lst[["strata.orig.BTC"]]<-strata.BTC;
    
    #Original strata for blue king crab
    strata.BKC<-as.data.frame(list(
                              code    =c(                90,              4,                      99,                  6,                5,                 7,                     9,                   8),
                              district=c("BKC Unstratified","Pribilof MTCA","Pribilof MTCA Hot Spot","Pribilof Multiple","Pribilof Single","St. Matthew MTCA","St. Matthew Multiple","St. Matthew Single"),
                              stratum =c("BKC Unstratified","Pribilof MTCA","Pribilof MTCA Hot Spot","Pribilof Multiple","Pribilof Single","St. Matthew MTCA","St. Matthew Multiple","St. Matthew Single")
                              ));
    lst[["strata.orig.BKC"]]<-strata.BKC;
    
    #strata conversions to EW166
    strata.EW166<-list(orig=c("East 166","East 166 Single","East 166 Multiple","East 166 Hot Spot","West 166","Pribilof MTCA","St. Matthew MTCA","Pribilof MTCA Hot Spot","West 166 Single","West 166 Multiple"),
                       revd=c("East 166",       "East 166",         "East 166",         "East 166","West 166",     "West 166",        "West 166",              "West 166",       "West 166",         "West 166")
                      );
    lst[["strata.EW166"]]<-as.data.frame(strata.EW166);
    return(lst);
}
