#'
#' @title Creates a survey input file for TCSAM02
#'
#' @description This function creates a survey input file for TCSAM02.
#'
#' @param fn : output file name
#' @param survey_name : name to use for survey
#' @param acdInfo : list with sublists for abundance and biomass specifying fitType, likeType, and likeWgt
#' @param zcsInfo : list for size compositions specifying fitType, likeType, and likeWgt
#' @param cutpts : vector of cutpoints for the input size compositions
#' @param dfrACD : dataframe of aggregated catch data
#' @param dfrZCs : dataframe of size comps
#' @param dfrSSs : dataframe with relative sample sizes to use with size comps
#' @param ssCol : sample size column name in dfrSSs (default="numIndivs")
#' @param verbose : flag to print debugging info
#'
#' @details This function that creates a survey input file for TCSAM02. It utilizes
#' functions from the \pkg{tcsamFunctions} package to organize the inputs and write the file,
#' with the survey data treated as index catch data.
#'
#' \code{acdInfo} list has elements \code{abundance} and \code{biomass}, each of which
#' is a list with elements (consistent with the output from
#' [tcsamFunctions::inputList_AggregateCatchData]):
#' \itemize{
#' \item{dfr - dataframe}
#' \item{cv - default cv (e.g., for fishery data)}
#' \item{minErr - minimum assumed error in catch data (in 1's)}
#' \item{optFit - objective function fitting option}
#' \item{likeType - likelihood type}
#' \item{likeWgt - likelihood multiplier}
#' \item{unitsIn - input units}
#' \item{unitsOut - output units}
#' }
#'
#' \code{zcsInfo} is a list (consistent with output from
#' [tcsamFunctions::inputList_SizeCompsData]) with elements:
#'
#' \itemize{
#' \item{dfrZCs - size comps dataframe}
#' \item{dfrSSs - sample sizes dataframe}
#' \item{cutpts - cutpoints for size comps}
#' \item{tail_compression - 2-element vector giving tail compression factors}
#' \item{optFit - objective function fitting option}
#' \item{likeType - likelihood type}
#' \item{likeWgt - likelihood multiplier}
#' \item{unitsIn - input units}
#' \item{unitsOut - output units}
#' }
#'
#' @import dplyr
#' @import magrittr
#' @import tcsamFunctions
#'
#' @export
#'
createInputFile.SurveyData<-function(fn,
                                      survey_name="NMFS_trawl_survey",
                                      acdInfo=list(abundance=list(fitType="BY_XM",
                                                                  likeType="LOGNORMAL",
                                                                  likeWgt=0.0,
                                                                  unitsIn="MILLIONS",
                                                                  unitsOut="MILLIONS"),
                                                   biomass=list(fitType="BY_XM",
                                                                likeType="LOGNORMAL",
                                                                likeWgt=1.0),
                                                                unitsIn="MILLIONS",
                                                                unitsOut="MILLIONS"),
                                      zcsInfo=list(fitType="BY_XM",
                                                   likeType="MULTINOMIAL",
                                                   likeWgt=1.0,
                                                   tail_compression=c(0,0),
                                                   unitsIn="MILLIONS",
                                                   unitsOut="MILLIONS"),
                                      cutpts,
                                      dfrACD,
                                      dfrZCs,
                                      dfrSSs,
                                      ssCol="numIndivs",
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
  ss  <- ssCol;

  dfrAbd = dfrACD %>%
             dplyr::select(year=YEAR,sex=SEX,maturity=MATURITY,`shell condition`="SHELL_CONDITION",
                           value=totABUNDANCE,cv=cvABUNDANCE);
  lstAbd = tcsamFunctions::inputList_AggregateCatchData(type="ABUNDANCE",
                                                        dfr=dfrAbd,
                                                        cv=NULL,
                                                        optFit=acdInfo$abundance$fitType,
                                                        likeType=acdInfo$abundance$likeType,
                                                        likeWgt=acdInfo$abundance$likeWgt,
                                                        unitsIn="MILLIONS",
                                                        unitsOut="MILLIONS");
  dfrBio = dfrACD %>%
             dplyr::select(year=YEAR,sex=SEX,maturity=MATURITY,`shell condition`="SHELL_CONDITION",
                           value=totBIOMASS,cv=cvBIOMASS);
  lstBio = tcsamFunctions::inputList_AggregateCatchData(type="BIOMASS",
                                                        dfr=dfrBio,
                                                        cv=NULL,
                                                        optFit=acdInfo$biomass$fitType,
                                                        likeType=acdInfo$biomass$likeType,
                                                        likeWgt=acdInfo$biomass$likeWgt,
                                                        unitsIn="THOUSANDS_MT",
                                                        unitsOut="THOUSANDS_MT");
  if (!is.null(dfrZCs)){
    dfrZCsp = dfrZCs %>%
               dplyr::select(year=YEAR,sex=SEX,maturity=MATURITY,`shell condition`=SHELL_CONDITION,size=SIZE,
                             value=totABUNDANCE);
    names(dfrSSs)[names(dfrSSs)==ssCol]="ss";
    dfrSSsp = dfrSSs %>%
               dplyr::select(year=YEAR,sex=SEX,maturity=MATURITY,`shell condition`="SHELL_CONDITION",ss=ss);
    lstZCsp = tcsamFunctions::inputList_SizeCompsData(dfrZCs=dfrZCsp,
                                                      dfrSSs=dfrSSsp,
                                                      cutpts=cutpts,
                                                      tail_compression =zcsInfo$tail_compression,
                                                      optFit=zcsInfo$fitType,
                                                      likeType=zcsInfo$likeType,
                                                      likeWgt=zcsInfo$likeWgt,
                                                      unitsIn="MILLIONS",
                                                      unitsOut="MILLIONS");
  } else {
    lstZCsp = NULL;
  }
  lstIC = list(lstAbd=lstAbd,lstBio=lstBio,lstZCs=lstZCsp);
  #--write output to assessment data file
  if (!file.exists(fn)) {
    res<-file.create(fn);
    if (!res) stop(paste0("Could not create file '",fn,"'.\nAborting...\n"));
  }
  con<-file(fn);
  open(con,open="w");
  tcsamFunctions::writeInputFile_FleetData(con=con,
                                           fleet=survey_name,
                                           type="SURVEY",
                                           lstIC=lstIC);
  close(con);
}
