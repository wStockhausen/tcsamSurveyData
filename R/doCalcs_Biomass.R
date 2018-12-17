#'
#'@title Function to calculate cpue by survey haul from station, haul and individual crab info.
#'
#'@description Function to calculate cpue by survey haul from station, haul and individual crab info.
#'
#'@param tbl.hauls   : dataframe with hauls info (output from \code{\link{selectHauls.TrawlSurvey}})
#'@param tbl.indivs  : dataframe with individual crab info (output from \code{\link{selectIndivs.TrawlSurvey}})
#'@param tbl.strata  : dataframe with survey strata info (output from \code{\link{selectStrata.TrawlSurvey}})
#'@param calcCPUE.byStation : TRUE (otherwise calc cpue using calcCPUE.ByHaul)
#'@param calcBiomass.byEW166 : TRUE
#'@param bySex : FALSE
#'@param byShellCondition : FALSE
#'@param byMaturity : FALSE
#'@param bySize : FALSE
#'@param cutpts : seq(from=0,to=185,by=5)
#'@param truncate.low : TRUE
#'@param truncate.high : FALSE
#'@param export.cpue.byH : FALSE
#'@param export.cpue.byS : FALSE
#'@param export.bio.byStrata : FALSE
#'@param export.bio.EW166 : FALSE
#'@param export.totBio : FALSE
#'@param out.csv.cpue.byH : 'cpue.byH.csv'
#'@param out.csv.cpue.byS : 'cpue.byS.csv'
#'@param out.csv.bio.byStrata : 'bio.byStrata.csv'
#'@param out.csv.bio.EW166 : 'bio.EW166.csv'
#'@param out.csv.totBio : 'totBiomass.csv'
#'@param out.dir : output directory
#'@param verbosity : integer flag indicating level of printed output (0=off,1=minimal,2=full)
#'
#'@return   a list with the following elements: \cr
#'\itemize{
#' \item {cpue.byH}     {: data frame of cpue (numbers and weight) by year, haul and other factor levels}
#' \item {cpue.byS}     {: data frame of cpue (numbers and weight) by year, station and other factor levels (optional)}
#' \item {bio.byStrata} {: data frame with abundance, biomass by year, stratum and other factor levels}
#' \item {bio.EW166}    {: data frame with abundance, biomass by year, EW166 split and other factor levels (optional)}
#' \item {bio.tot}      {: data frame with abundance, biomass by year and other factor levels}
#'}
#'
#'@details
#'Notes: \itemize{
#'   \item CPUE in numbers is in no/(sq. nm.)
#'   \item CPUE in weight  is in mt/(sq. nm.)
#'   \item Abundance is in 10^6 individuals
#'   \item Biomass   is in 10^3 mt
#'   }
#'
#' @export
#'
doCalcs_Biomass<-function(tbl.strata,
                         tbl.hauls,
                         tbl.indivs,
                         calcCPUE.byStation=TRUE,
                         calcBiomass.byEW166=TRUE,
                         bySex=FALSE,
                         byShellCondition=FALSE,
                         byMaturity=FALSE,
                         bySize=FALSE,
                         cutpts=seq(from=0,to=185,by=5),
                         truncate.low=TRUE,
                         truncate.high=FALSE,
                         export.cpue.byH=FALSE,
                         export.cpue.byS=FALSE,
                         export.bio.byStrata=FALSE,
                         export.bio.EW166=FALSE,
                         export.totBio=FALSE,
                         out.csv.cpue.byH='cpue.byH.csv',
                         out.csv.cpue.byS='cpue.byS.csv',
                         out.csv.bio.byStrata='bio.byStrata.csv',
                         out.csv.bio.EW166='bio.EW166.csv',
                         out.csv.totBio='totBiomass.csv',
                         out.dir=NULL,
                         verbosity=0){

    if (verbosity>1) cat("starting doCalcs_Biomass.\n");

    res<-list();#empty list for output

    #calc cpue by haul for all selected individuals
    cpue.byH<-calcCPUE.ByHaul(tbl.hauls,
                              tbl_indivs=tbl.indivs,
                              bySex=bySex,
                              byShellCondition=byShellCondition,
                              byMaturity=byMaturity,
                              bySize=bySize,
                              cutpts=cutpts,
                              truncate.low=truncate.low,
                              truncate.high=truncate.high,
                              export=export.cpue.byH,
                              out.csv=out.csv.cpue.byH,
                              out.dir=out.dir,
                              verbosity=verbosity);
    res[["cpue.byH"]]<-cpue.byH;


    #calc abundance, biomass by strata
    if (calcCPUE.byStation){
        #calc cpue by station from cpue by haul
        cpue.byS<-calcCPUE.ByStation(cpue.byH,
                                     export=export.cpue.byS,
                                     out.csv=out.csv.cpue.byS,
                                     out.dir=out.dir,
                                     verbosity=verbosity);
        res[["cpue.byS"]]<-cpue.byS;

        bio.byStr<-calcBiomass.ByStratum(tbl.strata,
                                         cpue.byS,
                                         export=export.bio.byStrata,
                                         out.csv=out.csv.bio.byStrata,
                                         out.dir=out.dir,
                                         verbosity=verbosity);
    } else {
        bio.byStr<-calcBiomass.ByStratum(tbl.strata,
                                         cpue.byH,
                                         export=export.bio.byStrata,
                                         out.csv=out.csv.bio.byStrata,
                                         out.dir=out.dir,
                                         verbosity=verbosity);
    }
    res[["bio.byStr"]]<-bio.byStr;

    #calc total abundance, biomass
    if (calcBiomass.byEW166){
        #calc abundance, biomass in EW166 regions
        bio.EW166<-calcBiomass.EW166(bio.byStr,
                                     export=export.bio.EW166,
                                     out.csv=out.csv.bio.EW166,
                                     out.dir=out.dir,
                                     verbosity=verbosity);
        res[["bio.EW166"]]<-bio.EW166;
        bio.tot<-calcBiomass.EBS(bio.EW166,
                                 export=export.totBio,
                                 out.csv=out.csv.totBio,
                                 out.dir=out.dir,
                                 verbosity=verbosity)
    } else {
        bio.tot<-calcBiomass.EBS(bio.byStr,
                                 export=export.totBio,
                                 out.csv=out.csv.totBio,
                                 out.dir=out.dir,
                                 verbosity=verbosity)
    }
    res[["bio.tot"]]<-bio.tot;

    if (verbosity>1) cat("finished doBiomassCalcs.\n");
    return(res)
}
