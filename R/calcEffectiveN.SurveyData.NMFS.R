#'
#' @title Calculate and plot effective N's for NMFS survey size compositions using a bootstrapping approach
#'
#' @description Function to calculate and plot effective N's for NMFS survey size compositions using a bootstrapping approach.
#'
#' @param loadRZCs - NULL to calculate and plot resampled size compositions, or path to RData file to re-plot a saved set
#'
#' @details Uses rmd/calcEffectiveN.SurveyData.NMFS.Rmd to produce a report file. Saves results in outDir/RZCs.RData.
#'
#' @export
#'
calcEffectiveN.SurveyData.NMFS<-function(
                     loadRZCs=NULL,
                     outDir="./OutputFiles",
                     title="Effective N for NMFS Survey Size Compositions",
                     bootN=100,
                     species="BTC",
                     minYr=1975,
                     maxYr=2017,
                     minSize=25,
                     maxSize=185,
                     binSize=5,
                     lglZ=125,
                     bySex=TRUE,
                     byMaturity=TRUE,
                     byShellCondition=FALSE,
                     output_format="pdf_document",
                     clean=TRUE,
                     verbosity=0
                     ){
  requireNamespace(rmarkdown);
  render(system.file("rmd/calcEffectiveN.SurveyData.NMFS.Rmd",package="tcsamSurveyData"),
         params=list(loadRZCs=loadRZCs,
                     outDir=outDir,
                     title=title,
                     bootN=bootN,
                     species=species,
                     minYr=minYr,
                     maxYr=maxYr,
                     minSize=minSize,
                     maxSize=maxSize,
                     binSize=binSize,
                     lglZ=lgl,
                     bySex=bySex,
                     byMaturity=byMaturity,
                     byShellCondition=byShellCondition),
         output_format=output_format,
         output_options=list(keep_tex=FALSE,
                             includes=list(in_header=system.file("rmd/StylesforRmdPDFs.sty",package="wtsUtilities"))),
         clean=clean)
}
