#'
#' @title Calculate and plot effective N's for NMFS survey size compositions using a bootstrapping approach,
#' with output to a Rmarkdown-created document
#'
#' @description Function to calculate and plot effective N's for NMFS survey size compositions using a bootstrapping approach,
#' with output to a Rmarkdown-created document.
#'
#' @param loadRZCs : NULL to calculate and plot resampled size compositions, or path to RData file to re-plot a saved set
#' @param outDir : path to output folder for plots
#' @param title : document title
#' @param bootN : number of bootstrap runs to make
#' @param species : crab species
#' @param minYr : min year. to include
#' @param maxYr : max survey year to include
#' @param minSize : min size (mm) to include in size comps
#' @param maxSize : max size (mm) to include in size comps
#' @param binSize :bin size (mm)
#' @param lglZ : legal size
#' @param  bySex            : flag (T/F) to calc by sex
#' @param  byMaturity       : flag (T/F) to calc by maturity state
#' @param  byShellCondition : flag (T/F) to calc by shell condition
#' @param  bySize        : flag (T/F) to calc by size
#' @param  output_format : output format for Rmarkdown file
#' @param clean : clean previous files from outDir
#' @param verbosity : integer flag indicating level of printed output (0=off,1=minimal,2=full)
#'
#' @return nothing
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
