#'
#' @title Create a tmap-style map of survey cpue as bubbles
#'
#' @description Function to create a tmap-style map of survey cpue as bubbles.
#'
#' @details Uses the tmap package.
#'
#' @param basemap - tmap-style basemap
#' @param cpue - spatial dataframe with cpue data
#' @param raster - raster object with environmental data
#' @param cpueCol - name of column in cpue dataframe to plot as bubbles
#' @param cpueColor - color for cpue bubbles
#' @param cpueAlpha - alpha (transparency) for cpue bubbles
#' @param maxCPUE - max value for cpue scale
#' @param cpueLabel - label for cpue scale
#' @param cpueLegend - TRUE/FALSE to show cpue scale
#' @param rasterCol - name of raster data column
#' @param rasterPalette - palette for raster
#' @param rasterAlpha - alpha (transparency) for raster
#' @param rasterLabel - label for raster scale
#' @param rasterBreaks - breaks for raster scale
#' @param rasterLegend - TRUE/FALSE to show raster scale
#' @param panelLabel - label for map panel
#'
#' @return tmap map object
#'
#' @export
#'
gisCreateSurveyMap.CPUE<-function(basemap,
                                  cpue,
                                  raster=NULL,
                                  cpueCol="CPUE",
                                  cpueColor="blue",
                                  cpueAlpha=0.6,
                                  maxCPUE=NULL,
                                  cpueLabel="CPUE",
                                  cpueLegend=TRUE,
                                  rasterCol="z",
                                  rasterPalette=NULL,
                                  rasterAlpha=0.8,
                                  rasterLabel="",
                                  rasterBreaks=NULL,
                                  rasterLegend=TRUE,
                                  panelLabel="Survey CPUE"
                                  ){
    map<-basemap;

    if (!is.null(raster))
      map <- map + tmap::tm_shape(raster) +
                   tmap::tm_raster(col=rasterCol,alpha=rasterAlpha,showNA=FALSE,legend.show=rasterLegend,title=rasterLabel,
                                   style="cont",breaks=rasterBreaks,palette=rasterPalette,auto.palette.mapping=FALSE);

    cpue[[cpueCol]]<-ifelse(cpue[[cpueCol]]<maxCPUE,cpue[[cpueCol]],maxCPUE);
    map <- map + tmap::tm_shape(cpue) +
                 tmap::tm_bubbles(size=cpueCol,col=cpueColor,alpha=cpueAlpha,showNA=FALSE,legend.size.is.portrait=TRUE,
                                  size.max=maxCPUE,legend.size.show=cpueLegend,title.shape=cpueLabel) +
                 tmap::tm_layout(panel.show=TRUE,
                                 panel.labels=panelLabel,
                                 legend.position=c("right","top"));

    return(map);
}
