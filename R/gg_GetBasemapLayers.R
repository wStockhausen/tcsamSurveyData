#'
#' @title Create basemap layers for maps based on the \pkg{ggplot2} package
#'
#' @description This function creates basemap layers for maps based on the \pkg{ggplot2} package
#'
#' @details Uses functions from package [wtsGIS].
#'
#' @param sf_land - a spatial layer representing land (or NULL)
#' @param sf_bathym - a spatial layer representing bathymetry (or NULL)
#' @param sfs_survey -  a list of spatial layers representing the survey grid and survey stations (or NULL)
#' @param final_crs - string representation of CRS (default = WGS84) used for ALL shapefiles
#' @param bbox - a \code{sf::bbox} bounding box
#' @param bw - flag (T/F) to create layers in black & white (TRUE) or color (FALSE)
#' @param colors - list of colors to use for "land","bathym", "grid" and "stations"
#' @param size - point size for the station locations
#'
#' @return - a list of \pkg{ggplot2} objects
#'
#' @details The returned list has elements:
#' \itemize{
#'   \item{bathym}
#'   \item{land}
#'   \item{grid}
#'   \item{stations}
#'   \item{map_scale}
#'   \item{theme}
#' }
#'
#' @import ggplot2
#' @importFrom wtsGIS get_crs
#' @importFrom wtsGIS getStandardBBox
#' @importFrom wtsGIS transformCRS
#'
#' @export
#'
gg_GetBasemapLayers<-function(sf_land=NULL,
                              sf_bathym=NULL,
                              sfs_survey=NULL,
                              final_crs=wtsGIS::get_crs("AlaskaAlbers"),
                              bbox=wtsGIS::getStandardBBox("EBS"),
                              bw=FALSE,
                              colors=list(land="green",
                                          bathym="blue",
                                          grid="dark grey",
                                          stations="red"),
                              size=0.01){

  if (is.null(sf_land))    sf_land    = wtsGIS::getPackagedLayer("Alaska");
  if (is.null(sf_bathym))  sf_bathym  = wtsGIS::getPackagedLayer("ShelfBathymetry");
  if (is.null(sfs_survey)) sfs_survey = gisGetSurveyGridLayers();

  if (bw){
    lyr_land   = ggplot2::geom_sf(data=sf_land,            colour=NA,fill="grey",inherit.aes=FALSE);
    lyr_bathym = ggplot2::geom_sf(data=sf_bathym,          colour="grey",        inherit.aes=FALSE);
    lyr_grid   = ggplot2::geom_sf(data=sfs_survey$grid,    colour="grey",fill=NA,inherit.aes=FALSE);
    lyr_stns   = ggplot2::geom_sf(data=sfs_survey$stations,colour="black",       inherit.aes=FALSE,size=size);
  } else {
    lyr_land   = ggplot2::geom_sf(data=sf_land,            colour=NA,fill=colors$land,inherit.aes=FALSE);
    lyr_bathym = ggplot2::geom_sf(data=sf_bathym,          colour=colors$bathym,      inherit.aes=FALSE);
    lyr_grid   = ggplot2::geom_sf(data=sfs_survey$grid,    colour=colors$grid,fill=NA,inherit.aes=FALSE);
    lyr_stns   = ggplot2::geom_sf(data=sfs_survey$stations,colour=colors$stations,    inherit.aes=FALSE,size=size);
  }

  #--define coordinate scale for default basemap
  bbx = wtsGIS::transformBBox(bbox,final_crs);
  map_scale = ggplot2::coord_sf(xlim=c(bbx["xmin"],bbx["xmax"]),
                                ylim=c(bbx["ymin"],bbx["ymax"]),
                                crs=final_crs,
                                expand=FALSE,clip="on",
                                default=TRUE);

  #--define theme
  #----define aspect ratio for panels
  asp=NULL; #--let ggplot2 work it out
  if (!sf::st_is_longlat(final_crs)) asp = (bbx["ymax"]-bbx["ymin"])/(bbx["xmax"]-bbx["xmin"]);
  #----remove axis titles
  theme = ggplot2::theme(axis.title.x=ggplot2::element_blank(),
                         axis.title.y=ggplot2::element_blank(),
                         axis.text=ggplot2::element_blank(),
                         panel.spacing=grid::unit(0.05,"cm"),
                         plot.margin=ggplot2::margin(t=0.05,r=0.05,b=0.05,l=0.05,unit="cm"),
                         legend.margin=ggplot2::margin(t=0.05,r=0.05,b=0.05,l=0.05,unit="cm"),
                         aspect.ratio=asp);
  return(list(bathym=lyr_bathym,
              land=lyr_land,
              grid=lyr_grid,
              stations=lyr_stns,
              map_scale=map_scale,
              theme=theme));
}

# lyrs = gg_GetBasemapLayers();
# ggplot2::ggplot()+lyrs$land+lyrs$bathym+lyrs$grid+lyrs$stations+lyrs$map_scale;

