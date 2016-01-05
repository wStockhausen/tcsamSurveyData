#'
#'@title Retrieve a stamenmap (replaces ggmap::get_stamenmap)
#'
#'@description Function to retrieve a stamenmap (replaces ggmap::get_stamenmap)
#'
#'@param bbox - bounding box coordinates (left, bottom, right, top) (default is for EBS)
#'@param zoom - zoom level fordownloading tiles (1- 18)
#'@param maptype - "terrain", "watercolor" or "toner"
#'@param land - color for land when maptype is "toner"
#'@param water - color for water when maptype is "toner"
#'@param crop - crop tiles to bbox
#'@param messaging - report messages
#'@param urlonly - return only the url
#'@param filename - temporary file name
#'@param color - 'color' or 'bw'
#'@param ... - other inputs to ggmap::get_stamenmap
#'
#'@return ggmap object
#'
#'@details Requires internet connection.
#'
#'@return object to use with ggmap
#'
#'@importFrom jpeg readJPEG
#'@importFrom plyr ldply
#'
#'@import ggmap 
#'
#'@export
#'
get_stamenmap<-function (bbox = c(left=-180,bottom=54,right=-155,top=63), 
                         zoom = 6, 
                         maptype = c("terrain", "terrain-background", 
                                     "terrain-labels", "terrain-lines", 
                                     "toner", "toner-2010", "toner-2011", 
                                     "toner-background", "toner-hybrid", 
                                     "toner-labels", "toner-lines", 
                                     "toner-lite", "watercolor"), 
                         land="black",
                         water="white",
                         crop = TRUE, 
                         messaging = FALSE, 
                         urlonly = FALSE,
                         filename = "ggmapTemp", 
                         color = c("color", "bw"), 
                         ...) {
    args <- as.list(match.call(expand.dots = TRUE)[-1])
    argsgiven <- names(args)
    if ("bbox" %in% argsgiven) {
        if (!(is.numeric(bbox) && length(bbox) == 4)) {
            stop("bounding box improperly specified.  see ?get_openstreetmap", 
                call. = F)
        }
    }
    if ("zoom" %in% argsgiven) {
        if (!(is.numeric(zoom) && length(zoom) == 1 && zoom == 
            round(zoom) && zoom >= 0 && zoom <= 18)) {
            stop("scale must be a postive integer 0-18, see ?get_stamenmap.", 
                call. = F)
        }
    }
    if ("messaging" %in% argsgiven) 
        stopifnot(is.logical(messaging))
    if ("urlonly" %in% argsgiven) 
        stopifnot(is.logical(urlonly))
    if ("checkargs" %in% argsgiven) {
        .Deprecated(msg = "checkargs argument deprecated, args are always checked after v2.1.")
    }
    maptype <- match.arg(maptype)
    color <- match.arg(color)
    if (is.null(names(bbox))) 
        names(bbox) <- c("left", "bottom", "right", "top")
    if (maptype %in% c("terrain", "terrain-background", "watercolor")) {
        filetype <- "jpg"
    }
    else {
        filetype <- "png"
    }
    fourCorners <- expand.grid(lon = c(bbox["left"], bbox["right"]), 
        lat = c(bbox["bottom"], bbox["top"]))
    fourCorners$zoom <- zoom
    row.names(fourCorners) <- c("lowerleft", "lowerright", "upperleft", 
        "upperright")
    fourCornersTiles <- apply(fourCorners, 1, function(v) LonLat2XY(v[1], 
        v[2], v[3]))
    xsNeeded <- Reduce(":", sort(unique(as.numeric(sapply(fourCornersTiles, 
        function(df) df$X)))))
    numXTiles <- length(xsNeeded)
    ysNeeded <- Reduce(":", sort(unique(as.numeric(sapply(fourCornersTiles, 
        function(df) df$Y)))))
    numYTiles <- length(ysNeeded)
    tilesNeeded <- expand.grid(x = xsNeeded, y = ysNeeded)
    if (nrow(tilesNeeded) > 40) {
        message(paste0(nrow(tilesNeeded), " tiles needed, this may take a while ", 
            "(try a smaller zoom)."))
    }
    base_url <- "http://tile.stamen.com/"
    base_url <- paste(base_url, maptype, "/", zoom, sep = "")
    urls <- paste(base_url, apply(tilesNeeded, 1, paste, collapse = "/"), 
        sep = "/")
    urls <- paste(urls, filetype, sep = ".")
    if (messaging) 
        message(length(urls), " tiles required.")
    if (urlonly) 
        return(urls)
    count <- 0
    nTiles <- nrow(tilesNeeded)
    listOfTiles <- lapply(split(tilesNeeded, 1:nrow(tilesNeeded)), 
        function(v) {
            v <- as.numeric(v)
            get_stamenmap_tile(maptype, zoom, v[1], v[2], force = force, 
                messaging = messaging)
        })
    map <- stitch(listOfTiles)
    if (!crop) {
        dfr.bb <- attr(map, "bb");
    } else if (crop) {
        mbbox <- attr(map, "bb");
        size <- 256 * c(length(xsNeeded), length(ysNeeded));
        slon <- seq(mbbox$ll.lon, mbbox$ur.lon, length.out = size[1]);
        slat <- vector("double", length = 256 * length(ysNeeded));
        for (k in seq_along(ysNeeded)) {
            slat[(k - 1) * 256 + 1:256] <- sapply(as.list(0:255), 
                                                  function(y) {
                                                     XY2LonLat(X = xsNeeded[1], Y = ysNeeded[k], 
                                                     zoom, x = 0, y = y)$lat
                                                   });
        }
        slat <- rev(slat)
        keep_x_ndcs <- which(bbox["left"] <= slon & slon <= bbox["right"])
        keep_y_ndcs <- sort(size[2] - which(bbox["bottom"] <= 
            slat & slat <= bbox["top"]))
        croppedmap <- map[keep_y_ndcs, keep_x_ndcs];
        map <- as.raster(croppedmap);
        dfr.bb <- data.frame(ll.lat = bbox["bottom"], ll.lon = bbox["left"], 
                             ur.lat = bbox["top"],    ur.lon = bbox["right"]);
    }
    class(map) <- c("ggmap", "raster");
    attr(map, "bb")      <- dfr.bb;
    attr(map, "source")  <- "stamen";
    attr(map, "maptype") <- maptype;
    attr(map, "zoom")    <- zoom;
    if (maptype=="toner"){
        idx <- (map=="#000000");#water cells
        map[idx]  <- water;#set water cells to water color
        map[!idx] <- land; #set land cells to land color
    }
    class(map) <- c("ggmap", "raster")
    attr(map, "bb") <- bb;
    
    return(map);
}
#assignInNamespace("get_stamenmap",get_stamenmap,ns="ggmap")
