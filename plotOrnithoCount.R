plotOrnithoCount <- function(x, what = "observations", 
                             add = TRUE, legend = TRUE){
  
  if ( is.list(x) & all(sapply(x, inherits, what = "ornithoSpec")) ){
    x <- ornitho2df(x)
  }
  
  ## count number of observations or species
  ## per coordinate pair
  ## -------------------
  what <- match.arg(what, c("observations", "species"))
  if ( what == "obseravtions"){
    counter <- function(x) length(x)
  } else {
    counter <- function(x) length(unique(x))
  }
  x <- tapply(x$scientific.name, 
              list(paste(x$x, x$y, sep = "-")), 
              counter)
  
  ## transform to spatial points object
  ## ----------------------------------
  crds <- strsplit(names(x), "-")
  crds <- lapply(crds, as.numeric)
  crds <- do.call(rbind, crds)
  x <- SpatialPointsDataFrame(coords = crds,
                              proj4string = CRS(" +proj=longlat +ellps=WGS84"),
                              data = data.frame(freq = x))
  x <- spTransform(x, CRS("+proj=tmerc +lat_0=0 +lon_0=9 +k=1 +x_0=3500000 +y_0=0 +datum=potsdam +units=m +no_defs"))
  x <- x[order(x$freq), ]
  
  ## plot points
  ## -----------
  #   if ( missing(col) ){
  col <- colorRampPalette(c("yellow", "red"))
  pal <- col(length(unique(x$freq)))
  #   }
  col <- pal[x$freq]
  plot(x, col = col, add = add)
  
  ## plot legend
  ## -----------
  if ( legend ){
    legend("bottomright", inset = c(.3, .075), 
           bty = "n", legend = min(x$freq):max(x$freq), 
           fill = pal, border = "black", cex = 1 ,
           title = paste("Number of", what)
    )
  }
}