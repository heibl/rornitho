## This code is part of the ornitho package
## Copyright S. Thorn & C. Heibl 2015 (last update 2015-07-03)

plotOrnithoRaster <- function(x, map, model = "maxInd", col, 
                              gridcol = "grey25", legend = TRUE){
  
  ## checks and definitions
  ## ----------------------
  if ( inherits(x, "ornithoSpec") ) x <- x@observations
  model <- match.arg(model, c("sumObs", "sumInd", "maxInd"))
  
  if ( missing(col) ){
    col <- colorRampPalette(c("yellow", "red"))
    col <- col(9)
  }
  
  ## create and transform SpatialPoints object
  ## -----------------------------------------
  x <- SpatialPointsDataFrame(
    coords = x[, c("x", "y")],
    proj4string = CRS(" +proj=longlat +ellps=WGS84"),
    data = x[, "count", drop = FALSE])
  x <- spTransform(x, CRS(proj4string(map@grid)))
  
  ## spatial overlay: summary statistics for 
  ## each grid cell of 'grid' (spgc)
  ## -------------------------------
  grid <- map@grid4
  grid.id <- over(x, grid)
  x <- data.frame(count = x$count, grid.id)
  spgc <- data.frame(
    sumObs = tapply(rep(1, nrow(x)), x$OBJECTID, sum),
    sumInd = tapply(x$count, x$OBJECTID, sum),
    maxInd = tapply(x$count, x$OBJECTID, max)
  )
  
  ## impose frequency classes
  ## ------------------------
  spgc[spgc >= 2 & spgc <= 3] <- 2
  spgc[spgc >= 4 & spgc <= 7] <- 3
  spgc[spgc >= 8 & spgc <= 20] <- 4
  spgc[spgc >= 21 & spgc <= 50] <- 5
  spgc[spgc >= 51 & spgc <= 150] <- 6
  spgc[spgc >= 151 & spgc <= 400] <- 7
  spgc[spgc >= 401 & spgc <= 1000] <- 8
  spgc[spgc > 1000] <- 9
  
  id <- match(grid$OBJECTID, rownames(spgc))
  grid$count <- spgc[id, model]
 
  ## plot raster and grid
  ## --------------------
  grid$col <- col[grid$count]
  plot(grid, border = gridcol, col = grid$col, lwd = .05)
  plot(map@grid, border = gridcol, lwd = .1, add = TRUE)
  
  ## plot legend
  ## -----------
  if ( legend ){
    legend("bottomright", inset = c(.3, .075), 
           bty = "n", legend = 
             c("1", "2-3", "4-7", "8-20", "21-50", 
               "51-150", "151-400", "401-1000", 
               "\u003E 1000"), # \u2013: conversion fails
           fill = col, border = gridcol, cex = 1 ,
           title = "H\u00E4ufigkeitsklassen")
  }
}