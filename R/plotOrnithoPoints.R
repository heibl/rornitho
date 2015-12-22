## This code is part of the ornitho package
## © S. Thorn & C. Heibl 2015 (last update 2015-10-22)

plotOrnithoPoints <- function (x, show.hidden = FALSE, col,
                               grid, gridcol = "grey25", cex, ...) {
  
  x <- x@observations
  
  ## hidden observations
  ## -------------------
  hidden.obs <- x$hidden == "1"
  if ( !show.hidden ){
    x <- x[!hidden.obs, ]
    hidden.note <- paste(length(which(hidden.obs)), 
                         "gesch\u00FCtzte Beobachtungen nicht angezeigt")
  }
  
  ## if all observations are hidden: plot only grid and quit
  ## -------------------------------------------------------
  if ( nrow(x) == 0 ){
    if ( !missing(grid) ){
      plot(grid@grid4, border = gridcol, lwd = .05, add = TRUE)
      plot(grid@grid, border = gridcol, lwd = .1, add = TRUE)
    } 
    return()
  } 
  
  ## generate sizes for points
  ## -------------------------
  #   if ( format(mean(x$count), digits = 0, big.mark = ".") == 1 ){
  #     x$pt_size <- 1.5 
  #   } else {
  #     x$pt_size <- log(x$count)/log(max(x$count)) * 5 + 1
  #   }
  ## impose frequency classes
  ## ------------------------
  x$freq <- x$count
  x$freq[x$freq >= 2 & x$freq <= 3] <- 2
  x$freq[x$freq >= 4 & x$freq <= 7] <- 3
  x$freq[x$freq >= 8 & x$freq <= 20] <- 4
  x$freq[x$freq >= 21 & x$freq <= 50] <- 5
  x$freq[x$freq >= 51 & x$freq <= 150] <- 6
  x$freq[x$freq >= 151 & x$freq <= 400] <- 7
  x$freq[x$freq >= 401 & x$freq <= 1000] <- 8
  x$freq[x$freq > 1000] <- 9
  freq.id <- x$freq
  txt <-c("1", "2-3", "4-7", "8-20", "21-50", 
          "51-150", "151-400", "401-1000", 
          "\u003E 1000") # \u2013: conversion fails
  txt <- txt[sort(unique(freq.id))]
  x$freq <- x$freq/max(x$freq) + .5
  
  #   if ( missing(cex) ) cex <- x$freq
  
  ## transform to spatial points object
  ## ----------------------------------
  x <- SpatialPointsDataFrame(coords = x[, c("x", "y")],
                              proj4string = CRS(" +proj=longlat +ellps=WGS84"),
                              data = x[, c("freq", "id_obs_det")])
  x <- spTransform(x, CRS("+proj=tmerc +lat_0=0 +lon_0=9 +k=1 +x_0=3500000 +y_0=0 +datum=potsdam +units=m +no_defs"))
  
  ## breeding vs. non-breeding
  ## -------------------------
  if ( missing(col) ) {
    col <- c("skyblue", "yellow")
  } else {
    col <- col[c(2, 8)]
  }
  col <- rep(col[1], nrow(x))
  id  <- grep("Nester|Gesang_Balz", x$id_obs_det)
  col[id] <- col[2]
  
  ## color settings
  ## --------------
  pal <- c("#4575B4", "#74ADD1", "#ABD9E9", "#E0F3F8", "#FFFFBF", "#FEE090", 
           "#FDAE61", "#F46D43", "#D73027")
  col <- pal[freq.id]
  
  ## plot points
  ## ----------
  if ( nrow(x) > 0 ) {
    points(x, cex = x$freq, col = col, bg = col, ...)
  }
  if ( !missing(grid) ){
    plot(grid@grid4, border = gridcol, lwd = .05, add = TRUE)
    plot(grid@grid, border = gridcol, lwd = .1, add = TRUE)
  } 
  ## legend
  ## ------
  legend("bottomright", 
         bty = "n", legend = txt, 
         col = pal[sort(unique(freq.id))], pch = 19, 
         pt.cex = unique(sort(x$freq)),
         title = "H\u00E4ufigkeitsklassen")
  # text("bottomleft", labels = hidden.note)
}
