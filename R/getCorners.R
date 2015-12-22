## This code is part of the Roxalis package
## © C. Heibl 2015 (last update 2015-11-24)

getCorners <- function(sp, buffer = 0.1, aspect){
  
  ## take coordinates of corners
  ## ---------------------------
  xy <- bbox(sp)
  rownames(xy) <- c("lat", "long")
  
  ## apply buffer
  ## ------------
  a <- apply(xy, 1, diff)
  a <- min(buffer * a * .5)
  xy[, "min"] <- xy[, "min"] - a
  xy[, "max"] <- xy[, "max"] + a
  
  ## apply aspect correction
  ## -----------------------
  if ( !missing(aspect) ) {
    
    stop("implement me!")
    
    ## lat/long ist kein kartesischen Koordiantensystem
    ## sonst ginge (H + x) / B = soll, fur soll > ist
    ## und H / (B + x) = soll, für soll < ist
  }
  
  
  list(upperLeft = c(xy["long", "max"], xy["lat", "min"]),
       lowerRight = c(xy["long", "min"], xy["lat", "max"]))
  
}