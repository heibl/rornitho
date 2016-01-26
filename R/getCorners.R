## This code is part of the rornitho package
## © S. Thorn 2016 (last update 2016-01-26)

getCorners <- function(sp, buffer = 0.1, aspect){
  
  ## take coordinates of corners
  ## ---------------------------
  # sp <- hessen_map
  xy <- spTransform(sp, CRS("+proj=longlat +ellps=WGS84"))
  xy <- summary(xy)
  xy <- xy$bbox
  
  rownames(xy) <- c("lat", "long")
  
  ## apply buffer
  ## ------------
  a <- apply(xy, 1, diff)
  a <- min(buffer * a * .5)
  xy[, "min"] <- xy[, "min"] - a
  xy[, "max"] <- xy[, "max"] + a
  
  # compile output
  list(upperLeft = c(xy["long", "max"], xy["lat", "min"]),
       lowerRight = c(xy["long", "min"], xy["lat", "max"]))
  
}