## This code is part of the ornitho package
## © C. Heibl 2015 (last update 2015-10-01)

crop2polygon <- function(x, polygon){
  
  if ( inherits(x, "SpatialLines") ){
    id <- vector(mode = "list", length = length(x))
    for ( i in seq_along(id) ){
      lxy <- x[i, ]@lines[[1]]@Lines[[1]]@coords
      lxy <- SpatialPoints(coords = lxy,
                           proj4string = CRS(proj4string(polygon)))
      z <- over(lxy, polygon)
      id[[i]] <- !is.na(z$OBJECTID)
    }
    inside <- sapply(id, any)
    partial <- sapply(id[inside], function(obj) any(!obj))
    inside <- which(inside)
    partial <- inside[partial]
    
    ## leave segments of rivers that leave border
    ## and return again
    insideTrue <- function(a){
      aa <- which(a)
      a[min(aa):max(aa)] <- TRUE
      a
    }
    id[partial] <- lapply(id[partial], insideTrue)
    partialLines <- vector(mode = "list", 
                           length = length(inside))
    for ( i in seq_along(inside) ){
      partialLines[[i]] <- Lines(Line(x[inside[i], ]@lines[[1]]@Lines[[1]]@coords[id[[inside[i]]], , drop = FALSE]), 
                                 ID = i)
    }
    obj <- SpatialLines(partialLines,
                        CRS(proj4string(polygon)))
  }
  if ( inherits(x, "SpatialPolygons") ){
    id <- vector(mode = "list", length = length(x))
    for ( i in seq_along(id) ){
      xy <- x[i, ]@polygons[[1]]@Polygons[[1]]@coords
      xy <- SpatialPoints(coords = xy,
                          proj4string = CRS(proj4string(polygon)))
      z <- over(xy, polygon)
      id[[i]] <- !is.na(z$OBJECTID)
    }
    inside <- sapply(id, any)
    if ( !any(inside) ){
      obj <- NULL
    } else {
      obj <- x[inside, ]
    }
  }
  obj
}