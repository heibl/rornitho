## This code is part of the rornitho package
## © S. Thorn 2016 (last update 2016-01-26)


getOpenMap <- function(border, type){

  border <- spTransform(border,
                        CRS("+proj=longlat +ellps=WGS84"))
  xy <- getCorners(sp = border)

  # compile background layer
  #backgr <- matrix(c(xy$upperLeft, c(xy$upperLeft[1], xy$lowerRight[2]),
  #                 xy$lowerRight, c(xy$lowerRight[1], xy$upperLeft[2])), 
  #                 ncol = 2, byrow = T)
  #backgr <- backgr[,2:1]
  #backgr <- Polygon(backgr)
  #backgr <- Polygons(list(backgr),1)
  #backgr <- SpatialPolygons(list(backgr))
  
  #backgr@proj4string <- CRS("+proj=longlat +ellps=WGS84")
  #backgr <- spTransform(backgr,  
  #                 CRS("+proj=tmerc +lat_0=0 +lon_0=9 +k=1 +x_0=3500000 +y_0=0 +datum=potsdam +units=m +no_defs +ellps=bessel +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7"))

  # retrieve open map and project according to border
  map <- openmap(upperLeft = xy$upperLeft, lowerRight = xy$lowerRight, 
                 type = type)
  map <- openproj(map,  
                  projection = CRS("+proj=tmerc +lat_0=0 +lon_0=9 +k=1 +x_0=3500000 +y_0=0 +ellps=bessel +units=m +no_defs"))
  map
}
