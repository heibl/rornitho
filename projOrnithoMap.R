## This code is part of the ornitho package
## © C. Heibl 2015 (last update 2015-10-17)

projOrnithoMap <- function(map, projection){
  
  map@grid <- spTransform(map@grid, projection)
  map@grid4 <- spTransform(map@grid4, projection)
  map@border <- spTransform(map@border, projection)
  if ( inherits(hessen_map@river, "SpatialLines") )
    map@river <- spTransform(map@river, projection)
  if ( inherits(hessen_map@river, "SpatialLines") )
    map@lake <- spTransform(map@lake, projection)
  map
}