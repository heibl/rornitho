line.in.polygon <- function(line, polygon){
  
  lxy <- line@lines[[1]]@Lines[[1]]@coords
  lxy <- SpatialPoints(coords = lxy,
                       proj4string = CRS(proj4string(polygon)))
  id <- over(lxy, polygon)
  !all(is.na(id$OBJECTID))
}