## This code is part of the ornitho package
## © C. Heibl 2015 (last update 2015-08-18)

extract.coords <- function(sp){
  sub.tmp <- slot(sp, "Polygons")
  sub.tmp[[1]]@coords 
}
