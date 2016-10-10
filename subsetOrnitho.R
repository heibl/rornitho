subsetOrnitho <- function(ornitho, ornitho.map){
  
  n1 <- names(ornitho)
  ornitho <- ornitho2df(ornitho)
  x <- SpatialPoints(
    coords = ornitho[, c("x", "y")],
    proj4string = CRS("+proj=longlat +ellps=WGS84")
  )
  x <- spTransform(x, CRS(proj4string(ornitho.map@grid)))
  pol <- slot(ornitho.map@border, "polygons")[[1]]@Polygons[[1]]@coords
  
  id <- point.in.polygon(x$x, x$y, pol[, 1], pol[, 2])
  ornitho <- ornitho[id == 1, ]
  ornitho <- df2ornitho(ornitho)
  n2 <- names(ornitho)
  n3 <- setdiff(n1, n2)
  cat("not part of new subset:", 
      paste("\n -", n3, collapse = ""))
  ornitho
}