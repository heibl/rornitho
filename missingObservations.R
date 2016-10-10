## This code is part of the ornitho package
## © C. Heibl 2015 (last update 2015-09-29)

missingObservations <- function(ornitho, ornitho.map){
  
  grid <- ornitho.map@grid4
  
  ## all points in the list of ornithoSpec objects
  ## transformed to match projection of grid
  ## ---------------------------------------
  x <- ornitho2df(ornitho)
  x <- SpatialPoints(
    coords = x[, c("x", "y")],
    proj4string = CRS(" +proj=longlat +ellps=WGS84")
    )
  x <- spTransform(x, CRS(proj4string(grid)))
  
  ## spatial overlay
  ## x: for each point, the OBJECTID gives the
  ##    respective grid cell ID it falls in
  ## Obs: vector of grid cell IDs with 
  ##      at least 1 observation
  ## -----------------------------------
  x <- over(x, grid)
  Obs <- unique(x$OBJECTID) 
  noObs <- setdiff(1:length(grid), Obs) 
  
  ## IDs of grid cells inside the administrative border
  ## --------------------------------------------------
  OID <- slot(grid, "data")$OBJECTID
  pol <- slot(ornitho.map@border, "polygons")[[1]]@Polygons[[1]]@coords
  inside <- sapply(OID, grid.in.polygon, 
               grid = grid, pol = pol, colname = "OBJECTID")
  inside <- OID[inside]
  
  ## subsetting the grid
  ## -------------------
  OID <- OID %in% intersect(noObs, inside)
  if ( any(OID) ){
    SpatialPoints(
      coords = coordinates(grid)[OID, , drop = FALSE],
      proj4string = CRS(proj4string(grid))
    )
  } else {
    NULL
  }
}