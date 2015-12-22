## This code is part of the ornitho package
## © C. Heibl 2015 (last update 2015-09-29)

crop.grid <- function(grid, polygon){
  
  tk25 <- levels(slot(grid, "data")$tk25)
  pol <- slot(polygon, "polygons")[[1]]@Polygons[[1]]@coords
  id <- sapply(tk25, grid.in.polygon, grid = grid, pol = pol)
  id <- tk25[id]
  if ( length(id) == 0 ){
    stop("no intersection between grid and polygon")
  }
  grid <- grid[slot(grid, "data")$tk25 %in% id, ]
  grid
}