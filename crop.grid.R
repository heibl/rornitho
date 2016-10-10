## This code is part of the ornitho package
## © C. Heibl & S. Thorn 2015 (last update 2016-06-02)

crop.grid <- function(grid, border, colname){
  
  if(missing(colname))
    stop(paste("colname must be one of", paste(names(grid))))
  
  tk25 <- unique(slot(grid, "data")[,colname])
  pol <- slot(border, "polygons")[[1]]@Polygons[[1]]@coords
  id <- sapply(tk25, grid.in.polygon, grid = grid, pol = pol, colname = colname)
  id <- tk25[id]
  if ( length(id) == 0 ){
    stop("no intersection between grid and polygon")
  }
  grid <- grid[slot(grid, "data")[,colname] %in% id, ]
  grid
}