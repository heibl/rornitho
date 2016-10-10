## This code is part of the ornitho package
## © C. Heibl & S. Thorn 2015 (last update 2016-06-02)

grid.in.polygon <- function(id, grid, pol, colname){
  
  if(missing(colname))
    stop(paste("colname must be one of", names(grid)))
  
  g <- grid[slot(grid, "data")[, colname] == id, ]
  tmp <- slot(g, "polygons")
  xy <- lapply(tmp, extract.coords)
  xy <- do.call(rbind, xy)
  
  res <- point.in.polygon(point.x = xy[, 1], 
                          point.y = xy[, 2],
                          pol.x = pol[, 1], 
                          pol.y = pol[, 2])
  
  any(res > 0)
}