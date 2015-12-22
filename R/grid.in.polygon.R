## This code is part of the ornitho package
## © C. Heibl 2015 (last update 2015-09-29)

grid.in.polygon <- function(id, grid, pol, colname = "tk25"){
  
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