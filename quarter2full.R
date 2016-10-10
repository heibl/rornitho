## This code is part of the ornitho package
## © C. Heibl 2015 (last update 2015-08-17)

quarter2full <- function(grid){
  
  quarter2full.cell <- function(grid, tk25){
    
    g <- grid[slot(grid, "data")$tk25 == tk25, ]
    tmp <- slot(g, "polygons")
    xy <- lapply(tmp, extract.coords)
    
    ord <- slot(g, "data")[, c("row", "col")]
    ord$row[min(ord$row) == ord$row] <- "top"
    ord$row["top" != ord$row] <- "bottom"
    ord$col[min(ord$col) == ord$col] <- "left"
    ord$col["left" != ord$col] <- "right"
    ord <- paste(ord$row, ord$col, sep = "")
    names(xy) <- ord
    
    getCorners <- function(z, corner){
      
      z <- unique(z[[corner]])
      decr <- ifelse(length(grep("left", corner)) == 1, 
                     FALSE, TRUE)
      minmax <- ifelse(length(grep("top", corner)) == 1, 
                       max, min)
      
      z <- z[order(z[, 1], decreasing = decr), ]
      dz <- abs(diff(z[, 1]))
      id <- 1:(which(dz > max(dz)/4)[1])
      id <- intersect(id, which(z[, 2] == minmax(z[id, 2])))
      z[id, , drop = FALSE][1, ]
    } # end of function: getCorners
    
    cr <- c("bottomleft", "topleft", "topright",
            "bottomright", "bottomleft")
    cr <- lapply(cr, getCorners, z = xy)
    do.call(rbind, cr)
  } # end of function: quarter2full.cell
  
  id <- unique(slot(grid, "data")$tk25)
  p <- lapply(id, quarter2full.cell, grid = grid)
  #   p <- Polygons(lapply(p, Polygon), ID = "a")
  pp <- list()
  for ( i in seq_along(id) ){
    pp[[i]] <- Polygon(p[[i]])
    pp[[i]] <- Polygons(pp[i], ID = id[i])
  }
  p <- SpatialPolygons(
    pp,
    proj4string = CRS(proj4string(grid)))
  id <- data.frame(tk25 = id)
  rownames(id) <- id$tk25
  SpatialPolygonsDataFrame(p, data = id)
}