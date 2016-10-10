## This code is part of the ornitho package
## © S. Thorn, C. Heibl 2015 (last update 2015-10-01)

plotOrnithoMap <- function (x, add = FALSE, 
                            maps = c("border", "lake", "river"),
                            border.col = "saddlebrown",
                            water.col = "blue",
                            ...) {
  
  if ( is.null(x@river) ) maps <- maps[maps != "river"]
  if ( is.null(x@lake) ) maps <- maps[maps != "lake"]
  
  ## create ADD vector
  ## -----------------
  add <- rep(add, length(maps))
  add[-1] <- TRUE
  names(add) <- maps
  
  ## base map
  ## --------
  if ( "border" %in% maps ){
    plot(x@border, border = border.col, # col = "grey75",
         add = add["border"], ...)
  }
  
  ## lakes
  ## ----
  if ( "lake" %in% maps ){
    plot(x@lake, add = add["lake"], 
         col = water.col, border = water.col, ...)
  }
  ## rivers
  ## ------
  if ( "river" %in% maps ){
    plot(x@river, add = add["river"], 
         col = water.col, ...)
  }
}
