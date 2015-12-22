## This code is part of the ornitho package
## © C. Heibl 2015 (last update 2015-10-26)

setClass("ornithoMap", 
         slots = list(
           grid = "SpatialPolygons",
           grid4 = "SpatialPolygons",
           border = "SpatialPolygons",
#            river = "SpatialLines",
#            lake = "SpatialPolygons",
           river = "ANY",
           lake = "ANY")
)

## SET METHOD: INITIALIZE
## ----------------------
setMethod("initialize", "ornithoMap",
          function(.Object, 
                   grid, grid4, border, lake, river){
            .Object@grid <- grid
            .Object@grid4 <- grid4
            .Object@border <- border
            .Object@river <- river
            .Object@lake <- lake
            return(.Object)
          }
)

## USER LEVEL CONSTRUCTOR
## ----------------------
"ornithoMap" <- function(grid4, border, river, lake,
                         district){
  if ( !missing(district) ){
    d.names <- border$Name_admin
    if ( !district %in% d.names ){
      stop(" district '", district, "' not available", 
           "\navailable districts:", paste(d.names, collapse = ", "))
    }
    border <- border[which(d.names == district), ]
    grid4 <- crop.grid(grid4, border[1, ])
    
  } else {
    grid4 <- crop.grid(grid4, border[1, ])
  }
  grid <- quarter2full(grid4)
  river <- crop2polygon(river, border)
  lake <- crop2polygon(lake, border)
  
  new(Class = "ornithoMap", 
      grid = grid,
      grid4 = grid4, 
      border = border,
      river = river,
      lake = lake
  )
}

## SET METHOD: SHOW
## ----------------
setMethod("show",
          signature(object = "ornithoMap"),
          function (object) {
            p <- proj4string(object@border)
            p <- strsplit(p, "[[:space:]]{0,1}[+]")
            p <- unlist(p)[-1]
            cat("\n  *** CLASS ornithoMap ***",
                "\nextent x   :", bbox(hessen_border)[1, ],
                "\nextenx y   :", bbox(hessen_border)[2, ],
                "\nprojection :", paste(p, "\n            ", sep = "")
            )
          }
)

