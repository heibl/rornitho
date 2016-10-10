## This code is part of the ornitho package
## © C. Heibl & S. Thorn 2016 (last update 2016-02-06)

setClass("ornithoMap", 
         slots = list(
           grid = "SpatialPolygons",
           grid4 = "SpatialPolygons",
           border = "SpatialPolygons",
           aerial = "OpenStreetMap",
           min_raster = "SpatialPolygons"
#            river = "SpatialLines",
#            lake = "SpatialPolygons"
           #river = "ANY",
           #lake = "ANY"
)
)

## SET METHOD: INITIALIZE
## ----------------------
setMethod("initialize", "ornithoMap",
          function(.Object, 
                   border, lake, river){
            .Object@border <- border
            .Object@aerial <- aerial
            .Object@river <- river
            .Object@lake <- lake
            return(.Object)
          }
)

## USER LEVEL CONSTRUCTOR
## ----------------------
"ornithoMap" <- function(what, to, type, river, lake){
  
  #if ( !missing(district) ){
   # d.names <- border$Name_admin
    #if ( !district %in% d.names ){
     # stop(" district '", district, "' not available", 
      #     "\navailable districts:", paste(d.names, collapse = ", "))
  #  }
  border <- crop.border(what = what, to = to)

  # get mtbs for border extension
  data(mtb)
  grid <- crop.grid(grid = mtb, border = border, colname = "TK_NR")
  rm(mtb)

  # get mtbs for border extension
  data(mtb4)
  grid4 <- crop.grid(mtb4, border, colname = "TK_NR")
  rm(mtb4)
  
  # get min raster for extension
  data(min_raster)
  index <- paste0(min_raster$TK25_NR,"_",min_raster$TK25_VIERT)
  index <- which(index %in% unique(grid4$TK_TKVIERT))
  min_raster <- min_raster[index,]
  rm(index)
  
  #} else {
  #  grid4 <- crop.grid(grid4, border)
  #}
  #grid <- quarter2full(grid4)
  #river <- crop2polygon(river, border)
  #lake <- crop2polygon(lake, border)
  
  # get arial map for background
  aerial <- getOpenMap(border, type = type)
 
  new(Class = "ornithoMap",
      border = border,
      grid = grid,
      grid4 = grid4,
      min_raster = min_raster,
      aerial = aerial,
      river = river,
      lake = lake
  )
}

## SET METHOD: SHOW
## ----------------
setMethod("show",
          signature(object = "ornithoMap"),
          function (object) {
            #p <- proj4string(object@border)
            #p <- strsplit(p, "[[:space:]]{0,1}[+]")
            #p <- unlist(p)[-1]
            cat("\n  *** CLASS ornithoMap ***",
                "\nextent x   :", bbox(hessen_border)[1, ],
                "\nextenx y   :", bbox(hessen_border)[2, ],
                "\nprojection :", paste(p, "\n            ", sep = "")
            )
          }
)

