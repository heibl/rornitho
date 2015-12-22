## This code is part of the ornitho package
## © C. Heibl 2015 (last update 2015-10-01)

setClass("ornithoSpec", 
         slots = list(
           scientific.name = "character",
           vernacular.name = "character",
           phenology = "character",
           observations = "data.frame")
)

## SET METHOD: INITIALIZE
## ----------------------
setMethod("initialize", "ornithoSpec",
          function(.Object, 
                   scientific.name, vernacular.name,
                   phenology, observations){
            .Object@scientific.name <- scientific.name
            .Object@vernacular.name <- vernacular.name
            .Object@phenology <- phenology
            .Object@observations <- observations
            return(.Object)
          }
)

## USER LEVEL CONSTRUCTOR
## ----------------------
"ornithoSpec" <- function(x, def){
  
  s.name <- names(x) %in% c("scientific.name", 
                            "spec_latin")
  v.name <- names(x) %in% c("vernacular.name", 
                            "spec_name")
  scientific.name <- unique(x[, s.name])
  vernacular.name <- unique(x[, v.name])
  phenology <- def$phenology[def$latin == scientific.name]
  if ( length(phenology) > 1 ){
    phenology <- def$phenology[def$german == vernacular.name]
  }
  not.use <- c("obs_id", "spec_id", "spec_name", 
               "spec_latin", "order", "scientific.name",
               "vernacular.name", "phenology")
  not.use <- names(x) %in% not.use
  observations <- x[, !not.use] 
  
  new(Class = "ornithoSpec", 
      scientific.name = scientific.name,
      vernacular.name = vernacular.name, 
      phenology = phenology,
      observations = observations
  )
}

## SET METHOD: SHOW
## ----------------
setMethod("show",
          signature(object = "ornithoSpec"),
          function (object) {
            cat("\n  *** CLASS ornithoSpec ***",
                "\nscientific name        :", object@scientific.name,
                "\nvernacular name        :", object@vernacular.name,
                "\nphenology              :", object@phenology,
                "\nnumber of observations :", nrow(object@observations),
                "\nfirst observation      :", format(min(object@observations$date)),
                "\nlast observation       :", format(max(object@observations$date))
            )
          }
)

