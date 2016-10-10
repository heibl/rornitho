## This code is part of the ornitho package
## © C. Heibl 2015 (last update 2015-07-01)

ornitho2df <- function(ornitho){
  
  make.df <- function(x){
    data.frame(scientific.name = x@scientific.name,
               vernacular.name = x@vernacular.name,
               phenology = x@phenology,
               x@observations, 
               stringsAsFactors = FALSE)
  }
  if ( inherits(ornitho, "ornithoSpec") )
    ornitho <- list(ornitho)
  ornitho <- lapply(ornitho, make.df)
  ornitho <- do.call(rbind, ornitho)
  rownames(ornitho) <- NULL
  ornitho
}