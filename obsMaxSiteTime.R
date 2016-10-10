## This code is part of the ornitho package
## © S. Thorn, C. Heibl 2015 (last update 2015-06-24  )

obsMaxSiteTime <- function(x, time = "decade"){
  
  if ( inherits(x, "ornithoSpec") ) x <- x@observations
  
  ## calc maxima per site and time unit
  ## ----------------------------------
  how.many.max <- function(x){
    mx <- max(x)
    length(which(x == mx))
  }
  tt <- tapply(x$count, INDEX = list(x$place, x[, time]), 
               FUN = how.many.max)
  sum(tt, na.rm = TRUE)
}