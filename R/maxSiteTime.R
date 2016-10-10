## This code is part of the ornitho package
## © S. Thorn, C. Heibl 2015 (last update 2015-10-26)

maxSiteTime <- function(x, time = "decade"){
  
  if ( inherits(x, "ornithoSpec") ) x <- x@observations
  
  x$place <- paste(x$place, " HIDDEN", x$hidden, sep = "")
  
  ## calculate maxima per site and time unit
  ## ---------------------------------------
  tt <- tapply(x$count, INDEX = list(x$place, x[, time]), 
               FUN = max)
  tt <- data.frame(tt)
  names(tt) <- gsub("X", "", names(tt))
  tt[is.na(tt)] <- 0
  
  ## if one or more time units are missing ...
  ## -----------------------------------------
  ifelse(time == "decade", gg <- 1:36, gg <- 1:72)
  if ( !all(as.character(gg) %in% names(tt)) ){
    cc <- gg[!gg %in% names(tt)]
    cc <- matrix(0, nrow = nrow(tt), ncol = length(cc))
    cc <- data.frame(cc)
    names(cc) <- gg[!gg %in% names(tt)]
    tt <- cbind(tt, cc)
    tt <- tt[, order(as.numeric(names(tt)))]
  }
  tt
}