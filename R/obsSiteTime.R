#' Calculate number of observations per site and time
#'
#' Counts the observations per site and decade/ pentade
#' @param x An object of class OrnithoData
#' @param time A time space in which to return maxima, either "decade" or "pentade", the default is "decade"
#' @return Returns a data.frame with count maxima per site
#' @export

obsSiteTime <- function(x, time = "decade"){
  
  ifelse(time == "decade", gg <- 1:36, gg <- 1:72)
  
  ## tablulate number of observations per time unit
  ## ----------------------------------------------
  tt <- table(x[, time])
  
  # enter missing rows
  cc <- gg[!gg %in% names(tt)]
  cc <- rep(0, length(cc))
  names(cc) <- gg[!gg %in% names(tt)]
  
  # combine and sort vectors
  tt <- c(tt, cc)
  tt <- tt[order(as.numeric(as.character(names(tt))))]

  return(tt)
}