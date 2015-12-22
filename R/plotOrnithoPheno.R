## This code is part of the ornitho package
## Copyright S. Thorn, C. Heibl 2015 (last update 2015-10-27)

plotOrnithoPheno <- function(x, unit = "decade", 
                             from = "Jan", to = "Dec",
                             col){
  
  x <- x@observations
  
  ## construct index for shiftign x-axis
  ## -----------------------------------
  from <- match.arg(from, month.abb)
  to <- match.arg(to, month.abb)
  id1 <- which(month.abb %in% from)
  id2 <- which(month.abb %in% to)
  if ( id1 < id2 ){
    id <- id1:id2
  } else {
    id <- c(id1:12, 1:id2)
  }
  index <- function(k) (3 * k - 2):(3 * k)
  ID <- unlist(lapply(id, index))
  
  ## construct x-axis
  ## ----------------
  od <- obsSiteTime(x, time = unit)
  od <- od[ID]
  gdm <- maxSiteTime(x, time = unit)
  gdm <- colSums(gdm)
  gdm <- matrix(gdm, ncol = 12)
  gdm <- gdm[, id]
  
  
  ## count maxima per decade
  par(mar = c(3, 5, 4, 3))
  par(mgp = c(0, .6, 0))
  
  ## default color: greyscale
  if ( missing(col) ) col <- gray(8:0/8)
  
  ## barplot
  ## -------
  german <- c("Jan", "Feb", "M\u00E4r", "Apr", "Mai","Jun", 
              "Jul", "Aug", "Sep", "Okt", "Nov", "Dez")
  brpl <- barplot(gdm, beside = TRUE, 
                  col = col[3],
                  space = c(.2, .7),
                  axes = FALSE, ylim = range(pretty(c(0, gdm))),
                  names.arg = german[id]
                  )
  
  axis(2) #, at = pretty(c(0, gdm)), labels = pretty(c(0, gdm)))
  #  format(pos,digits = 0, scientific = F, big.mark = ".")
      
  ## add line for number of observations
  ## -----------------------------------
  par(new = TRUE)
  plot(as.vector(brpl), od, ylab = "", xlab = "",
       col = col[6], type = "l", axes = FALSE, 
       ylim = range(pretty(c(0, od))),
       lwd = 2.5)
  axis(4)
  
  # add axis title
  ## -------------
  mtext("GDM", side = 2, cex = .75, line = 2.5)
  mtext("Summe von GDM (Balken) und Beob. (Linie) pro Dekade", 
        side = 3, cex = .9, line = 1)
  mtext("Beobachtungen", side = 4, cex = .75, line = 2.5)
}