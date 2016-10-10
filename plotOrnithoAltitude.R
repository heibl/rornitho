## This code is part of the ornitho package
## © S. Thorn, C. Heibl 2015 (last update 2015-08-18)

plotOrnithoAltitude <- function(x, background, 
                                alt.int = 50,
                                alt.min = 0, 
                                alt.max = 1000,
                                col = "grey75"){
  
  x <- x@observations
  
  # create vector with altitudes
  alti <- sort(x$altitude)
  
  # create cutting sequence
  cc <- seq(alt.min + alt.int, alt.max, alt.int)
  countAlti <- function(d, alti = alti, alt.int = alt.int){
    length(alti[alti >= (d - alt.int) & alti < d])
  }
  alti <- sapply(cc, countAlti, alti = alti, alt.int = alt.int)
	
  ## start plotting 
  ## --------------
  brpl <- barplot(alti, col = col[3], 
                  horiz = TRUE,
                  axes = TRUE, 
                  xlim = rev(c(0, max(alti) * 1.1)),
                  cex.lab = 1)
  
  if ( !missing(background) ){
    bg <- sapply(cc, countAlti, 
                 alti = background, alt.int = alt.int)
    bg <- bg / max(bg) * max(alti)
    aa <- rbind(bg, alti)
    #     less.than <- aa[2, ] > 0
    #     aa[2, ] <- abs(aa[2, ])
    #     
    #     col <- rbind(rep("grey75", ncol(aa)),
    #                  rep("yellow", ncol(aa)))
    #     col[, less.than] <- col[2:1, less.than]
    
    polygon(c(bg, bg[1]), 
            c(brpl[, 1], brpl[1, 1]),
            border = col[6])
  }
  box()
  
#   axis(1, line = 1)
  bar.width <- brpl[2, 1] - brpl[1, 1]
  at <- brpl[, 1] + (bar.width / 2)
  at <- c(at[1] - bar.width, at)
  cc[c(TRUE, FALSE)] <- NA
  cc <- c(alt.min, cc)
  axis(4, at, cc, las = 2)

  # text(0.35, 18, paste("Ø =",format(mean(x$Höhe), digits = 0)), cex = 1)
  mtext("Anzahl Beobachtungen", side = 1, line = 2, cex = .75)
  mtext("H\u00F6henverteilung", side = 3, line = 1.3, cex = .9)
}