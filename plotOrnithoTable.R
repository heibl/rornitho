## This code is part of the ornitho package
## © C. Heibl 2015 (last update 2015-06-29  )

plotOrnithoTable <- function(tab, header, cex = 1, gex = 1,
                             align){
  
  if ( missing(header) ) header <- colnames(tab)
  tab <- rbind(header, tab)
  nr <- nrow(tab); nc <- ncol(tab)
  if ( missing(align) ){
    align <- rep("left", nc)
  }
  
  ylim <- nr:0
  plot(c(0, 1), range(ylim), 
       xaxs = 'i', yaxs = 'i', type = 'n', 
       xlab = '', ylab = '', axes = FALSE)
  
  gap <- strwidth("    ", "user", cex) * gex
  string.widths <- apply(tab, c(1, 2), strwidth, cex = cex)
  string.widths <- string.widths + gap
  max.column.width <- apply(string.widths, 2, max)
  xlim <- c(0, cumsum(max.column.width))
  x <- adj <- vector(length = nc)
  for ( i in seq_along(x) ){
    if ( align[i] == "center" ) {
      x[i] <- mean(xlim[i + (0:1)])
      adj[i] <- .5
    }
    if ( align[i] == "left" ) {
      x[i] <- xlim[i] + .05 * max.column.width[i]
      adj[i] <- 0
    }
    if ( align[i] == "right" ) {
      x[i] <- xlim[i] + .95 * max.column.width[i]
      adj[i] <- 1
    }
  }
  
  for ( r in 1:nr ) {
    for ( c in 1:nc ) {
#       rect(xlim[c],
#            ylim[r],
#            xlim[c + 1],
#            ylim[r + 1])
      font <- ifelse(r == 1, 2, 1)
      text(x[c], mean(ylim[r + (0:1)]),  tab[r, c], 
           adj = adj[c], cex = cex, font = font)
#       draw.cell(paste("", temp.table[r, c], "", sep = " "), 
#                 r, c,
#                 text.cex[r, c], 
#                 bg.col[r, c], 
#                 frame.cell)
    }
  }
  lines(range(xlim), rep(ylim[1], 2))
  lines(range(xlim), rep(ylim[2], 2))
  lines(range(xlim), rep(tail(ylim, 1), 2))
}