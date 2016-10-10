## This code is part of the ornitho package
## © S. Thorn, C. Heibl 2015 (last update 2015-10-27)

ornithoPDF <- function(x, map, file = "ornitho.pdf",
                       show.hidden = FALSE,
                       rasterize = 100,
                       raster.model = "maxInd",
                       from = "Jan",
                       to = "Dec",
                       col = c("yellow", "red"), 
                       gridcol = "grey25",
                       reporter = TRUE){
  
  ## TK25 squares without observations
  ## ---------------------------------
  noObs <- missingObservations(x, map)
  
  ## convert list of ornithoSpec object
  ## to data frame
  ## -------------
  xdf <- ornitho2df(x)
  
  
  pdf(file, paper = "a4", height = 20)
  
  ## summary page
  ## ------------
  layout(matrix(c(1, 1,  
                  2, 3,
                  3, 3,
                  4, 5), 4, 2, byrow = TRUE), 
         heights = c(1, 1, 8, 3), widths = c(2.1, .9))
  par(mar = c(.1, 0, .1, .1))
  plot(0:10, 0:10, xlab = "", ylab = "", axes = FALSE, 
       type = "n")
  text(x = 0, y = 5, labels = "Anzahl der Arten pro Koordinatenpaar", 
       cex = 3, adj = c(0, .5))
  plot.new()
  plotOrnithoMap(hessen_map)
  plotOrnithoCount(xdf, what = "spec", add = TRUE)
  plot.new(); plot.new()
  
  ## species treatments
  ## ------------------
  lapply(x, plotOrnitho, 
         map = map, 
         show.hidden = show.hidden,
         rasterize = rasterize, 
         raster.model = raster.model,
         col = col, 
         gridcol = gridcol, 
         missing.obs = noObs,
         alt.background = xdf$altitude,
         from = from, to = to,
         reporter = reporter)
  dev.off()
}