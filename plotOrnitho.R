## This code is part of the ornitho package
## © S. Thorn, C. Heibl 2015 (last update 2015-10-26)

plotOrnitho <- function(x, map, 
                        show.hidden = FALSE, 
                        rasterize = 100, 
                        raster.model = raster.model,
                        from = "Jan",
                        to = "Dec",
                        col = c("yellow", "red"), 
                        gridcol = "grey25",
                        missing.obs,
                        alt.background,
                        reporter = TRUE){
  ## checks and definitions
  ## ----------------------
  cat("\n", x@scientific.name)
  
  ## color theme
  ## -----------
  col <- colorRampPalette(col)
  col <- col(9)
  
  ## point or rasterized spatial distribution 
  ## of observations
  ## ---------------
  rasterize <- ifelse(nrow(x@observations) >= rasterize, TRUE, FALSE)
  
  ## hidden observations
  ## -------------------
  hidden.obs <- length(which(x@observations$hidden == "1"))
  if ( !show.hidden & hidden.obs > 0 & !rasterize ){
    hidden.note <- paste(hidden.obs, 
                         "gesch\u00FCtzte Beobachtungen nicht angezeigt")
  } else {
    hidden.note <- NULL
  }
  
  layout(matrix(c(1, 1,  
                  2, 3,
                  3, 3,
                  4, 5), 4, 2, byrow = TRUE), 
         heights = c(1, 1, 8, 3), widths = c(2.1, .9))
  #   layout.show(5)
  
  ## 1: heading
  ## -------------
  par(mar = c(.1, 0, .1, .1))
  plot(0:10, 0:10, xlab = "", ylab = "", axes = FALSE, 
       type = "n")
  text(x = 0, y = 7.5, labels = x@vernacular.name, 
       cex = 4, adj = c(0, .5))
  text(x = 0, y = 2.5, labels = x@scientific.name, 
       cex = 2.5, adj = c(0, .5), font = 3)
  text(x = 9.95, y = 7.5, labels = x@phenology, 
       cex = 1.5, adj = c(1, .5))
  
  ## 2: summary data 
  ## ------------------
  plot(0:10, 0:10, xlab = "", ylab = "", axes = FALSE, 
       type = "n")
  text(x = .18, y = 8.5, 
       labels = paste("Anzahl Beobachtungen:", 
                      format(nrow(x@observations), big.mark = ".")),
       cex = 1, adj = c(0, .5))
  text(x = .18, y = 6.5, 
       labels = paste("Anzahl Individuen:", 
                      format(sum(x@observations$count), big.mark = ".")),
       cex = 1, adj = c(0, .5))
  text(x = .18, y = 4.5, paste(labels = "GDM Beobachtungen:", 
                             format(obsMaxSiteTime(x), big.mark = ".")), 
       cex = 1, adj = c(0, .5))
  gdm <- maxSiteTime(x@observations, time = "decade")
  gdm <- colSums(gdm)
  text(x = .18, y = 2.5, labels = paste("Summe GDM:", 
                                      format(sum(gdm), 
                                             big.mark = ".")), 
       cex = 1, adj = c(0, .5))
  if ( !is.null(hidden.note) )
  text(x = .18, y = 0.5, labels = hidden.note, 
       cex = 1, adj = c(0, .5))
  ## 3: map 
  ## ---------
#   bb <- bbox(map@grid)
#   bb <- apply(bb, 1, diff)
#   ar <- bb["y"] / bb["x"]
#   if ( ar < 1.2 ){
#   s <- sum(apply(bbox(map@grid), 1, diff))
#   if ( s < 250000 ){
#     par(mar = rep(9, 4))
#   }
  if ( rasterize ){
    ## a: raster
    ## ---------
    cat("\n.. plotting rasterized distribution ..")
    plotOrnithoRaster(x, map, raster.model, 
                      col, gridcol)
    cat("\n.. plotting base map ..")
    plotOrnithoMap(map, add = TRUE)
    plotOrnithoMTB(cex = .75)
    if ( !is.null(missing.obs) ) 
      plot(missing.obs, pch = 4, add = TRUE)
  } else {
    ## b: points
    ## ---------
    cat("\n.. plotting base map ..")
    plotOrnithoMap(map, add = FALSE)
    if ( !is.na(gridcol) ) plotOrnithoMTB(cex = .75)
    cat("\n.. plotting points ..")
    plotOrnithoPoints(x = x, show.hidden = show.hidden,
                      col = col,
                      grid = map, gridcol = gridcol, 
                      cex = 1, pch = 21)
  }
  par(mar = c(0.1, 0.1, 0.1, .1))
  
  ## 4: phenology 
  ## ---------------
  cat("\n.. plotting phenology ..")
  if ( nrow(x@observations) > 1 ){
    plotOrnithoPheno(x = x, unit = "decade", 
                     from = from, to = to, col)
  } else {
    plot.new()
  }
  
  ## 5: altitude 
  ## --------------
  cat("\n.. plotting altitudinal distribution ..")
  par(mar = c(5.1, 4.1, 4.1, 2.5))
  plotOrnithoAltitude(x = x, background = alt.background,
                      alt.max = 1000, alt.min = 0,
                      alt.int = 50, col = col)
  
  ## plot summary page
  ## -----------------
  o <- x@observations
  
  if ( nrow(o) <= 40 ){
    
    cat("\n.. number of observations < 40: plotting all ..")
    
    o <- o[order(o$date), ]
    o$date <- format(o$date, format = "%d.%m.%Y")
    o$beobachter <- paste(o$name, o$surname)
    if ( !show.hidden ) { 
      o$place[o$hidden == "1"] <- "[Ortsangabe gesch\u00FCtzt]"
    }
    cols <- c("date", "count", "county", "place")
    header <- c("Datum", "Anzahl", "Kreis", "Gebiet")
    if ( reporter ) {
      cols <- c(cols, "beobachter")
      header <- c(header, "Melder")
    }
    o <- o[, cols]
    
    tab.height <- round(nrow(o) * .6, 0)
    layout(matrix(c(1, 
                    2, 
                    3,
                    4), 4, 1, byrow = TRUE), 
           heights = c(1, 1, tab.height, 25 - tab.height), 
           widths = c(1, 1))
    
    ## header: species name
    par(mar = c(.1, 0, .1, .1))
    plot(0:10, 0:10, xlab = "", ylab = "", axes = FALSE, 
         type = "n")
    text(x = 9.95, y = 7.5, labels = x@scientific.name, 
         font = 3, cex = 1.5, adj = c(1, .5))
    
    par(mar = c(.1, 0, .1, .1))
    plot(0:1, 0:1, xlab = "", ylab = "", axes = FALSE, 
         type = "n")
    text(-.05, .25, "S\u00E4mtliche Beobachtungen", 
         cex = 1.5, font = 2, pos = 4)
    align <- c("left", "right", "left", "left", "left")
    plotOrnithoTable(o, cex = 1, 
                     header = header, align = align)
    
    
  } else {
    
    ## TAB 1: 'sdm' (Summe DekadenMaxima):
    ## - sum = sum of GDM
    ## - n = number of decades that go into this sum
    ## - county = abbreviation of county
    ## - loc = place of observation
    ## --------------------------
    sdm <- maxSiteTime(o)
    loc.hidden <- paste(o$place, " HIDDEN", o$hidden, sep = "") 
    sdm <- data.frame(
      sum = rowSums(sdm), 
      n = apply(sdm, 1, function(a) length(which(a > 0))),
      county = o$county[match(rownames(sdm), loc.hidden)],
      loc = rownames(sdm), 
      stringsAsFactors = FALSE)
    sdm <- head(sdm[order(sdm$sum, decreasing = TRUE), ], 10)
    rownames(sdm) <- NULL
    if ( show.hidden ){
      sdm$loc <- gsub(" HIDDEN[[:digit:]]", "", sdm$loc)
    } else {
      sdm$loc[grep("HIDDEN1", sdm$loc)] <- "[Ortsangabe gesch\u00FCtzt]"
      sdm$loc <- gsub(" HIDDEN0", "", sdm$loc)
    }
    

    ## TAB 2: maximum counts 
    ## ---------------------
    o$beobachter <- paste(o$name, o$surname)
    id <- duplicated(o[, c("date","place","count")], fromLast = TRUE)
    o$beobachter[id] <- paste(o$beobachter[id], "u.a.")
    id <- duplicated(o[, c("date","place","count")])
    cols <- c("date", "count", "county", "place")
    header <- c("Datum", "Anzahl", "Kreis", "Gebiet")
    if ( reporter ) {
      cols <- c(cols, "beobachter")
      header <- c(header, "Melder")
    }
    o <- o[!id, c(cols , "yday", "hidden")]
    if ( !show.hidden ) { 
      o$place[o$hidden == "1"] <- "[Ortsangabe gesch\u00FCtzt]"
    }
    
    max.count <- o[order(o$count, decreasing = TRUE), ]
    max.count <- max.count[1:10, cols]
    max.count$date <- format(max.count$date, format = "%d.%m.%Y")
    
    ## TAB 3: earliest observations 
    ## ----------------------------
    if ( x@phenology == "Wintergast" ){
      id <- o$yday > 180
      o$yday[!id] <- o$yday[!id] + 180 
      o$yday[id] <- o$yday[id] - 180
    }
    first <- o[order(o$yday), ]
    first <- first[1:10, cols]
    first$date <- format(first$date, format = "%d.%m.%Y")
    
    ## TAB 4: latest observations 
    ## --------------------------
    last <- o[order(o$yday, decreasing = TRUE), ]
    last <- last[1:10, cols]
    last$date <- format(last$date, format = "%d.%m.%Y")
    
    layout(matrix(c(1, 
                    2, 
                    3, 
                    4, 
                    5, 
                    6, 
                    7, 
                    8,
                    9,
                    10), 10, 1, byrow = TRUE), 
           heights = c(1, rep(c(1, 6), 4), 1), 
           widths = c(1, 1))
    
    ## header: species name
    par(mar = c(.1, 0, .1, .1))
    plot(0:10, 0:10, xlab = "", ylab = "", axes = FALSE, 
         type = "n")
    text(x = 9.95, y = 7.5, labels = x@scientific.name, 
         font = 3, cex = 1.5, adj = c(1, .5))
    
    par(mar = c(.1, 0, .1, .1))
    plot(0:1, 0:1, xlab = "", ylab = "", axes = FALSE, 
         type = "n")
    text(-.05, .25, "Gebiete nach Summe ihrer Dekadenmaxima", 
         cex = 1.5, font = 2, pos = 4)
    plotOrnithoTable(sdm, cex = 1,
                     header = c("Summe GDM", "GDM [n]", "Kreis", "Gebiet"), 
                     align = c("right", "right", "left", "left"))
    align <- c("left", "right", "left", "left", "left")
    plot(0:1, 0:1, xlab = "", ylab = "", axes = FALSE, 
         type = "n")
    text(-.05, .25, "Gebietsmaxima", 
         cex = 1.5, font = 2, pos = 4)
    plotOrnithoTable(max.count, cex = 1, 
                     header = header, align = align)
    
    if ( x@phenology != "Standvogel" ){
      plot(0:1, 0:1, xlab = "", ylab = "", axes = FALSE, 
           type = "n")
      text(-.05, .25, "Erstbeobachtungen", 
           cex = 1.5, font = 2, pos = 4)
      plotOrnithoTable(first, cex = 1, 
                       header = header, align = align)
      
      plot(0:1, 0:1, xlab = "", ylab = "", axes = FALSE, 
           type = "n")
      text(-.05, .25, "Letztbeobachtungen", 
           cex = 1.5, font = 2, pos = 4)
      plotOrnithoTable(last, cex = 1, 
                       header = header, align = align)
    }
  }
}