## This code is part of the ornitho package
## © S. Thorn, C. Heibl 2015 (last update 2015-11-06)

read.ornitho <- function(file, subsp = NULL, 
                         family = NULL, 
                         phenology = NULL, 
                         rm.abs = TRUE){
  
  if ( length(grep("[.]txt", file)) != length(file) )
    stop("function currently only for type .txt available")
  
  ## read tab-separated database export
  ## more than one file can be read
  ## ------------------------------
  a <- setAttributes(file)
  x <- lapply(file, read.csv2, as.is = TRUE, 
              sep = "\t", fill = TRUE,
              skip = 2, quote = "", dec = ".",
              col.names = a$alias, 
              colClasses = a$class,
              encoding = "latin1")
  x <- do.call(rbind, x)
  x$date <- as.POSIXct(x$date, format = "%d.%m.%Y")
  
  ## species list
  ## ------------
  def <- speclist(subsp, family, phenology)
  
  ## determine ignored 'taxa' (first time)
  ## --------------------------------------
  ignored <- !x$spec_latin %in% def$latin
  if ( any(ignored) ){
    ignored <- x[ignored, c("spec_latin", "spec_name")]
    ignored <- table(ignored$spec_latin)
    ignored <- data.frame(ignored,
                          stringsAsFactors = FALSE)
    names(ignored) <- c("species", "nb.obs")
    
    ## pool subspecies (according to def)
    ## ----------------------------------
    pool <- paste(def$latin, "[[:lower:]]{2,}")
    pool <- sapply(pool, grep, x = ignored$species)
    pool <- def$latin[sapply(pool, length) > 0]
    for ( i in pool ){
      j <- paste(i, ".+", sep = "")
      x$spec_latin <- gsub(j, i, x$spec_latin)
    }
    
    ## determine ignored 'taxa' (second time)
    ## --------------------------------------
    ignored <- !x$spec_latin %in% def$latin
    ignored <- x[ignored, c("spec_latin", "spec_name")]
    cat("\n", nrow(ignored), " of ", nrow(x), 
        " observations (~", 
        round(nrow(ignored) / nrow(x) * 100, 1),
        "%) have been ignored", 
        "\n    (see file 'ignored.txt' for details)", 
        sep = "")
    ignored <- table(ignored$spec_latin)
    ignored <- data.frame(ignored,
                          stringsAsFactors = FALSE)
    names(ignored) <- c("species", "nb.obs")
    write.csv(ignored, "ignored.csv")
  }
  

  
  ## subset data (according to def)
  ## ------------------------------
  x <- x[x$spec_latin %in% def$latin, ]
  
  ## replace vernacular names by 'standard names'
  ## --------------------------------------------
  id <- match(x$spec_latin, def$latin)
  x$spec_name <- def$german[id]
  
  ## order according to linear classification
  ## ----------------------------------------
  x <- x[order(x$order), ]
  spec <- unique(x$spec_latin) 
  
  ## translate column 'hidden'
  ## -------------------------
  x$hidden <- gsub("Nein", "0", x$hidden)
  x$hidden <- gsub("Ja", "1", x$hidden)
  
  ## id_obs_det
  ## -----------
  vec <- data.frame(
    id = paste(4, 1:20, sep = "_"), 
    meaning = c("rastend_ruhend", "Nahrung suchend", 
                "Schlaf_Sammelplatz","Mauserplatz",
                "Flug zum_vom Schlafplatz",
                "Flug ins Nahrungsgebiet",
                "\u00FCberfliegend kein_nicht sicherer Zug",
                "N ziehend","NO ziehend","O ziehend",
                "SO ziehend","S ziehend","SW ziehend",
                "W ziehend", "NW ziehend",
                "Totfund_Rupfung",
                "ziehend ohne Richtungsangabe",
                "Fang zur Beringung",
                "Nesterz\u00E4hlung",
                "Gesang_Balz"))
  x$id_obs_det <- vec$meaning[match(x$id_obs_det, vec$id)]
  
  # id_acc_loc
  vec <- data.frame(
    id = paste(2, 1:5, sep = "_"), 
    meaning = c("\u003C10 m","10-100 m",
                "100-500 m","500-1000 m",
                "\u003E1000 m"))
  x$id_acc_loc <- vec$meaning[match(x$id_acc_loc, vec$id)]
  
  # id_rest_hab
  vec <- data.frame(
    id = paste(1, 1:20, sep = "_"),
    meaning = c("Brache kein Ackerbrache",
                "Buhnenfeld",
                "Getreidestoppel",
                "Gew\u00E4sser",
                "Gr\u00FCnland",
                "Gr\u00FCnland \u00FCberschwemmt",
                "Kartoffeln_Kartoffelstoppel",
                "Kleegras_Luzerne",
                "Mais_Maisstoppel",
                "Raps_Rapsstoppel",
                "Sommergetreide",
                "R\u00FCben_R\u00FCbenstoppel", 
                "Salzwiese", 
                "Acker frisch umgebrochen", 
                "Watt_Windwatt", 
                "Wintergetreide", 
                "Sonstiges", 
                "Acker \u00FCberschwemmt", 
                "Ackerbrache",
                "Stoppelacker unbestimmt"))
  x$id_rest_hab <- vec$meaning[match(x$id_rest_hab, vec$id)]
  
  # remove absence points
  if ( rm.abs )  x <- x[x$count > 0, ] 
  
  x <- split(x, f = x$spec_latin)
  x <- lapply(x, ornithoSpec, def = def)
  spec <- spec[spec %in% names(x)]
  x <- x[match(spec, names(x))]
  return(x)
}
