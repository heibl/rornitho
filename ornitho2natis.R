## This code is part of the ornitho package
## © S. Thorn 2016 (last update 2016-06-02)

ornitho2natis <- function(x, file){

  # load reference list
  data("natis_ref", envir = environment())
  
  # convert object
  x <- ornitho2df(x)

  # define output table
  tab <- matrix(ncol = 137, nrow = nrow(x))
  tab <- data.frame(tab)
  
  names(tab) <- c("KART_KEY","K_ART_KEY","K_GEB_KEY","DATUM_INT","DATUM_EXP","ZEITRAUM",
                  "ANZAHL","ANZ_GESAMT","ANZ_UNBEST","ANZ_M","ANZ_W","ANZ_JUV","STADIUM",
                  "STATUS","LAGE","GKK_RP","GKK_HP","BIOTOPE","PROJEKT","ERFASSER",
                  "BESTIMMER","QUELLE","KBEMERKUNG","K_JOKER1","K_JOKER2","K_JOKER3",
                  "EING_DAT","K_LZT_AEND","K_GSAFETY","K_ASAFETY","K_BESTQUAL",
                  "K_ERF_METH","K_VERWEND","K_EINGABE","K_IMPQUELL","K_AENDERNG",
                  "K_UTMZ","K_UTMR","K_UTMH","K_LONGIT","K_LATIT","K_TK25NR","K_TK25TL",
                  "K_UNSCHRF","K_DAT_HERK","K_KLASS","ART_KEY","DT_NAME","ENGL_NAME",
                  "KLASSE","ORDNUNG","FAMILIE","GATTUNG","ART","UNTERART","ERSTBESCHR",
                  "SYNONYM","ABEMERKUNG","RL_BRD","RL_LAND","RL_REGION","A_JOKER1",
                  "A_JOKER2","A_JOKER3","A_LZT_AEND","A_VSR_I","A_VSR_II1","A_VSR_II2",
                  "A_VSR_III1","A_VSR_III2","A_VSR_LIFE","A_FFH_II","A_FFH_IV",
                  "A_FFH_V","A_EU_CODE","A_BERN_K","A_BONN_K","A_E_BATS","A_AEWA",
                  "A_ASAFETY","A_EING_DAT","A_TAXQUEL","A_EINGABE","A_IMPQUELL",
                  "A_AENDERNG","A_SYN_PRI","A_SYN_SEK","A_ART_GRP","A_S_STATUS",
                  "A_BARTSVO","GEBIET_KEY","GEBIET","GEBIET_NR","GEMEINDE","GEMEIND_NR",
                  "GEMARKUNG","GEMARK_NR","KREIS","RP","LAND","NATURRAUM","NRAUM_NR",
                  "GROESSE","GRS_EINH","LAENGE","BREITE","HOEHE_MIN","HOEHE_MAX",
                  "G_JOKER1","G_JOKER2","G_JOKER3","G_JOKER4","G_JOKER5","G_JOKER6",
                  "GBEMERKUNG","SCHUTZSTAT","GEPLANT","TK25_NR","TK25_TEIL","G_UTMZ",
                  "G_UTMR","G_UTMH","GKK_R","GKK_H","UNSCHAERFE","G_LONGIT","G_LATIT",
                  "G_LZT_AEND","G_GSAFETY","G_EING_DAT","G_LAGEBEST", 
                  "G_EINGABE","G_IMPQUELL","G_AENDERNG","UTM","MINUTEN_O","MINUTEN_N")

  # fill columns by replacing NAs---------------------------------------------
  # date
  tab$DATUM_EXP <- paste0(x$date)
  tab$DATUM_INT <- paste0(substr(x$date[1], 9, 10),".", 
                          substr(x$date[1], 6, 7), ".",
                          substr(x$date[1], 1, 4))
  
  ## counts ## 
  tab$ANZ_GESAMT <- as.numeric(as.character(x$count))
  
  # add number of males
  rx <- regexpr(pattern = paste0("[0-9]","+(?=x M?nnchen)"), text = x$detail, 
                perl = T)
  tab$ANZ_M <- as.numeric(as.character(substring(x$detail, rx, rx+attr(rx, "match.length")-1)))
  tab$ANZ_M <- ifelse(is.na(tab$ANZ_M), 0, tab$ANZ_M)
  
  # add number of females
  rx <- regexpr(pattern = paste0("[0-9]","+(?=x Weibchen)"), text = x$detail, 
                perl = T)
  tab$ANZ_W <- as.numeric(as.character(substring(x$detail, rx, rx+attr(rx, "match.length")-1)))
  tab$ANZ_W <- ifelse(is.na(tab$ANZ_W), 0, tab$ANZ_W)
  
  # add number of juveniles
  juvi <- c(paste0("[0-9]","+(?=x immature)"),
            paste0("[0-9]","+(?=x Männchen immatur)"),
            paste0("[0-9]","+(?=x Weibchen immatur)"),
            paste0("[0-9]","+(?=x weibchenfarbige immature)"),
            paste0("[0-9]","+(?=x 1. KJ / diesj?hrige)"),
            paste0("[0-9]","+(?=x 2. KJ / vorjährige)"),
            paste0("[0-9]","+(?=x 3. KJ)"),
            paste0("[0-9]","+(?=x 4. KJ)"),
            paste0("[0-9]","+(?=x 5. KJ)"),
            paste0("[0-9]","+(?=x 6. KJ)"),
            paste0("[0-9]","+(?=x Pulli / nicht-flügge)"),
            paste0("[0-9]","+(?=x weibchenfarbige Pulli / nicht-flügge)"),
            paste0("[0-9]","+(?=x Pullus / nicht-flügge)"))

  grepJuvi <- function(w){
    # w <- juvi[1]
    w <- regexpr(pattern = w, text = x$detail, perl = T)
    w <- as.numeric(as.character(substring(x$detail, w, w+attr(w, "match.length")-1)))
    w <- ifelse(is.na(w), 0, w)
  }
            
  rx <- sapply(juvi, grepJuvi)
  tab$ANZ_JUV <- rowSums(rx)
  rm(rx, juvi, grepJuvi)
  
  # add merged count string
  tab$ANZAHL <- paste0(tab$ANZ_M,",", tab$ANZ_W, "+", tab$ANZ_JUV)
  
  # replace 0 in "tab$anzahl" by count from tab$ANZ_GESAMT
  tab$ANZAHL <- ifelse(tab$ANZ_M + tab$ANZ_W + tab$ANZ_JUV == 0, 
                       tab$ANZ_GESAMT, tab$ANZAHL)
  
  tab$ANZ_UNBEST <- tab$ANZ_GESAMT - (tab$ANZ_M + tab$ANZ_W + tab$ANZ_JUV)
  
  ##checkpoint species name #####################################################
  print(paste(unique(x$vernacular.name)[!unique(x$vernacular.name) %in% natis_ref$OR_NAME],
              "are not in imported. May check reference list."))
  
  ## observation and species name##
  tab$K_ART_KEY <- natis_ref$ART_KEY[match(x$vernacular.name, natis_ref$OR_NAME)]
  tab$DT_NAME <- natis_ref$DT_NAME[match(x$vernacular.name, natis_ref$OR_NAME)]
  
  tab$KLASSE <- natis_ref$KLASSE[match(x$vernacular.name, natis_ref$OR_NAME)]
  tab$ORDNUNG <- natis_ref$ORDNUNG[match(x$vernacular.name, natis_ref$OR_NAME)]
  tab$FAMILIE <- natis_ref$FAMILIE[match(x$vernacular.name, natis_ref$OR_NAME)]
  tab$GATTUNG <- natis_ref$GATTUNG[match(x$vernacular.name, natis_ref$OR_NAME)]
  tab$ART <- natis_ref$ART[match(x$vernacular.name, natis_ref$OR_NAME)]
  tab$UNTERART <- natis_ref$UNTERART[match(x$vernacular.name, natis_ref$OR_NAME)]
  tab$ERSTBESCHR <- natis_ref$ERSTBESCHR[match(x$vernacular.name, natis_ref$OR_NAME)]
  tab$SYNONYM <- natis_ref$SYNONYM[match(x$vernacular.name, natis_ref$OR_NAME)]
  tab$ABEMERKUNG <- natis_ref$ABEMERKUNG[match(x$vernacular.name, natis_ref$OR_NAME)]
  
  tab$RL_BRD <- natis_ref$RL_BRD[match(x$vernacular.name, natis_ref$OR_NAME)]
  tab$RL_LAND <- natis_ref$RL_LAND[match(x$vernacular.name, natis_ref$OR_NAME)]
  tab$RL_REGION <- natis_ref$RL_REGION[match(x$vernacular.name, natis_ref$OR_NAME)]
  
  tab$A_JOKER1 <- natis_ref$A_JOKER1[match(x$vernacular.name, natis_ref$OR_NAME)]
  tab$A_JOKER2 <- natis_ref$A_JOKER2[match(x$vernacular.name, natis_ref$OR_NAME)]
  tab$A_JOKER3 <- natis_ref$A_JOKER3[match(x$vernacular.name, natis_ref$OR_NAME)]
  
  tab$A_LZT_AEND <- natis_ref$A_LZT_AEND[match(x$vernacular.name, natis_ref$OR_NAME)]
  tab$K_ASAFETY <- natis_ref$A_ASAFETY[match(x$vernacular.name, natis_ref$OR_NAME)]
  tab$A_EING_DAT <- natis_ref$A_EING_DAT[match(x$vernacular.name, natis_ref$OR_NAME)]
  tab$A_AENDERNG <- natis_ref$A_AENDERNG[match(x$vernacular.name, natis_ref$OR_NAME)]
  
  ## stadium ##
  tab$STADIUM <- x$detail
  tab$STATUS <- x$atlas_code
  tab$K_ERF_METH <- "nicht bekannt"
  
  ## coordinates ##
  tab$K_UTMR <- x$coord_E
  tab$K_UTMH <- x$coord_N
  tab$K_UTMZ <- paste0(x$coord_F,"U")
  
  x$x <- as.numeric(as.character(x$x))
  x$y <- as.numeric(as.character(x$y))
  
  coords <- SpatialPoints(coords = x[,c("x","y")], 
                proj4string = CRS(" +proj=longlat +ellps=WGS84"))
  coords <- spTransform(coords, CRS("+proj=tmerc +lat_0=0 +lon_0=9 +k=1 +x_0=3500000 +y_0=0 +ellps=bessel +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7 +units=m +no_defs"))
  coords <- as.data.frame(coords)
  tab$GKK_RP <- coords[,1]
  tab$GKK_HP <- coords[,2]
  
  tab$K_LONGIT <- x$lon_dms
  tab$K_LATIT <- x$lat_dms
  
  rm(coords)
  
  ## place ##
  tab$KREIS <- x$county
  tab$LAND <- x$country
  tab$GEBIET <- x$place
  tab$GEBIET_NR <- x$id_place
  
  rx <- regexpr("\\((.*)\\)", x$municipality)
  tab$GEMEINDE <- substring(x$municipality, 1, rx-2)

  ## eingabe ##
  tab$ERFASSER <- tab$BESTIMMER <- paste(x$name, x$surname)
  tab$PROJEKT <- "Ornitho Beobachtung"
  tab$K_AENDERNG <- "natis_schnittstelle"
  tab$KBEMERKUNG <- x$comment
  tab$EING_DAT <- x$insert_date
  tab$K_LZT_AEND <- as.character(Sys.time())
  tab$K_EINGABE <- paste(x$name, x$surname)
  
  ## others ## 
  tab$K_IMPQUELL <- ","

  # kick nas
  tab <- tab[!is.na(tab$K_ART_KEY),]
  
  write.dbf(dataframe = tab, file = file)
}
