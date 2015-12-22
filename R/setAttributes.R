## This code is part of the ornitho package
## © C. Heibl 2015 (last update 2015-11-25)

setAttributes <- function(file){
  
  x <- lapply(file, read.csv2, as.is = TRUE, 
              sep = "\t", fill = TRUE,
              nrows = 1, quote = "", dec = ".",
              encoding = "latin1")
  x <- lapply(x, function(z) names(z))
  x <- unique(x)
  # x <- tail(x, 1)
  
  ## check for different headers
  ## ---------------------------
  if ( length(x) > 1 ) {
    xx <- lapply(x, function(z) sort(z))
    xx <- unique(xx)
    if ( length(xx) ==  1 ) {
      stop("different ordering of columns")
    } else {
      stop("found different headers")
    }
  } else {
    x <- unlist(x)
  }
    
  ## match current header with definition
  ## ------------------------------------
  data("attribute", envir = environment())
  depCols <- setdiff(attribute$names, x)
  newCols <- setdiff(x, attribute$names)
  
  ## delete deprecated columns
  ## -------------------------
  if ( length(depCols) > 0 ){
    cat("delete deprecated columns:", 
        paste(depCols, collapse = " "), "\n")
    depCols <- attribute$names  %in% depCols
    attribute <- attribute[!depCols, ] 
  }
    
  ## add new columns
  ## -------------------------
  if ( length(newCols) > 0 ){
    cat("add new columns:", 
        paste(newCols, collapse = " "))
    newCols <- data.frame(names = newCols,
                          alias = tolower(newCols), 
                          class = "character")
    attribute <- rbind(attribute, newCols)
    attribute <- attribute[match(x, attribute$names), ]
  }
  attribute
}