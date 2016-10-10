## This code is part of the ornitho package
## © C. Heibl 2015 (last update 2016-02-05)

readOrnithoSingleFile <- function(file, attribute){
  
  cat("\nreading:", file)
  
  ## get column names as the appear in the file:
  ## -------------------------------------------
  x <- read.csv2(file = file, as.is = TRUE, sep = "\t", 
                 fill = TRUE, 
                 nrows = 1, quote = "", dec = ".", 
                 encoding = "latin1")
  x <- names(x)
  
  ## load the in-build column headers:
  if (missing(attribute) ) data("attribute", envir = environment())
  
#   ## find column names that are not in-build
#   ## add add them to the data frame:
#   ## ------------------------------
#   newCols <- setdiff(x, attribute$names)
#   if (length(newCols) > 0) {
#     cat("\nadd new columns:", paste(newCols, collapse = " "))
#     newCols <- data.frame(names = newCols, alias = tolower(newCols), 
#                           class = "NULL", 
#                           order = nrow(attribute) + (1:length(newCols)))
#     attribute <- rbind(attribute, newCols)
#     attribute <- attribute[match(x, attribute$names), ]
#   }
  
  ## find column names that are not present in the file
  ## and delete them from the attribute table
  ## ----------------------------------------
  missCols <- setdiff(attribute$names, x)
  if ( length(missCols) > 0 ) {
#     cat("\ndelete deprecated columns:", 
#         paste(missCols, collapse = " "))
    fileCols <- attribute[!attribute$names %in% missCols, ]
  } 
  
  if ( length(missCols) == 0 ) {
    fileCols <- attribute
  }
  
  ## reorder attribute data frame according to file:
  fileCols <- fileCols[match(x, fileCols$name), ]
  
  ## read and return ornitho data
  obj <- read.csv2(file, as.is = TRUE, 
                 sep = "\t", fill = TRUE,
                 skip = 2, quote = "", dec = ".",
                 col.names = fileCols$alias, 
                 colClasses = fileCols$class,
                 encoding = "latin1")
  
  missCols <- attribute[attribute$names %in% missCols & attribute$class != 'NULL',
                           "alias"]
  
  if ( length(missCols) > 0 ) {
    m <- matrix(nrow = nrow(obj), ncol = length(missCols))
    m <- as.data.frame(m)
    names(m) <- missCols
    obj <- cbind(obj, m)
  }
  
  # compile correct order and return data
  index <- match(attribute$alias, names(obj))
  index <- index[!is.na(index)]
  obj[,index]
  
}