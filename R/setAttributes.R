
setAttributes <- function(file){
    
    x <- read.csv2(file = file, as.is = TRUE, sep = "\t", fill = TRUE, 
                   nrows = 1, quote = "", dec = ".", encoding = "latin1")
    x <- names(x)

    data("attribute", envir = environment())
    depCols <- setdiff(attribute$names, x)
    newCols <- setdiff(x, attribute$names)

    if (length(depCols) > 0) {
        cat("delete deprecated columns:", paste(depCols, collapse = " "), 
            "\n")
        depCols <- attribute$names %in% depCols
        attribute <- attribute[!depCols, ]
    }
    if (length(newCols) > 0) {
        cat("add new columns:", paste(newCols, collapse = " "))
        newCols <- data.frame(names = newCols, alias = tolower(newCols), 
            class = "character",)
        attribute <- rbind(attribute, newCols)
        attribute <- attribute[match(x, attribute$names), ]
    }
    attribute
}