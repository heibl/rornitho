df2ornitho <- function(df){
  
  ## checks and definitions
  ## ----------------------
  if ( !is.data.frame(df) ) 
    stop("'df' is not a data frame")
  cols <- c("scientific.name",
            "vernacular.name",
            "phenology")
  if ( !all(cols %in% names(df)) ){
    stop("'df' is not well-formatted")
  }
  ## split data frame and convert to list of
  ## ornithoSpec elements
  ## --------------------
  df <- split(df, f = df$scientific.name)
  data(def, envir = environment())
  df <- lapply(df, ornithoSpec, def = def)
  
  ## order according to linear classification
  ## ----------------------------------------
  def <- def[def$latin %in% names(df), ]
  df[match(def$latin, names(df))]
}