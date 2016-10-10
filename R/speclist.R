## This code is part of the ornitho package
## © C. Heibl 2015 (last update 2015-10-27)

speclist <- function(subsp = NULL, family = NULL, 
                     phenology = NULL){
  
  ## load standard species list
  ## ----------------------------
  data(def, envir = environment())

  ## subset to a certain family
  ## -----------------------------
  if ( !is.null(family) ){
    fam <- unique(def$family)
    family <- match.arg(family, fam, several.ok = TRUE)
    def <- def[def$family %in% family, ]
  }
  
  ## subset to a certain phenology
  ## -----------------------------
  if ( !is.null(phenology) ){
    pheno.states <- unique(def$phenology)
    phenology <- match.arg(phenology, pheno.states,
                           several.ok = TRUE)
    def <- def[def$phenology %in% phenology, ]
  }
  
  ## produce vector of species names
  ## that are present in the list as
  ## as single species and with at least
  ## two subspecies
  ## --------------
  sp <- split(def$interspecific_rank, 
             f = paste(def$genus, def$species))
  both <- function(x){
    length(setdiff(x, c("-0", "ssp."))) == 0 & 
      all(c("-0", "ssp.") %in% x) &
      length(x[x == "ssp."]) > 1
  }
  id <- sapply(sp, both)
  sp <- names(sp)[id]
  
  ## elimite species names form vector
  ## that will be treated as subspecies
  ## ----------------------------------
  sp <- setdiff(sp, subsp)
  
  ## subset standard species list
  ## ----------------------------
  if ( length(subsp) > 0 ){
    def <- def[!def$latin %in% subsp, ]
  }
  if ( length(sp) > 0 ){
    id <- intersect(
      grep(paste(sp, collapse = "|"), def$latin), 
      which(def$interspecific_rank == "ssp.")
      )
    def <- def[-id, ] 
  }
  def
}