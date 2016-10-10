## This code is part of the ornitho package
## © S. Thorn 2016 (last update 2016-02-05)

crop.border <- function(what = c("country", "county", "subcounty"), to){
  
  # load underlying border
  data("border", envir = environment())
  
  # define outer border of plotting region-------------------------------------
  if(what == "country")
    index <- border$country %in% to
     
  if(what == "county")
    index <- border$land %in% to

  if(what == "subcounty")
    index <- border$kurz %in% to

  index[which(!index)] <- NA
  outer_border <- unionSpatialPolygons(border, index)
  
  outer_border
}