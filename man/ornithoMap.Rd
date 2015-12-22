\name{ornithoMap}
\alias{ornithoMap}

\title{Preparing vector maps}
\description{
Create an object of class \code{ornithoMap}, which holds vector maps for plotting grids, administrative borders and water bodies together with \bold{ornitho} data.
}
\usage{
ornithoMap(grid4, border, river, lake, district)
}
\arguments{
  \item{grid4}{
an object of class \code{\link[sp]{SpatialPolygons}} representing the grid of 1:25.000 topographical maps (often called "Messtischblaetter").
}
  \item{border}{
an object of class \code{\link[sp]{SpatialPolygons}} representing administrative borders.
}
  \item{river}{
an object of class \code{\link[sp]{SpatialLines}} representing flowing water bodies.
}
  \item{lake}{
an object of class \code{\link[sp]{SpatialPolygons}} representing standing water bodies.
}
  \item{district}{
a character string giving the name of an administrative subunit; may be missing (default) to use the entire area.
}
}
%\details{}
\value{
an object of class \code{ornithoMap}.
}
%\references{}
\author{Christoph Heibl}
\seealso{
a description of the class \code{\link[=ornithoMap-class]{ornithoMap}}; \code{\link{read.ornitho}} for importing and \code{\link{ornithoPDF}} for plotting \bold{ornitho}
monitoring data.
}
\examples{
## load map data
data(hessen_grid)
data(hessen_border)
data(hessen_river)
data(hessen_lake)

## create ornithoMap object
hessen_map <- ornithoMap(
  grid4 = hessen_grid, 
  border = hessen_border,
  river = hessen_river,
  lake = hessen_lake
)

## see a short summary
print(hessen_map)

## plot map
opar <- par()$mar
par(mar = rep(0, 4))
plotOrnithoMap(hessen_map)
par(mar = opar)
}
