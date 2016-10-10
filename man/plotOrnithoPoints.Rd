\name{plotOrnithoPoints}
\alias{plotOrnithoPoints}
\title{Add Observations to Basemap}
\description{
Draw localities of bird observations onto a basemap produced by \code{\link{plotOrnithoMap}}.
}
\usage{
plotOrnithoPoints(x, show.hidden = FALSE, col, 
    grid, gridcol = "grey25",cex, ...)
}
\arguments{
\item{x}{
an object of class \code{ornithoSpec}.
}
\item{show.hidden}{
logical, indicating if restricted ("hidden") data should be shown, defaults to \code{FALSE}.
}
\item{grid}{an object of class \code{\link[sp:sp]{SpatialPolygonsDataFrame}} containing the grid that defined the resolution of the raster.
}
\item{gridcol}{a character string giving the color of the grid for plotting; setting \code{gridcol = NA} suppresses the plotting of the grid.
}
\item{col}{a nine-level color palette, the second and eighth levels are used to color non-breeding and breeding code observations, respectively.}
\item{cex}{
numeric, giving the size of points to be plotted, if argument is missing cex size will be abundance weighted.
}
\item{...}{
further arguments passed through function \code{\link{points}}.
}
}
\author{Simon Thorn & Christoph Heibl}
\seealso{
\code{\link{ornithoPDF}} and \code{\link{plotOrnitho}} for the corresponding user-level plotting functions.
}
\examples{
## load map data
data(hessen_map)

## load ornitho data
data(amsel)

opar <- par()$mar
par(mar = rep(0, 4))
plotOrnithoMap(hessen_map)
plotOrnithoPoints(amsel)
par(mar = opar)
}


