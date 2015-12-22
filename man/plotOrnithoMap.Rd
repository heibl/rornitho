\name{plotOrnithoMap}
\alias{plotOrnithoMap}
\title{Basemap with County Borders, Lakes and Rivers}
\description{
Plots a basemap consisting of administrative borders, lakes and rivers.
}
\usage{
plotOrnithoMap(x, add = FALSE, 
  maps = c("border", "lake", "river"),     
  border.col = "saddlebrown", water.col = "blue", ...)
}
\arguments{
\item{x}{
a characater string giving the region, for which a basemap should be plotted, currently only "hessen" available.
}
\item{add}{
logical, should the basemap override any previous plotting?
}
\item{maps}{

}
\item{border.col}{
a character string giving the color to be used draw political borders.
}
\item{water.col}{
a character string giving the color to be used draw water bodies.
}
\item{...}{further line characteristics, such as \code{lwd} or \code{col}.
}
}
\author{Simon Thorn & Christoph Heibl}
\seealso{
\code{\link{ornithoPDF}} and \code{\link{plotOrnitho}} for the corresponding user-level plotting functions.
}
\examples{
## load map data
data(hessen_map)

## plot map
opar <- par()$mar
par(mar = rep(0, 4))
plotOrnithoMap(hessen_map)
par(mar = opar)
}

