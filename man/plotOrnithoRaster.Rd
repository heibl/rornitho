\name{plotOrnithoRaster}
\alias{plotOrnithoRaster}
\title{
Plot Rasterized Distribution Data
}
\description{
Rasterize point distribution data and plot the result as heatmap.
}
\usage{
plotOrnithoRaster(x, map, model = "maxInd", 
    col, gridcol = "grey25", legend)
}
\arguments{
  \item{x}{an object of class \code{ornithoSpec}.
}
  \item{map}{an object of class \code{\link[=ornithoMap-class]{ornithoMap}} containing the grid that defines the resolution of the raster.
}
  \item{model}{
a character string that gives the raster model that will be applied for species with more than \code{rasterize} observations; the choices are \code{"sumObs"}, \code{"sumInd"}, and \code{"maxInd"} (see Details).
}
\item{col}{
a nine-level color palette for the frequency classes of the discretized bird count data.
}
  \item{gridcol}{
  a character string giving the color of the grid for plotting; setting \code{gridcol = NA} suppresses the plotting of the grid.
}
\item{legend}{logical indicating if a legend of frequency classes should be plotted.
}
} % end of arguments
%\details{}
%\references{}
\author{Simon Thorn and Christoph Heibl}
\seealso{
\code{\link{plotOrnithoPoints}} for plotting the raw vector data; \code{\link{plotOrnithoMap}} for plotting a base map with topographical features; \code{\link{plotOrnitho}} for the top-level plotting function.
}
\examples{
data(amsel)
data(hessen_map)
par(mfcol = c(1, 3), mar = c(5, 0, 5, 0))
plotOrnithoRaster(amsel, hessen_map, model = "sumObs",
  legend = FALSE)
title(main = "sumObs", 
    sub = "Sum of observations")
plotOrnithoRaster(amsel, hessen_map, model = "sumInd",
  legend = FALSE)
title(main = "sumInd", 
    sub = "Sum of all counts")
plotOrnithoRaster(amsel, hessen_map,
  legend = FALSE)
title(main = "maxInd", 
    sub = "Highest count of individuals")
}
