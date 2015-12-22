\name{plotOrnitho}
\alias{plotOrnitho}
\title{Plot Ornitho Data}
\description{
This function calls a series of plotting functions to generate a multi-facetted plot of \bold{ornitho} monitoring data.
}
\usage{
plotOrnitho(x, map, show.hidden = FALSE, rasterize = 100, 
    raster.model = raster.model, from = "Jan", to = "Dec",
    col = c("yellow", "red"), gridcol = "grey25", 
    missing.obs, alt.background,
    reporter = TRUE)
}
\arguments{
  \item{x}{
an object of class \code{ornithoSpec}.
  }
  \item{map}{
an object of class \code{ornithoMap}.
  }
  \item{show.hidden}{
logical, indicating if restricted ("hidden") data should be shown, defaults to \code{FALSE}.
}
  \item{rasterize}{
a numeric giving a threshold number of observations. The coordinates of species that have \code{rasterize} or less observations are shown as points. If more observations are available, an index of abundance per grid cell according to a \code{raster.model} is calculated and displayed.
  }
  \item{from, to}{
  a character string giving one of \code{"Jan"}, \code{"Feb"}, \code{"Mar"}, \code{"Apr"}, \code{"May"}, \code{"Jun"}, \code{"Jul"}, \code{"Aug"}, \code{"Sep"}, \code{"Oct"}, \code{"Nov"}, or \code{"Dec"} (see \code{\link{plotOrnithoPheno}} for details).
  }
  \item{raster.model}{
a character string that gives the raster model that will be applied for species with more than \code{rasterize} observations; the choices are \code{"sumObs"}, \code{"sumInd"}, and \code{"maxInd"} (see \code{\link{plotOrnithoRaster}}).
}
\item{col}{
vector of any of the three kinds of R color specifications, i.e., either a color name (as listed by \code{\link{colors}()}), a hexadecimal string of the form "#rrggbb" or "#rrggbbaa" (see \code{\link{rgb}}), or a positive integer i meaning \code{\link{palette}()[i]}. The given colors are used to interpolate a nine-level color palette for plotting.
}
\item{gridcol}{a character string giving the color of the grid for plotting; setting \code{gridcol = NA} suppresses the plotting of the grid.
}
\item{missing.obs}{an object of class \code{SpatialPoints} that gives the coordinates of the centroids of those grid cells that have not been sampled and that will be marked by a cross.

}
\item{alt.background}{numeric, a vector of integers that are representative for the distribution of the altitude of the samples. Used to compare the altitudinal distribution of a species samples to the altitudinal distribution sampled.  
}
\item{reporter}{
logical, indicating if the name of the reporter should be included in the tables summarizing the observations on the second page of each species account.
}
}
%\details{}
%\value{}
%\references{}
\author{Christoph Heibl}
\seealso{
\code{\link{ornithoPDF}}
}
%\examples{}
