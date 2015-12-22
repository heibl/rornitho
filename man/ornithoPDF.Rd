\name{ornithoPDF}
\alias{ornithoPDF}
\title{
PDF Graphics Device
}
\description{
Produces PDF graphics of \bold{ornitho} monitoring data.
}
\usage{
ornithoPDF(x, map, file = "ornitho.pdf", show.hidden = FALSE,
    rasterize = 100, raster.model = "maxInd", from = "Jan", 
    to = "Dec", col = c("yellow", "red"), gridcol = "grey25", 
    reporter = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a list, whose elements are objects of class \code{\link{ornithoSpec}}.
}
\item{map}{an object of class \code{\link{ornithoMap}}.
}
  \item{file}{a character string giving the name of the file.
}
  \item{show.hidden}{
logical, indicating if restricted ("hidden") data should be shown, defaults to \code{FALSE}.
}
  \item{rasterize}{
a numeric giving a threshold number of observations. The coordinates of species that have \code{rasterize} or less observations are shown as points. If more observations are available, an index of abundance per grid cell according to a \code{raster.model} is calculated and displayed.
}
  \item{raster.model}{
a character string that gives the raster model that will be applied for species with more than \code{rasterize} observations; the choices are \code{"sumObs"}, \code{"sumInd"}, and \code{"maxInd"} (see \code{\link{plotOrnithoRaster}}).
}
\item{from, to}{
  a character string giving one of \code{"Jan"}, \code{"Feb"}, \code{"Mar"}, \code{"Apr"}, \code{"May"}, \code{"Jun"}, \code{"Jul"}, \code{"Aug"}, \code{"Sep"}, \code{"Oct"}, \code{"Nov"}, or \code{"Dec"} (see \code{\link{plotOrnithoPheno}} for details).
  }
\item{col}{
vector of any of the three kinds of R color specifications, i.e., either a color name (as listed by \code{\link{colors}()}), a hexadecimal string of the form "#rrggbb" or "#rrggbbaa" (see \code{\link{rgb}}), or a positive integer i meaning \code{\link{palette}()[i]}. The given colors are used to interpolate a nine-level color palette for plotting.
}
\item{gridcol}{a character string giving the color of the grid for plotting; setting \code{gridcol = NA} suppresses the plotting of the grid.
}
\item{reporter}{
logical, indicating if the name of the reporter should be included in the tables summarizing the observations on the second page of each species account.
}
}
\details{The PDF produced will contain two pages for each species. The first page gives graphics showing the geographical, saisonal, and altitudinal distribution of the species. The second page contains one or serval tables, depending on the species' status, with details on abundance and timing of observations.}
\author{Christoph Heibl}
\references{
The German \bold{ornitho} website: \url{http://www.ornitho.de/}
}
\seealso{
\code{\link{read.ornitho}} for importing \bold{ornitho}
monitoring data.}
%\examples{}
