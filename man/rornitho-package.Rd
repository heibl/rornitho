\name{rornitho-package}
\alias{rornitho-package}
\alias{ornitho}
\alias{rornitho}
\alias{Rornitho}
\alias{rOrnitho}
\docType{package}
\encoding{UTF-8}
\title{Ornitho Data in R}
\description{
Read bird observation data from the `ornitho.de' project database and produce graphics suitable for bird atlasses.
}
\details{
\tabular{ll}{
Package: \tab rornitho\cr
Type: \tab Package\cr
Version: \tab 1.0-2\cr
 Date: \tab 2015-11-06\cr
License: \tab GPL (>= 2)\cr
}
The \bold{rornitho} package offers three sets of functions:
\enumerate{
  \item \bold{Import and formating of ornitho data} - Data can be imported with \code{\link{read.ornitho}}. This gives a list of objects of class \code{\link[=ornithoSpec-class]{ornithoSpec}}, which can be tranformed to a data frame with \code{\link{ornitho2df}} and back to a list with \code{\link{df2ornitho}}.
  
  \item \bold{Preparing maps for visualization} - The function \code{\link{ornithoMap}} bundles maps of topographical features like political borders, water bodies and grids into a single, coherent object of class \code{\link[=ornithoMap-class]{ornithoMap}}, which can be reprojected with \code{\link{projOrnithoMap}} and plotted with \code{\link{plotOrnithoMap}}.
  \item \bold{Plotting} - The geographical distribution of bird observations can be plotted either as points (\code{\link{plotOrnithoPoints}}) or rasterized grid (\code{\link{plotOrnithoRaster}}). The functions \code{\link{plotOrnithoAltitude}} and \code{\link{plotOrnithoPheno}} will produce barplots
 of the altitudinal and the phenological distribution of bird observations, respectively. Currently, there is one convenience function for plotting ready-designed two-page summaries suited for bird atlasses: \code{\link{ornithoPDF}}.
}
This work was founded by the Hessische Gesellschaft für Ornithologie und Naturschutz (\url{http://www.hgon.de/}).
}
\author{
Simon Thorn <thorn@hgon.de>, Christoph Heibl <christoph.heibl@gmx.net>
Maintainer: Christoph Heibl <christoph.heibl@gmx.net>
}
\references{
The German \bold{ornitho} website: \url{http://www.ornitho.de/}
}
\keyword{package}
