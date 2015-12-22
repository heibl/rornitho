\name{plotOrnithoAltitude}
\alias{plotOrnithoAltitude}
\title{Histograms of Altitudinal Distribution}
\description{
Plots histograms of the altitudinal distribution of bird observations from \bold{ornitho} monitoring data.
}
\usage{
plotOrnithoAltitude(x, background,
  alt.int = 50, alt.min = 0, alt.max = 1000, 
  col = "grey75")
}
\arguments{
\item{x}{
an object of class \code{ornithoSpec}.
}
\item{background}{numeric, a vector of integers that are representative for the distribution of the altitude of the samples. Used to compare the altitudinal distribution of a species samples to the altitudinal distribution sampled.  
}
\item{alt.int}{
numeric, giving an interval (in meters) for summarizing observations.
}
\item{alt.min}{
numeric, giving the minimal altitude (in meters) for scaling the altitude histogram.
}
\item{alt.max}{
numeric, givint the maximal altitude (in meters) for scaling the altitude histogram.
}
\item{col}{
any valid specification of a color to fill the bars of the plot.
}
}
\author{Simon Thorn & Christoph Heibl}
\seealso{
\code{\link{ornithoPDF}} and \code{\link{plotOrnitho}} for the corresponding user-level plotting functions.
}
\examples{
data(amsel)
plotOrnithoAltitude(amsel)
}


