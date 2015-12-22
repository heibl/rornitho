\name{plotOrnithoPheno}
\alias{plotOrnithoPheno}
\title{Plot Phenology of Bird Observations}
\description{
Plots phenology of bird observations based on count maxima per sites and number of observations
}
\usage{
plotOrnithoPheno(x, unit = "decade", from = "Jan", 
    to = "Dec", col)
}
\arguments{
  \item{x}{
an object of class \code{\link{ornithoSpec}}.
  }
  \item{unit}{
a time unit in which to devide the plot, either "decade" or "pentade", the default is "decade".
  }
  \item{from, to}{
  a character string giving one of \code{"Jan"}, \code{"Feb"}, \code{"Mar"}, \code{"Apr"}, \code{"May"}, \code{"Jun"}, \code{"Jul"}, \code{"Aug"}, \code{"Sep"}, \code{"Oct"}, \code{"Nov"}, or \code{"Dec"} (see Details).
  }
%  \item{to}{
%  a character string giving one of \code{"Jan"}, \code{"Feb"}, \code{"Mar"}, \code{"Apr"}, \code{"May"}, \code{"Jun"}, \code{"Jul"}, \code{"Aug"}, \code{"Sep"}, \code{"Oct"}, \code{"Nov"}, or \code{"Dec"}.
%  }
  \item{col}{a nine-level color palette, the third and sixth levels are used to plot, respectively. The default are grey levels returned by \code{\link{gray}}. 
  }
}
\details{
Arguments \code{from} and \code{to} can be used to subset or shift the order of months in the plot. To display only the breeding season of a set of species one could choose \code{from = "Mar"} and \code{to = "Jun"}. For wintering birds it might be useful to shift the plot by six months: \code{from = "Jul"} and \code{"Jun"}.
}
\author{Simon Thorn and Christoph Heibl}
\examples{
data(amsel)
plotOrnithoPheno(amsel)
}
