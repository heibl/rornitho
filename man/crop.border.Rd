\name{crop.border}
\alias{crop.border}
\encoding{UTF-8}
\title{Get outer border of plotting region}
\description{
This function crops the base map to a given political border, such as a county name.
}

\usage{
crop.border(to = "HE")
}

\arguments{
\item{what}{
a character string giving the level of political border to crop. Could be one of country, county or subcounty.}

\item{to}{
a character string giving the name of the region to crop the outer map. Could be either a country name (currently only GER implemented), county or subcounty name.}

}

\value{
a spatial polygon representing the outer border of plotting region.
}
\author{Simon Thorn}
\references{
The German \bold{ornitho} website: \url{http://www.ornitho.de/}
}


