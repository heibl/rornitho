\name{getOpenMap}
\alias{getOpenMap}
\encoding{UTF-8}
\title{Get an open map}
\description{
This function crops the base map to a given political border, such as a county name.
}

\usage{
getOpenMap(border, type)
}

\arguments{
\item{border}{
an object of class spatial Polygons.
}

\item{type}{
a character string giving the name of the openmap that should be downloaded. See function openmap for details.
}
}

\value{
an object of class OpenMap. Note that this map is projected.
}
\author{Simon Thorn}
\references{
The German \bold{ornitho} website: \url{http://www.ornitho.de/}
}


