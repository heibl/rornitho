\name{ornitho2df}
\alias{ornitho2df}
\alias{df2ornitho}
\title{
Conversion between Lists and Data Frames
}
\description{
A list of \code{ornithoSpec} objects is converted into a data frame and vice versa.
}
\usage{
ornitho2df(ornitho)

df2ornitho(df)
}
\arguments{
  \item{ornitho}{
a list, whose elements are objects of class \code{\link{ornithoSpec}}.
}
\item{df}{a data frame, must contain columns named \code{"scientific.name"}, \code{"vernacular.name"}, and \code{"phenology"}.
}
}
%\details{}
\value{
a data frame with 29 columns:
\item{scientific.name}{the scientific name}
\item{vernacular.name}{the vernacular name (in German)}
\item{phenology}{phenology, one of "Standvogel", "Wintergast", "Teil- und Kurzstreckenzieher", "Langstreckenzieher",  "Durchzuegler", "Seltenheit".}
\item{date}{}
\item{day}{}
\item{month}{}
\item{year}{}
\item{yday}{}
\item{pentade}{}
\item{decade}{}
\item{week}{}
\item{place}{}
\item{county}{}
\item{country}{}
\item{x}{}
\item{y}{}
\item{count}{}
\item{detail}{}
}
\references{
The German \bold{ornitho} website: \url{http://www.ornitho.de/}
}
\author{
Christoph Heibl
}
\seealso{
\code{\link{read.ornitho}} for reading \bold{ornitho} data; \code{\link{ornithoPDF}} for plotting \bold{ornitho} data.
}
\examples{
data(amsel)
amsel.df <- ornitho2df(amsel)
table(amsel.df$scientific.name)
df2ornitho(amsel.df)
}

