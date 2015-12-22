\name{read.ornitho}
\alias{read.ornitho}
\encoding{UTF-8}
\title{Read ORNITHO Data}
\description{
Read in the output of the online export from the \bold{ornitho} homepage based on \code{\link{read.csv2}}.
}
\usage{
read.ornitho(file, subsp = NULL, family = NULL, 
    phenology = NULL, rm.abs = TRUE)
}
\arguments{
\item{file}{
a character string giving the name of the (plain text) file including the suffix ".txt".
}
\item{subsp}{a character string giving the latin binomial (e.g. \code{"Carduelis flammea"}) of the species that should be treated on a subspecies level. Note that this will omit all those observations that are not determined to subspecies level.}
\item{family}{a character string giving the families the set of species should be limited to.}
\item{phenology}{a character string giving the phenological behaviour the set of species should be limited to. Possible choices are \code{"Standvogel"}, \code{"Wintergast"}, \code{"Seltenheit"}, \code{"Durchzügler"}, \code{"Teil- und Kurzstreckenzieher"}, \code{"Langstreckenzieher"}, or any combination thereof.
}
\item{rm.abs}{
logical, if \code{TRUE} (default) observations with 0 number of individuals are removed.
}
}
\details{
\code{read.ornitho} matches the scientific names found in the plain text file with a set of 'canonical' names defined in \code{data(def)}. Other scientific names encountered are mostly not determined to the species level and will be ignored. If this happen, \code{read.ornitho} will issue a message and print a file names \code{"ignored.txt"} to the current working directory with the scientific and vernacular names of the ignored, ie. removed, observations.}
\value{
a list whose elements are objects of class \code{ornithoSpec.}
}
\author{Simon Thorn & Christoph Heibl}
\references{
The German \bold{ornitho} website: \url{http://www.ornitho.de/}
}
\seealso{\code{\link{ornithoPDF}} for the user-level plotting function.}
%\examples{}


