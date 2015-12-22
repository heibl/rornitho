\name{ornithoSpec-class}
\docType{class}
\alias{ornithoSpec-class}
\alias{ornithoSpec}
\title{Class "ornithoSpec"}
\description{
Class for holding \bold{ornitho} monitoring data.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{ornithoSpec(x, def)}.
}
%% Slots
\section{Slots}{
 \describe{
 \item{scientific.name}{
 a vector of mode \code{character} holding the species' scientific name.
 }
 \item{vernacular.name}{
a vector of mode \code{character} holding the species' German vernacular name.
}
  \item{phenology}{
a vector of mode \code{character} holding the species' migrational status.
}
  \item{observations}{
a data.frame with the observations consisting of geographical coordinates and their attribute data.
}
 }
}
%% \section{Extends}{Class \code{"locus"}, directly.}
%% \details{}
%% \references{}
\author{Christoph Heibl}
%%\note{}
\section{seealso}{
\code{\link{read.ornitho}} to import and \code{\link{ornithoPDF}} to plot \bold{ornitho} monitoring data.
}
%\examples{}
