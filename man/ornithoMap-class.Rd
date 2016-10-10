\name{ornithoMap-class}
\docType{class}
\alias{ornithoMap-class}
\title{Class "ornithoMap"}
\description{
Class for holding topographical data.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{\link{ornithoMap}(what, to, type, grid, grid4, river, lake)}.
}
%% Slots
\section{Slots}{
 \describe{

\item{aerial}{an object of class \code{\link[OpenStreetMap]{OpenStreetMap}} represents an open arial map for border extend.}
    
 \item{grid}{an object of class \code{\link[sp]{SpatialPolygons}} representing the grid of 1:25.000 topographical maps (often called "Messtischblaetter").}
 
 \item{grid4}{an object of class \code{\link[sp]{SpatialPolygons}} representing a grid of quarters of \code{grid}.}
 
 \item{river}{an object of class \code{\link[sp]{SpatialLines}} representing flowing water bodies.}
 
 \item{lake}{an object of class \code{\link[sp]{SpatialPolygons}} representing standing water bodies.}
 }
}
%% \section{Extends}{Class \code{"locus"}, directly.}
%% \details{}
%% \references{}
\author{Christoph Heibl & Simon Thorn}
%%\note{}
\section{seealso}{
\code{\link{readOrnitho}} to import and \code{\link{ornithoPDF}} to plot \bold{ornitho} monitoring data.
}
%\examples{}
