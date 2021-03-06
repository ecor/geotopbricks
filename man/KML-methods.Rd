% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/KML.GeotopRasterBrick.R
\docType{methods}
\name{KML}
\alias{KML}
\alias{KML,GeotopRasterBrick-method}
\title{KML}
\usage{
\S4method{KML}{GeotopRasterBrick}(
  x,
  filename,
  crs = as.character("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"),
  ...
)
}
\arguments{
\item{x}{the \code{\link{GeotopRasterBrick}} object}

\item{filename}{mane of the KML file to produce}

\item{crs}{character string containg the LatLon reference system. Default is \code{"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"} (see \url{http://spatialreference.org/ref/epsg/4326/}).}

\item{...}{further argument for S4 method \code{KLM} for Raster object.}
}
\description{
\code{KML} method for a \code{GeotopRasterBrick} object
}
\note{
A coordinate transformation is made with \code{\link{projectRaster}}.
}
