\name{geotopbrick}
\alias{geotopbrick}
\alias{geotopbrick.GeotopRasterBrick}
\alias{geotopbrick.RasterBrick}
\alias{geotopbrick.RasterLayer}
\alias{geotopbrick.default}
\alias{geotopbrick.zoo}
\title{geotopbrick}
\usage{
geotopbrick(x = NULL, ...)

\method{geotopbrick}{default}(x, ...)

\method{geotopbrick}{zoo}(x, layer = NULL, time = NULL, crs = NULL,
  timerange = NULL, ...)

\method{geotopbrick}{RasterLayer}(x, layer = NULL, time = NULL,
  ascpath = zoo(NULL), ...)

\method{geotopbrick}{RasterBrick}(x, layer = NULL, time = NULL,
  ascpath = zoo(NULL), ...)

\method{geotopbrick}{GeotopRasterBrick}(x, layer = NULL, time = NULL,
  crs = NULL, timerange = NULL, ascpath = NULL, ...)
}
\arguments{
  \item{x}{a 'zoo' object returned by function
  \code{\link{pointer.to.maps.xyz.time}} or
  \code{\link{pointer.to.maps.xy.time}} or a
  \code{\link{GeotopRasterBrick-class}} object}

  \item{layer}{layer at which raster maps are imported. If
  is \code{NULL}, maps ara no-zlayer distributed and
  \code{zoo} must be returend by
  \code{\link{pointer.to.maps.xy.time}}}

  \item{time}{vector of time instants at which geotop maps
  are imported}

  \item{crs}{coordinate system see
  \code{\link{RasterBrick-class}}}

  \item{timerange}{two-elememts vector containing the time
  range at which geotop maps are imported}

  \item{ascpath}{\code{NULL} object or a \code{"zoo"} S3
  object containing the names of ascii maps provided by
  GEOtop}

  \item{...}{further arguments.}
}
\value{
a \code{\link{GeotopRasterBrick-class}}
}
\description{
geotopbrick

geotopbrick method bla bla bla
}

