\name{vertical.aggregate.brick.within.depth}
\alias{vertical.aggregate.brick.within.depth}
\title{Aggregates with a mean or an addition on the vertical profile the values of a 'brick' within a certain depth  and returns the vertical aggregated map}
\usage{
vertical.aggregate.brick.within.depth(x, depth = NULL, layers = NULL,
  i0 = NULL, verify = FALSE, FUN = identity, divide.by.depth = FALSE,
  ...)
}
\arguments{
  \item{x}{a 'RasterBrick' or a three-dimensional array}

  \item{depth}{depth map, generally a 'RasterLayer' object}

  \item{layers}{vector of layer thickness}

  \item{i0}{a 'Raster' containing the number of soil laver
  just over the bedrock. Default is \code{NULL} and is then
  calculated.}

  \item{verify}{logical. Default is \code{FALSE}. If it is
  \code{TRUE}, it verifies that function is working
  correctly.}

  \item{FUN}{function used for aggregation. If missing,
  \code{\link{identity}} is the default value.}

  \item{divide.by.depth}{logical. If \code{TRUE} the
  function returns the 'mean' value, otherwise a a cumulate
  value. Default is \code{FALSE}.}

  \item{...}{further argument for \code{FUN}}
}
\value{
a list of 'Raster' maps:

\code{i0} a 'Raster' containing the number of soil laver
just over the bedrock

\code{z0} a 'Raster' containing the depth of the center of
the \code{i0}-th layer

\code{result} a 'Raster' containing the aggregated map
}
\description{
Aggregates with a mean or an addition on the vertical
profile the values of a 'brick' within a certain depth and
returns the vertical aggregated map
}
\note{
\code{x} and \code{depth} or \code{i0} must cover the same
spatial region.
}
\examples{
library(geotopbricks)
# The examples is the following R script conteined
# in a 'inst' directory of the package source
f <- system.file("doc/examples/example.vertical.aggregate.brick.within.depth.R",
package="geotopbricks")
#  source(f) # Uncomment this line to run the example.
# You can copy the example file using file.copy(from=f,to=....,...) See file.copy documentation
}
\seealso{
\code{\link{getvalues.brick.at.depth}},\code{\link{brick}}
}

