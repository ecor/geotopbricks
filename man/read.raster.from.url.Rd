\name{read.raster.from.url}
\alias{read.raster.from.url}
\title{It imports a 'RasterLayer' object in Escri-Asci format from a URL 'http://....<FILENAME>.asc}
\usage{
read.raster.from.url(x, header_nrow = 6, ...)
}
\arguments{
  \item{x}{the charcater string containing the URL address}

  \item{header_nrow}{Number of header in the ASCII grid
  format. Deafault is 6. See
  \url{http://en.wikipedia.org/wiki/Esri_grid}}

  \item{...}{additional arguments}
}
\value{
a 'RasterLayer' object
}
\description{
It imports a 'RasterLayer' object in Escri-Asci format from
a URL 'http://....<FILENAME>.asc
}
\note{
This function reads a local or remote text files formatted
as \url{http://en.wikipedia.org/wiki/Esri_grid} and creates
a 'RasterLayer' object.
}
\seealso{
\code{\link{raster}},\code{\link{readLines}}
}

