\name{getProjection}
\alias{getProjection}
\title{It reads the CRS metadata utilzed in a GEOtop Simulation}
\usage{
getProjection(x, cond = TRUE, ...)
}
\arguments{
  \item{x}{name and full path of the file containimg CRS
  information}

  \item{cond}{logical value. If \code{FALSE} the function
  returns \code{NA}. Default is \code{TRUE}.}

  \item{...}{futher arguments}
}
\value{
A string corresponding the projection and CRS if the
argument \code{cond} is \code{TRUE}.
}
\description{
It reads the CRS metadata utilzed in a GEOtop Simulation
}
\examples{
library(geotopbricks)
wpath <- "http://www.boussinesq.org/geotopbricks/simulations/idroclim_test1"
x <- paste(wpath,"geotop.proj",sep="/")


crs <- getProjection(x)
}

