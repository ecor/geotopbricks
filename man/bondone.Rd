\name{bondone}
\docType{data}
\alias{meteo}
\alias{bondone}
\title{
Bondene Dataset 
}

\usage{
data(bondone)
}

\description{
It contains hourly meteorological data observed at MeteoTrentino \code{T0327} station located at Monte Bondone-Viotte (Trentino, Easter Alps, Italy) from August 2004 to December 2012.\\

The \code{\link{zoo}} object 'meteo' contains: 
\describe{
\item{\code{Iprec}}{Hourly Precipitation Depth expressed in millimeters}
\cr
\item{\code{AirT}}{Air Temperature expressed in Celsius Degree}
\cr
\item{\code{RH}}{Relative Humidity in PerCent}
\cr
\item{\code{WinDir}}{Wind Direction expressed in Degrees North Clockwise}
\cr
\item{\code{WinSp}}{Wind Direction expressed in meters per second}
\cr
\item{\code{Swglob}}{Short-Wave Radiation expressed in Watts per square meters}
}

The corresponding time axis vector for each observation  can be printed by typing \code{index(meteo)}.
}
\details{
This data set stores all meteorological information useful for a GEOtop \url{www.geotop.org} simulation. 
The user can easily use the package with his/her own data after replacing the values of such variables.
}
\format{
Data frame , 'zoo' object
}
\source{
Original data are provided by Provincia Autonoma di Trento (\url{http://www.meteotrentino.it/})).

This dataset is intended for research purposes only, being distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY.
}

%%\references{
%% ~put references to the literature/web site here ~
%%}
\keyword{dataset}
