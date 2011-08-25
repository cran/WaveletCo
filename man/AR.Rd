\name{AR}
\alias{AR}
\title{Auto Regression Simulation}
\description{Generate surrogate time series with similar auto-regression feature as analyzed time series.}
\usage{AR(x, order = 1)}
\arguments{
  \item{x}{Time series to be analyzed}
  \item{order}{The order of auto correlation, default is 1}
}
\value{Return a time series with similar auto-regression feature as time series "x".}
\author{Huidong Tian}
\seealso{\code{\link{acf}, \link{pacf}}}
\note{If the time series being analyzed has no significant auto-regreesion feature, don't use this method.}


