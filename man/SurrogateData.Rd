\name{SurrogateData}
\alias{SurrogateData}
\title{Generate Surrogate Time Series}
\description{This function gerenates a surrogate time series using several methods, such as "white.noise", "Fourier randomization", "auto-regression", etc. More methods will be available in future.}
\usage{SurrogateData(x, method = "white.noise")}
\arguments{
  \item{x}{The time series being analyzed}
  \item{method}{The method of which kind of surrogate time series will be generated. At current, there are four kinds of methods available: "white.noise" generates the normal distributed time series; "shuffle" generates surrogate time series by shuffling "x"; "Fourier.rand" generates surrogate time sereis with a similar spectrum as "x"; "AR" generates surrogate time series with the similar auto-regression feature as "x".}
}
\author{Huidong Tian, Bernard Cazelles}
\seealso{\code{\link{AR}, \link{FourierRand}}}

