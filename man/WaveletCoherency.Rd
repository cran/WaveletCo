\name{WaveletCoherency}
\alias{WaveletCoherency}
\title{Wavelet Coherence}
\description{This function conducts wavelet coherence of two time series}
\usage{WaveletCoherency(x, y, dt = 10, dj = 1/20, lowerPeriod = 2 * dt, upperPeriod = floor(length(x) * dt/3))}
\arguments{
  \item{x}{Time series x}
  \item{y}{Time series y}
  \item{dt}{Sampling resolution on temporal field}
  \item{dj}{Sampling resolution on frequency field}
  \item{lowerPeriod}{Lower period of wavelet decomposition}
  \item{upperPeriod}{upper period of wavelet decomposition}
}
\value{
  \item{WCo}{Wavelet coherence matrix}
  \item{Wxy}{Smoothed Cross Wavelet Spectrum}
  \item{WCg}{Average wavelet coherency}
  \item{Period}{Vector of period obtained based on the vector "scale"}
  \item{Scale}{Vector of the wavelet scale employed during the computation}
  \item{nr}{The row number of the "WCo"}
  \item{nc}{The colomn number of the "WCo"}
}
\references{
  Cazelles, B., M. Chavez, et al. (2008). "Wavelet analysis of ecological time series." Oecologia 156(2): 287-304.

  Torrence, C. and G. Compo (1998). "A practical guide to wavelet analysis." B Am Meteorol Soc 79(1): 61-78.
}
\author{Bernard Cazelles, Huidong Tian}
\seealso{\code{\link{WaveletTransform}}}
