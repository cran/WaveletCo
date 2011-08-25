\name{Ridge}
\alias{Ridge}
\title{Wavelet Power Spectrum Ridge}
\description{This function calculates the ridge of wavelet power spectrum}
\usage{Ridge(power, band = 5, scl = 0.1)}

\arguments{
  \item{power}{Wavelet power spectrum}
  \item{band}{Number of points used to compute a maximum, in fact 2*nb+1 points are used}
  \item{scl}{scale factor, only the value of wavelet power spectrum greater than "scl*power" will have the possibility to be selected as ridge.}
}

\author{Huidong Tian, Bernard Cazelles}
