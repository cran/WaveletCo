\name{wc.image}
\alias{wc.image}
\title{Plot Wavelet Coherence Spectrum}
\description{
This function plot the wavelet coherence spectrum, used for generating designed figure}
\usage{wc.image(WC = WC, p = 2)}
\arguments{
  \item{WC}{Result of "wc"}
  \item{p}{Which region will be added arrows. p = 0: add arrows on the region that wavelet coherence power greater than 0.975; p = 1: add arrows on the region that wavelet coherence power show statistical significance; p = 2: add arrows on the region that both wavelet coherence power greater than 0.975 and show statistical significance}
}
\author{Huidong Tian}
