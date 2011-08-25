\name{arrow}
\alias{arrow}
\title{Polygon Arrow}
\description{Draw arrow base on function "polygon"}
\usage{arrow(x, y, l = 0.1, w = 0.3 * l, alpha)}

\arguments{
  \item{x}{The x coordinate of the center of arrow.}
  \item{y}{The y coordinate of the center of arrow.}
  \item{l}{The length of arrow.}
  \item{w}{The width of arrow.}
  \item{alpha}{The direction of the arrow, range from 0 to 2*pi.}
}
\author{Huidong Tian}
\note{Change R's plot window will induce error of arrows' feature. So do not re-size the window dimenssion after plotting.}
\seealso{\code{\link{arrows}}}


