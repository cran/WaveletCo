\name{wt}
\alias{wt}

\title{Wavelet Periodicity Analysis}
\description{The main function conducting wavelet periodicity analysis}
\usage{wt(x, start = 0, dt = 10, dj = 1/20, method = "white.noise", lowerPeriod = 2 * dt, upperPeriod = floor(length(x) * dt/3), no.bs = 100, plot = TRUE)}

\arguments{
  \item{x}{Time series being analyzed}
  \item{start}{The start time point, e.g. year}
  \item{dt}{Sampling resolution on temporal field}
  \item{dj}{Sampling resolution on frequency field}
  \item{method}{the method of generating surrogate time series}
  \item{lowerPeriod}{Lower period of wavelet decomposition}
  \item{upperPeriod}{upper period of wavelet decomposition}
  \item{no.bs}{number of bootstrap}
  \item{plot}{If ture, the plot the result}
}

\value{This function return the wavelet decomposition of time series x, and its globle spectrum}

\author{Huidong Tian, Bernard Cazelles}

\examples{

x <- c(1.50* sin(seq(0, 30*pi, length.out = 70)) + 
     rnorm(70, sd = .25), rnorm(30, sd = 1.25)) + 
     sin(seq(0, 10*pi, length.out = 100))*2 
ns <- 200; 
WT <- wt(x, no.bs = ns, dt= 1, start = 1909, plot = FALSE)

##############################


f.n =  2      
f.h =  25/25.4 
m.t =  0.5;
m.b =  1.5     
m.1 =  1;
m.2 =  3;
m.3 =  1;
m.4 =  2   

W = 120/25.4
H = f.n*(f.h + (m.1+m.3)*4/30)+ (m.t + m.b)*4/30

tick0 = 0.025; 
tick1 = tick0*f.h/(.15*W-m.4*4/30)

fig.h = (f.h + (m.1+m.3)*4/30)/H
fig.b = m.b*4/30/H

fig.w = list(); for(i in 1:f.n) fig.w[[i]] = c( 0,.8, fig.b + (f.n-i)*fig.h,fig.b + (f.n+1-i)*fig.h)
fig.p = list(); for(i in 1:f.n) fig.p[[i]] = c(.8,1, fig.b + (f.n-i)*fig.h,fig.b + (f.n+1-i)*fig.h)
#############################################
x11(W,H,pointsize = 8)

## plot #1
  par(fig = fig.w[[1]],mar = c(m.1,m.2,m.3,0))
  plot (seq(1909, length.out= 100), x, type = "o", pch = 19, axes = FALSE, xaxs = "i")
  abline(lm(x~seq(1909, length.out= 100)), lwd = 2)
  box(lwd = .25)
  axis(1,lwd = .25,at = seq(1920, 2000, 20),label = NA,tck = tick0)
  axis(2,lwd = .25,at = seq(-6, 6, 3),label = NA,tck = tick0)
  mtext(seq(1920, 2000, 20),side = 1,at = seq(1920, 2000, 20),cex = 7/8)
  mtext(seq(-6, 6, 3),side = 2,at = seq(-6, 6, 3),cex = 7/8,las = 1,line = .1)
  mtext('(a)',side = 3,at = 1900,line = 0.1,font = 2,adj = 0)
  mtext('Time series',side = 2,line = 1.5,font = 1)

## plot #2
  par(fig = fig.w[[2]],mar = c(m.1,m.2,m.3,0), new = TRUE)
  wt.image(WT)
  axis(1,lwd = .25,at = seq(1920, 2000, 20),label = NA,tck = tick0)
  axis(2,lwd = .25,at = seq(1, 5),label = NA,tck = tick0)
  mtext(seq(1920, 2000, 20),side = 1,at = seq(1920, 2000, 20),cex = 7/8)
  mtext(2^seq(1, 5),side = 2,at = seq(1, 5),cex = 7/8,las = 1,line = .1)
  mtext('(b)',side = 3,at = 1900,line = 0.1,font = 2,adj = 0)
  mtext('Period',side = 2,line = 1.5,font = 1)
  mtext('Year',side = 1,line = 1)

  par(fig = fig.p[[2]],mar = c(m.1,0,m.3,m.4),new = TRUE)
  wt.power(WT)
  axis(1,lwd = .25,at = seq(0, 10, 5),label = NA,tck = tick0)
  axis(4,lwd = .25,at = seq(1, 5),label = NA,tck = tick0)
  mtext(seq(0, 10, 5),side = 1,at = seq(0, 10, 5),cex = 7/8)
  mtext(2^seq(1, 5),side = 4,at = seq(1, 5),cex = 7/8,las = 1,line = .1)
  mtext('Power',side = 1,line = 1)
}
