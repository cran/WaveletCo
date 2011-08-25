\name{wc}
\alias{wc}
\title{Wavelet Coherence Analysis}
\description{The main function of this package; conducts the wavelet coherence analysis}
\usage{wc(x, y, start = 0, dt = 10, dj = 1/20, method = "white.noise", lowerPeriod = 2 * dt, upperPeriod = floor(length(x)/3) * dt, no.bs = 100, plot = TRUE)}

\arguments{
  \item{x}{time series x}
  \item{y}{time series y}
  \item{start}{The start point of time axis}
  \item{dt}{Sampling resolution on temporal field}
  \item{dj}{Sampling resolution on frequency field}
  \item{method}{the method of generating surrogate time series}
  \item{lowerPeriod}{Lower period of wavelet decomposition}
  \item{upperPeriod}{upper period of wavelet decomposition}
  \item{no.bs}{the number of bootstrap}
  \item{plot}{if TRUE, then plot the result}
}


\author{Huidong Tian, Bernard Cazelles}

\examples{

x <- c(1.50* sin(seq(0, 30*pi, length.out = 70)) + 
     rnorm(70, sd = .25), rnorm(30, sd = 1.25)) + 
     sin(seq(0, 10*pi, length.out = 100))*2 +
     seq(4, -4, length.out = 100)

y <- c(1.25* sin(seq(0, 30*pi, length.out = 70)+7*pi/8) + 
     rnorm(70, sd = .25), rnorm(30, sd = 1.25) ) + 
     sin(seq(0, 10*pi, length.out = 100))*2 +
     seq(-4, 4, length.out = 100)

ns <- 200
WC <- wc(x, y, no.bs = ns, dt= 1, start = 1909, plot = FALSE)

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
  plot (seq(1909, length.out= 100), x, type = "o", ylim = c(min(x, y), max(x, y)), pch = 19, axes = FALSE, xaxs = "i")
  abline(lm(x~seq(1909, length.out= 100)), lwd = 2)
  lines(seq(1909, length.out= 100), y, type = "o", pch = 21, col = "red")
  abline(lm(y~seq(1909, length.out= 100)), lwd = 2, col = "red")
  box(lwd = .25)
  axis(1,lwd = .25,at = seq(1920, 2000, 20),label = NA,tck = tick0)
  axis(2,lwd = .25,at = seq(-3, 3, 3),label = NA,tck = tick0)
  mtext(seq(1920, 2000, 20),side = 1,at = seq(1920, 2000, 20),cex = 7/8)
  mtext(seq(-3, 3, 3),side = 2,at = seq(-3, 3, 3),cex = 7/8,las = 1,line = .1)
  mtext('(a)',side = 3,at = 1900,line = 0.1,font = 2,adj = 0)
  mtext('Time series',side = 2,line = 1.5,font = 1)

## plot #2
  par(fig = fig.w[[2]],mar = c(m.1,m.2,m.3,0), new = TRUE)
  wc.image(WC)
  axis(1,lwd = .25,at = seq(1920, 2000, 20),label = NA,tck = tick0)
  axis(2,lwd = .25,at = seq(1, 5),label = NA,tck = tick0)
  mtext(seq(1920, 2000, 20),side = 1,at = seq(1920, 2000, 20),cex = 7/8)
  mtext(2^seq(1, 5),side = 2,at = seq(1, 5),cex = 7/8,las = 1,line = .1)
  mtext('(b)',side = 3,at = 1900,line = 0.1,font = 2,adj = 0)
  mtext('Period',side = 2,line = 1.5,font = 1)
  mtext('Year',side = 1,line = 1)
  
  par(fig = fig.p[[2]],mar = c(m.1,0,m.3,m.4),new = TRUE)
  wc.power(WC)
  axis(1,lwd = .25,at = seq(.7, 1, .1),label = NA,tck = tick0)
  axis(4,lwd = .25,at = seq(1, 5),label = NA,tck = tick0)
  mtext(seq(.7, 1, .1),side = 1,at = seq(.7, 1, .1),cex = 7/8)
  mtext(2^seq(1, 5),side = 4,at = seq(1, 5),cex = 7/8,las = 1,line = .1)
  mtext('Power',side = 1,line = 1)

}

