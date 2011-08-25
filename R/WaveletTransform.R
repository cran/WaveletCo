WaveletTransform <- function(x, dt = dt, dj = dj, lowerPeriod = lowerPeriod,
                             upperPeriod = upperPeriod){
# All time series will be standardised before entering wavelet decomposition.
  x <- (x-mean(x))/sd(x)
  
# Basis parameters
  ff <- 1.033044	# Fourier factor;
  n <- length(x)
  x0 <- c(x, rep(0, 2^ceiling(log2(n)) - n))
  n0 <- length(x0)

  period.ls <- 2^seq(log2(lowerPeriod), log2(upperPeriod), dj)
  scale.ls <- period.ls/ff
  nr <- length(scale.ls)
  k <- 1:floor(n0/2)
  k <- k*(2*pi)/(n*dt)
  k <- c(0, k, -k[rev(1:floor((n0-1)/2))])

# Wavelet decomposition
  f <- fft(x0)  # length(f) == n0
  Wave <- matrix(NA ,nrow = nr, ncol = n0);
  for(i in 1:nr){
    scl <- scale.ls[i];
    expnt <- -(scl*k - 6)^2/2*(k > 0);
    norm <- sqrt(scl*k[2])*(pi^(-0.25))*sqrt(n0);
    daughter <- norm*exp(expnt);
    daughter <- daughter*(k > 0)
    Wave[i,] <- fft(f*daughter,inverse = TRUE)/n0;
  }
  Wave <- Wave[, 1:n];
  Power <- Mod(Wave)^2
  P.avg <- rowMeans(Power)

# output
  output <- list(Wave = Wave, Period = period.ls, Scale = scale.ls,
                 Power = Power, P.avg = P.avg, nr = nr, nc = n)
  return(invisible(output))
}
