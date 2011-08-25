WaveletCoherency <- function(x, y, dt = 10, dj = 1/20, lowerPeriod = 2*dt,
                    upperPeriod = floor(length(x)*dt/3)){
# wavelet transform
  WT.x <- WaveletTransform(x, dt = dt, dj = dj, lowerPeriod = lowerPeriod,
          upperPeriod = upperPeriod);
  WT.y <- WaveletTransform(y, dt = dt, dj = dj, lowerPeriod = lowerPeriod,
          upperPeriod = upperPeriod);
  wav.x <- WT.x$Wave
  wav.y <- WT.y$Wave
  Wxx <- Re(wav.x * Conj(wav.x));
  Wyy <- Re(wav.y * Conj(wav.y));
  Wxy <- wav.x * Conj(wav.y);

# Smooth fitler
  nr <- WT.x$nr;
  nc <- WT.x$nc
  lF <- ceiling(nr/20);
  lT <- ceiling(nc/10);
  A <- WindowBartlett(lF) %*% t(WindowBartlett(lT))
  A <- A / sum(A)

# Smooth wavelet cross
  Wxx <- Re(conv2(Wxx, A))
  Wyy <- Re(conv2(Wyy, A))
  Wxy <- conv2(Wxy,A)
  WCo <- Mod(Wxy / sqrt(Wxx * Wyy));
  WCg <- rowMeans(WCo)

# output
  output <- list(WCo = WCo, Wxy = Wxy, WCg = WCg, Period = WT.x$Period,
            Scale = WT.x$Scale, nr = nr, nc = nc)
  return(invisible(output))
}
