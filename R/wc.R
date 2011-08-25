wc <- function(x, y, start = 0, dt = 10, dj=1/20, method = "white.noise",
      lowerPeriod = 2*dt, upperPeriod = floor(length(x)/3)*dt,
      no.bs = 100, plot = TRUE){

# wavelet coherence
  WC <- WaveletCoherency(x, y, dt = dt, dj = dj, lowerPeriod = lowerPeriod,
                         upperPeriod = upperPeriod)
  WCo <- WC$WCo
  WCg <- WC$WCg
  Wxy <- WC$Wxy
  nr  <- WC$nr
  nc  <- WC$nc
  Period <- WC$Period
  Angle <- Arg(Wxy)

  
# Significance level
  WCo.sig <- matrix(0, nrow = nr, ncol = nc)
  WCg.sig <- rep(0, nr)

  pb <- txtProgressBar(min = 0, max = no.bs, style = 3) # create a process bar
  for(bs in 1:no.bs){
    x.s <- SurrogateData(x, method = method)
    y.s <- SurrogateData(y, method = method)
      WC.s <- WaveletCoherency(x.s, y.s, dt = dt, dj = dj,
                     lowerPeriod = lowerPeriod, upperPeriod = upperPeriod);
      WCo.s <- WC.s$WCo
      WCg.s <- WC.s$WCg
      WCo.sig[which(WCo.s >= WCo)] <- WCo.sig[which(WCo.s >= WCo)] + 1;
      WCg.sig[which(WCg.s >= WCg)] <- WCg.sig[which(WCg.s >= WCg)] + 1
    setTxtProgressBar(pb, bs) # set process bar
  }
  close(pb) # close process bar

  WCo.sig <- WCo.sig/no.bs;
  WCg.sig <- WCg.sig/no.bs;

# COI
  coi <- COI(start = start, dt = dt, nc = nc, nr = nr, Period = Period)

# Plot
  if(plot){
    x11(5, 2, pointsize = 10)
    # plot wavelet coherency
    par(fig = c(0, .8, 0, 1), mar = c(2, 3, 1, 0))
    axis.1 <- coi$axis.1
    axis.2 <- coi$axis.2
    image(axis.1, axis.2, t(WCo^15), col = WaveletColors(),
          ylab = "", xlab = '', axes = FALSE)
    contour(axis.1, axis.2, t(WCo.sig) < 0.1, levels = 1, lwd = .5,
            add = TRUE, col = "white", drawlabels = FALSE)
    Angle <- Angle
    Angle[WCo.sig > 0.05] <- NA
    A.row <- seq(1, NROW(Angle), round(NROW(Angle)/30))
    A.col <- seq(1, NCOL(Angle), round(NCOL(Angle)/40))
    size  <- min(par()$pin[2]/30, par()$pin[1]/40)
    ratio <- par()$pin[2]/par()$pin[1]
    Angle[-A.row,] <- NA; Angle[,-A.col] <- NA;

    # plot arrows
      for(i in 1:NROW(Angle)){
        for(j in 1:NCOL(Angle)){
          if(!is.na(Angle[i,j])){
            x = axis.1[j]
            y = log2(Period)[i]
            arrow(x, y, l = size, w = .3*size, alpha = Angle[i,j])
          }
        }
      }
      polygon(coi$x, coi$y, border = NA, col = rgb(1,1,1,0.5))
      box(lwd = .25)

    # plot axis
    A.1 <- axis(1, lwd = .25, label = NA, tck = .025)
    A.2 <- axis(2, lwd = .25, label = NA, tck = .025)
    mtext(A.1, side = 1, at = A.1, cex = .8, line = -.2)
    mtext(2^A.2, side = 2, at = A.2, cex = .8, las = 1, line = .2)
    mtext('Period', side = 2, line = 1.75, cex = .8)
    mtext('X-label', side = 1, line = .75, cex = .8)

    # plot global power
    par(fig = c(0.8, 1, 0, 1), mar = c(2, 0, 1, 2), new = TRUE)

    plot(WCg, log2(Period), lwd = 0.5, type = "l", axes = FALSE, ylab = "", xlab = '', yaxs = 'i')
    P.dat <- data.frame(Pvalue = WCg.sig, Log.period = log2(Period), Power = WCg)
    with(P.dat[P.dat$Pvalue < 0.10,], points(Power, Log.period, pch = 20, col = "blue"))
    with(P.dat[P.dat$Pvalue < 0.05,], points(Power, Log.period, pch = 20, col = "red"))

    A.1 <- axis(1, lwd = .25, label = NA, tck = .025)
    A.2 <- axis(2, lwd = .25, label = NA, tck = .025)
    A.2 <- axis(4, lwd = .25, label = NA, tck = .025)
    mtext(A.1[-1], side = 1, at = A.1[-1], cex = .8, line = -.2)
    mtext(2^A.2, side = 4, at = A.2, cex = .8, las = 1, line = .2)
    mtext('Global Power', side = 1, line = .75, cex = .8)
    box(lwd = .25)
  }

# output
  output <- list(WCo = WCo, Wxy = Wxy, WCg = WCg, WCo.sig = WCo.sig,
            WCg.sig = WCg.sig, Period = Period, Angle = Angle, nc = nc, nr = nr,
            coi.x = coi$x, coi.y = coi$y, axis.1 = coi$axis.1, axis.2 = coi$axis.2)
  return(invisible(output))
}
