wt <- function(x, start = 0, dt = 10, dj = 1/20, method = "white.noise",
      lowerPeriod = 2*dt, upperPeriod = floor(length(x)*dt/3),
      no.bs = 100, plot = TRUE){
      
# Wavelet Transform
  WT <- WaveletTransform(x, dt = dt, dj = dj,
                        lowerPeriod = lowerPeriod, upperPeriod = upperPeriod)
  Power <- WT$Power
  P.avg <- WT$P.avg
  Scale <- WT$Scale
  Period <- WT$Period
  nr <- WT$nr
  nc <- WT$nc

# Significance level test;
  Power.sig <- matrix(0, nrow = nr, ncol = nc)
  P.avg.sig <- rep(0, nr)
  pb <- txtProgressBar(min = 0, max = no.bs, style = 3) # create a process bar
  for(bs in 1:no.bs){
    x.s <- SurrogateData(x, method = method)
    WT.s <- WaveletTransform(x.s, dt = dt, dj = dj,
                             lowerPeriod = lowerPeriod, upperPeriod = upperPeriod)
    Power.s <- WT.s$Power
    P.avg.s <- WT.s$P.avg
    Power.sig[Power.s >= Power] <- Power.sig[Power.s >= Power] + 1
    P.avg.sig[P.avg.s >= P.avg] <- P.avg.sig[P.avg.s >= P.avg] + 1
    setTxtProgressBar(pb, bs) # set process bar
  }
  close(pb) # close process bar
  Power.sig <- Power.sig / no.bs  # siginificance level, e.g. < 0.05
  P.avg.sig <- P.avg.sig / no.bs  # siginificance level, e.g. < 0.05

# Power ridge
  rid <- Ridge(Power)

# COI
  coi <- COI(start = start, dt = dt, nc = nc, nr = nr, Period = Period)

# Plot
  if(plot){
    x11(5, 2, pointsize = 10)
    # plot wavelet coherency
    par(fig = c(0, .8, 0, 1), mar = c(2, 3, 1, 0))
    axis.1 <- coi$axis.1
    axis.2 <- coi$axis.2
    image(axis.1, axis.2, t(Power^.5), col = WaveletColors(),
          ylab = "", xlab = '', axes = FALSE)
    contour(axis.1, axis.2, t(rid), levels = 1, lwd = .5,
          add = TRUE, col = "white", drawlabels = FALSE)
    contour(axis.1, axis.2, t(Power.sig) < 0.1, levels = 1, lwd = .5,
            add = TRUE, col = "white", drawlabels = FALSE)
    polygon(coi$x, coi$y, border = NA, col = rgb(1,1,1,0.5))
    box(lwd = .25)

    A.1 <- axis(1, lwd = .25, label = NA, tck = .025)
    A.2 <- axis(2, lwd = .25, label = NA, tck = .025)
    mtext(A.1, side = 1, at = A.1, cex = .8, line = -.2)
    mtext(2^A.2, side = 2, at = A.2, cex = .8, las = 1, line = .2)
    mtext('Period', side = 2, line = 1.75, cex = .8)
    mtext('X-label', side = 1, line = .75, cex = .8)

    # plot global power
    par(fig = c(0.8, 1, 0, 1), mar = c(2, 0, 1, 2), new = TRUE)

    plot(P.avg, log2(Period), lwd = 0.5, type = "l", axes = FALSE, ylab = "", xlab = '', yaxs = 'i')
    P.dat <- data.frame(Pvalue = P.avg.sig, Log.period = log2(Period), Power = P.avg)
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
  output <- list(Wave = WT$Wave, Power = Power, P.avg = P.avg,
                Power.sig = Power.sig, P.avg.sig = P.avg.sig, Period = Period,
                Scale = Scale, nr = nr, nc = nc, rid = rid, axis.1 = coi$axis.1,
                axis.2 = coi$axis.2, coi.x = coi$x, coi.y = coi$y)
  return(invisible(output))
}



