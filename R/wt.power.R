wt.power <- function(WT = WT){
  plot(WT$P.avg, log2(WT$Period), lwd = 0.5, type = "l", axes = FALSE, ylab = "", xlab = '', yaxs = 'i')
  P.dat <- data.frame(Pvalue = WT$P.avg.sig, Log.period = log2(WT$Period), Power = WT$P.avg)
  with(P.dat[P.dat$Pvalue < 0.10,], points(Power, Log.period, pch = 20, col = "blue"))
  with(P.dat[P.dat$Pvalue < 0.05,], points(Power, Log.period, pch = 20, col = "red"))
  box(lwd = .25)
}
