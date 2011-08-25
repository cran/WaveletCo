wc.power <- function(WC = WC){
  plot(WC$WCg, log2(WC$Period), lwd = 0.5, type = "l", axes = FALSE, ylab = "", xlab = '', yaxs = 'i')
  P.dat <- data.frame(Pvalue = WC$WCg.sig, Log.period = log2(WC$Period), Power = WC$WCg)
  with(P.dat[P.dat$Pvalue < 0.10,], points(Power, Log.period, pch = 20, col = "blue"))
  with(P.dat[P.dat$Pvalue < 0.05,], points(Power, Log.period, pch = 20, col = "red"))
  box(lwd = 0.25)
}