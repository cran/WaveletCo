wc.image = function(WC = WC,p = 2){
  axis.1 <- WC$axis.1
  axis.2 <- WC$axis.2 
  image(axis.1, axis.2, t(WC$WCo^15), col = WaveletColors(),
        ylab = "", xlab = '', axes = FALSE)
  contour(axis.1, axis.2, t(WC$WCo.sig) < 0.1, levels = 1, lwd = .5,
          add = TRUE, col = "white", drawlabels = FALSE)
  wc.angle(WC, p = p)	
  polygon(WC$coi.x, WC$coi.y, border = NA, col = rgb(1, 1, 1, 0.5))
  box(lwd = .25)		
}
