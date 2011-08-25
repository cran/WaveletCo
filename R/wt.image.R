wt.image <- function(WT = WT){
  axis.1 <- WT$axis.1
  axis.2 <- WT$axis.2
  image(axis.1, axis.2, t(WT$Power^.5), col = WaveletColors(),
        ylab = "", xlab = "", axes = FALSE)
  contour(axis.1, axis.2, t(WT$rid), levels = 1, lwd = .5,
          add = TRUE, col = "white", drawlabels = FALSE)
  contour(axis.1, axis.2, t(WT$Power.sig) < 0.05, levels = 1, lwd = .25,
          add = TRUE, col = "black", drawlabels = FALSE)        
  polygon(WT$coi.x, WT$coi.y, border = NA,col = rgb(1,1,1,0.5))	
  box(lwd = .25)		
}
